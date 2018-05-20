#########################################################################
######  Multinomial External Regression                              ####
#########################################################################

# The idea in this file is to compute regressions as Janine sugested:
# Put overall score as response variable
# Combine suitability and proposal as one. 
# Then use that and aplicant track record as two predictors to predict the overall score. 
# Make two logisitic regresion models, one using track, and one with proposal. 
# Just do this to compare if gender makes a different impact in the two models. 
# Include interaction between gender of applicant and gender of reviewer
# See whether gender + applicant or gender + proposal have a bigger impact on the OverallGrade



# Initialize functions and get External Regression Data ------------------

load("SNFS Data/snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

rm(applications,reviews,referee_grades, test)

library(MASS)
library(grid)
library(vcd)
library(corrplot)
library(car)
library(caTools)
library(caret)

# Get the Regression data
  external_regression_data<-prepare_data_external_log_regression(final.apps,final.external)

# Average the grade for ScientificRelevance ans Suitability
 
  #response must be an order factor for odrinal Regression
  
  external_regression_data$ProposalCombined <- as.ordered(external_regression_data$ProposalCombined)
  external_regression_data$ApplicantTrack <- as.ordered(external_regression_data$ApplicantTrack)
  external_regression_data$OverallGrade <- as.ordered(external_regression_data$OverallGrade)
  str(external_regression_data)


# Fitting the Model for the project evaluation-------------------------------------------------------
## Run an Ordinal Logistic Regression
 
   # Visualization
    ggplot(external_regression_data, aes(x = ProposalCombined, y = PercentFemale, col=Gender)) +
      geom_jitter(alpha = .5) +
      facet_grid(InstType ~ Division, margins = TRUE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# We start fitting a small model and building it up to se that the polr() works well
  fit <- polr(ProposalCombined ~ Gender + Division + InstType, 
              method="logistic",data=external_regression_data)
  summary(fit)
  
  # Add another variable and make model comparison
    fit2 <- polr(ProposalCombined ~ Gender + Division + InstType + IsContinuation, 
                 method="logistic",data=external_regression_data)
    summary(fit2)
  
    deviance(fit)-deviance((fit2))
    pchisq(40.60535, fit2$edf-fit$edf, lower=FALSE)
    # [1] 1.862959e-10 -> include IsContinuation
  
  # Same thing again ans again
    fit3 <- polr(ProposalCombined ~ Gender + Division + InstType + IsContinuation + 
               Age, method="logistic",data=external_regression_data)
    summary(fit3)
    deviance(fit2)-deviance((fit3))
    pchisq(1.352098, fit3$edf-fit2$edf, lower=FALSE)
    # [1] 0.2449117 -> don't include Age
    
    fit4 <- polr(ProposalCombined ~ Gender + Division + InstType + IsContinuation + 
                   log(AmountRequested), method="logistic",data=external_regression_data)
    summary(fit4)
    
    deviance(fit2)-deviance((fit4))
    pchisq(38.31625, fit4$edf-fit2$edf, lower=FALSE)
    # [1] 6.015959e-10 -> include log(AmountRequested)

    fit5 <- polr(ProposalCombined ~ Gender + Division + InstType + IsContinuation + 
               log(AmountRequested)+PercentFemale, method="logistic",data=external_regression_data)
    summary(fit5)

    deviance(fit4)-deviance((fit5))
    pchisq(3.474756, fit5$edf-fit4$edf, lower=FALSE)
    # [1] 0.06231191 -> include PercentFemale
    
    AIC(fit5)
    AIC(fit4)

    fit6 <- polr(ProposalCombined ~ Gender + Division + InstType + IsContinuation + 
                   log(AmountRequested)+PercentFemale+Gender:Division, 
                 method="logistic",data=external_regression_data)
    summary(fit6)
    deviance(fit5)-deviance((fit6))
    pchisq(0.1299571, fit6$edf-fit5$edf, lower=FALSE)
    # [1] 0.9370876 -> include Gender:Division, although not significant, we want to
                      #show the client that this is so

    fit7 <- polr(ProposalCombined ~ Gender + Division + InstType + IsContinuation + 
                   log(AmountRequested)+PercentFemale+Gender:Division+Gender:PercentFemale, 
                 method="logistic",data=external_regression_data)
    summary(fit7)
    deviance(fit6)-deviance((fit7))
    pchisq(4.800679, fit7$edf-fit6$edf, lower=FALSE)
    # [1] 0.02844852 -> include Gender:PercentFemale
    
    # We chose this final model. Next we are going to test it
    PrAssModel<-fit7
  

# Model Diagnostic --------------------------------------------------------

    # Uses bootstrap to estimate the out of sample miss classification error
    # instead of only one split.
    # M: number of bootstraps
    ModelDiagnostic<- function(TestModel=PrAssModel,
                               outcomeName = 'ProposalCombined',
                               data=external_regression_data,Div="All",M=30) { 
      
      # Model formula and final data
        
        if (Div == "All"){
          formula=TestModel$call[["formula"]]
          final.data<- data
        } else {
          formula=update.formula(TestModel,.~.-Division-Gender:Division)
          final.data<- subset(data,Division==Div, select = -(Division)) 
        }
      
      # Fitting the model
      
        # Fit the model in all data
          Model <- polr(formula, 
                        method="logistic",
                        data=final.data,
                        Hess = TRUE)
          
        # Uses Bootstraps for the out of sample Miss classification Error
      
          MCE<-c()
          ACC<-c()
          set.seed(77)
          for ( i in 1:M){
            
              # Resample the data with replacement and obtained In Bag indices
                ind<-createResample(final.data[,outcomeName],1)[[1]]
                InBag <- unique(ind)
                
              # Split the data
                Train<-final.data[ind,]
                Test<- final.data[-InBag,]
          
              # Fit the model in the Train data
                B.Model <- polr(formula, 
                                method="logistic",
                                data=Train,
                                Hess = TRUE)
                #B.Model <- update(Model,.~.,data=Train)
                
              # Confusion Matrix Out of Bag 
                pred<-predict(B.Model,Test) # class prediction
                tab<- table(pred,Test[,outcomeName])
                
              # Miss Clasification Error  MCE
                MCE[i] <- 1- sum(diag(tab))/sum(tab)
                
              # Out of Sample Accuracy
                ACC[i] <- sum(diag(tab))/sum(tab)
          } 
           
         
          # p value calculation
                ctable<-coef(summary(Model))
                p <- pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
                ctable<-cbind(ctable,"p value"=p) 
                
          # Probalilities
            prob<-exp(cbind(OR=coef(Model), confint(Model)))
      
          # Confusion Matrices 
            pred<-predict(Model,data) # class prediction
            tab<- table(pred,data[,outcomeName])
            
          # Miss Clasification Error  MCE
            mce <- 1- sum(diag(tab))/sum(tab)
          
          # Average out of sample MCE
            BOOTMCE<- mean(MCE)
          
          # Average Accuracy
            AveAcc<- mean(ACC)
            
            
          # Kappa Statistic
            # O: observed accuracy
            # E: expected accuracy under chance agreement
            # k: (O-E)/(1-E)
            # Kappa()
            
      # Return
        res<-list(Model=formula,
                    Summary= summary(Model),
                   `Probabilities and Confident intervals`= prob,
                   `Confusion Matrix`= tab,
                   `Average out of sample MCE`=  paste(round(BOOTMCE,2)*100,"%"),
                   `Average Accuracy`=  paste(round(AveAcc,2)*100,"%"),    
                   `Miss Classification Error`= paste(round(mce,2)*100,"%"),
                    Accuracy=paste(round(1-mce,2)*100,"%"))
        hist(MCE,col="springgreen2", 
             main= "Out of Sample Missclassification Error",
             cex.main=0.8, sub=paste("Number of iterations ",M),cex.sub=0.8)
        hist(ACC,col="springgreen2", 
             main= "Out of Sample Accuracy ",
             cex.main=0.8, sub=paste("Number of iterations ",M),cex.sub=0.8)
        
        return(res)
      
          }
    
  ProposalCombinedfit<-ModelDiagnostic(TestModel = PrAssModel,outcomeName = 'ProposalCombined') 
  # Here the polr worked well, no errors
  ProposalCombinedfit
  # $`Average out of sample MCE`
  # [1] "46 %"
  # $`Average Accuracy`
  # [1] "54 %"
  # $`Miss Classification Error`
  # [1] "46 %"
  # $Accuracy
  # [1] "54 %"
  
##*****************************************************************##
  
  
  
# Fitting the Model for the Applicant Track -------------------------------
  
  # Visualization
    ggplot(external_regression_data, aes(x = ApplicantTrack, y = PercentFemale, col=Gender)) +
      geom_jitter(alpha = .5) +
      facet_grid(InstType ~ Division, margins = TRUE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  # Choose the model
      #response must be a factor
      
      fit <- polr(ApplicantTrack ~ Gender + Division + InstType, 
                  method="logistic",data=external_regression_data)
      summary(fit)
      
      fit2 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation, 
                   method="logistic",data=external_regression_data)
      summary(fit2)
      
      deviance(fit)-deviance((fit2))
      pchisq(29.22203, fit2$edf-fit$edf, lower=FALSE)
      # [1] 6.45412e-08 <- include IsContinuation
      
      fit3 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
                     Age, method="logistic",data=external_regression_data)
      summary(fit3)
      deviance(fit2)-deviance((fit3))
      pchisq(2.030959, fit3$edf-fit2$edf, lower=FALSE)
      # [1] 0.1541233-> don't include Age
      
      fit4 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
                     log(AmountRequested), method="logistic",data=external_regression_data)
      summary(fit4)
      
      deviance(fit2)-deviance((fit4))
      pchisq(41.22323, fit4$edf-fit2$edf, lower=FALSE)
      # [1] 1.35799e-10-> include log(AmountRequested)
      
      fit5 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
                     log(AmountRequested)+PercentFemale, method="logistic",data=external_regression_data)
      summary(fit5)
      
      deviance(fit4)-deviance((fit5))
      pchisq(5.829177, fit5$edf-fit4$edf, lower=FALSE)
      # [1] 0.0157625 -> include PercentFemale
      
      fit6 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
                     log(AmountRequested)+PercentFemale+Gender:Division, 
                   method="logistic",data=external_regression_data)
      summary(fit6)
      deviance(fit5)-deviance((fit6))
      pchisq(1.944121, fit6$edf-fit5$edf, lower=FALSE)
      # [1] 0.3783027 -> not significant but we need to include Gender:Division 
      
      fit7 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
                     log(AmountRequested)+PercentFemale+Gender:Division+Gender:PercentFemale, 
                   method="logistic",data=external_regression_data)
      summary(fit7)
      deviance(fit6)-deviance((fit7))
      pchisq(4.93612, fit7$edf-fit6$edf, lower=FALSE)
      # [1] 0.176535 -> don't include Gender:PercentFemale
      
      fit8<-polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
                   log(AmountRequested)+PercentFemale+Gender:Division+PreviousRequest, 
                 method="logistic",data=external_regression_data)
      summary(fit8)
      deviance(fit6)-deviance((fit8))
      pchisq(0.02199118, fit7$edf-fit5$edf, lower=FALSE)
      # 1] 0.9991384 -> don't include PreviousRequest
      
      #best Model is then fit6
      
      AppTrModel<-fit6
      
      
# Model Diagnostic Applicant Track ----------------------------------------

      ApplicantTrackfit<-ModelDiagnostic(TestModel = AppTrModel,outcomeName = 'ApplicantTrack',M=30)  
      ApplicantTrackfit
      
      # $`Average out of sample MCE`
      # [1] "55 %"
      # 
      # $`Average Accuracy`
      # [1] "45 %"
      # 
      # $`Miss Classification Error`
      # [1] "54 %"
      # 
      # $Accuracy
      # [1] "46 %"
      
      

# Model Diagnostic OverallGrade ~ ApplTrack + others ------------------------------------------
      
      # Visualization
      ggplot(external_regression_data, aes(x = OverallGrade, y = ApplicantTrack, col=Gender)) +
        geom_jitter(alpha = .5) +
        facet_grid(InstType ~ Division, margins = TRUE) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
      
      # Rearrenge Data
      OvGrApplTrackDF<- external_regression_data[,c(2,5,6,8:14)]
      OvGrApplTrackDF$AmountRequested<-log(OvGrApplTrackDF$AmountRequested)
      
      (with(OvGrApplTrackDF, table(OverallGrade,ApplicantTrack)))
      
      # Fit a model with all variables and interaction
      OvGrAppMod<- polr(OverallGrade~.+Gender:Division+Gender:PercentFemale, 
                        data=OvGrApplTrackDF,
                        method="logistic",
                        Hess=T) 
      summary(OvGrAppMod)
      
      # Correlation Plot
      OvGrAppDummy <- dummyVars("~.-OverallGrade",data=OvGrApplTrackDF, fullRank=F)
      X <- as.data.frame(predict(OvGrAppDummy,OvGrApplTrackDF))
      X<-cor(X)
      corrplot(X,method = "color",type="upper",
               title="Model: OverallGrade~ApplicantTrack ..")
      
      
      # p value calculation with a normal approximation (we have enough data to make this assumption)
      ctable<-coef(summary(OvGrAppMod))
      p <- pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
      (ctable<-cbind(ctable,"p value"=p)) 
      
      # Confidence intervals Profile or Wald?
      # With profile likelihood
      PCI <- confint(OvGrAppMod)
      # Wald CI
      WCI <- confint.default(OvGrAppMod)
      # Puting them together
      CI<- cbind(Profile=PCI,Wald=WCI, `lenght PCI`= PCI[,2]-PCI[,1],
                 `lenght WCI`= WCI[,2]-WCI[,1] )
      Diff<- round(CI[,5]-CI[,6],4)
      CI <- cbind(CI,Diff)
      
      # As the difference between PCI and WCI is not big, due to the fact that between variables
      # Te correlation is small (only ApplicantTrack is more correlated to OverallGrade)
      
      # Odds Ratios an Confidence Intervals
      (prob<-exp(cbind(OR=coef(OvGrAppMod), WCI)))
      
      # Confusion Matrices 
      pred<-predict(OvGrAppMod,OvGrApplTrackDF) # class prediction
      (tab<- table(Prediction=pred,
                   Observed=OvGrApplTrackDF$OverallGrade))
      
      
      # Miss Clasification Error  MCE
      (mce <- 1- sum(diag(tab))/sum(tab)) #[1]  0.4152803
      
      # Test the Out of Sample Miss classification error
      
      # Split the data
      set.seed(77)
      split<-sample.split(OvGrApplTrackDF$OverallGrade, SplitRatio = 0.8)
      Train<-subset(OvGrApplTrackDF, split=="TRUE")
      Test <-subset(OvGrApplTrackDF, split=="FALSE")
      
      # Fit the model in the Train data
      B.Model <- update(OvGrAppMod,.~.,data=Train)
      
      # Confusion Matrix Out of Bag 
      pred<-predict(B.Model,Test) # class prediction
      tab<- table(pred,Test[,'OverallGrade'])
      
      # Out of Sample Miss Clasification Error  OOSMCE
      (OOSMCE <- 1- sum(diag(tab))/sum(tab)) #[1] 0.4104938
      
      # Out of Sample Accuracy OOSACC
      (OOSACC <- sum(diag(tab))/sum(tab)) # [1] 0.5895062
      
# Model Diagnostic OverallGrade ~ ProposalCombined + others -----------------------------------
      
      # Visualization
      ggplot(external_regression_data, aes(x = OverallGrade, y = ProposalCombined, col=Gender)) +
        geom_jitter(alpha = .5) +
        facet_grid(InstType ~ Division, margins = TRUE) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
      
      
      
      # Rearrenge Data
      OvGrPropCombDF<- external_regression_data[,c(15,5,6,8:14)]
      OvGrPropCombDF$AmountRequested<-log(OvGrPropCombDF$AmountRequested)
      
      (with(OvGrPropCombDF, table(OverallGrade,ProposalCombined)))
      # Fit a model with all variables and interaction
      OvGrProMod<- polr(OverallGrade~.+Gender:Division+Gender:PercentFemale, 
                        data=OvGrPropCombDF,
                        method="logistic",
                        Hess=T) 
      summary(OvGrProMod)
      
      # Correlation Plot
      OvGrProDummy <- dummyVars("~.-OverallGrade",data=OvGrPropCombDF, fullRank=F)
      X <- as.data.frame(predict(OvGrProDummy,OvGrPropCombDF))
      X<-cor(X)
      corrplot(X,method = "color",type="upper",
               title="Model: OverallGrade~ApplicantTrack ..")
      
      
      # p value calculation with a normal approximation (we have enough data to make this assumption)
      ctable<-coef(summary(OvGrProMod))
      p <- pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
      (ctable<-cbind(ctable,"p value"=p)) 
      
      # Confidence intervals Profile or Wald?
      # With profile likelihood
      # Message: profiling has found a better solution, so original fit had not converged
      # PCI <- confint(OvGrProMod)
      
      # Wald CI
      WCI <- confint.default(OvGrProMod)
      
      # Puting them together
      # CI<- cbind(Profile=PCI,Wald=WCI, `lenght PCI`= PCI[,2]-PCI[,1],
      #            `lenght WCI`= WCI[,2]-WCI[,1] )
      # Diff<- round(CI[,5]-CI[,6],4)
      # CI <- cbind(CI,Diff)
      
      # As the difference between PCI and WCI is not big, due to the fact that between variables
      # Te correlation is small (only ApplicantTrack is more correlated to OverallGrade)
      
      # Probalilities
      (prob<-exp(cbind(OR=coef(OvGrAppMod), WCI)))
      
      # Confusion Matrices 
      pred<-predict(OvGrProMod,OvGrPropCombDF) # class prediction
      (tab<- table(Prediction=pred,
                   Observed=OvGrPropCombDF$OverallGrade))
      
      
      # Miss Clasification Error  MCE
      (mce <- 1- sum(diag(tab))/sum(tab)) #[1] 0.2612446
      
      # Test the Out of Sample Miss classification error
      
      # Split the data
      set.seed(77)
      split<-sample.split(OvGrPropCombDF$OverallGrade, SplitRatio = 0.8)
      Train<-subset(OvGrPropCombDF, split=="TRUE")
      Test <-subset(OvGrPropCombDF, split=="FALSE")
      
      # Fit the model in the Train data
      B.Model <- update(OvGrProMod,.~.,data=Train)
      
      # Confusion Matrix Out of Bag 
      pred<-predict(B.Model,Test) # class prediction
      tab<- table(pred,Test[,'OverallGrade'])
      
      # Out of Sample Miss Clasification Error  OOSMCE
      (OOSMCE <- 1- sum(diag(tab))/sum(tab)) #[1] 0.2314815
      
      # Out of Sample Accuracy OOSACC
      (OOSACC <- sum(diag(tab))/sum(tab)) # [1] 0.7685185
      
      

# Remarks -----------------------------------------------------------------

# Overall, with the external data we can't predict well, neither ApplicantTrack, nor ProposalCombined,
# From the graphs, there is no visible gender influence in either qualifications.
# From the regressions: in non of the models is Gender significant.
# To answer wether gender makes a different impact in the models using track, and proposal in the OverallGrade,
# we did the visual inspection and fit the models. The resulting Conficence intervals are as follow
      
      # For Overall ~ Proposal
      # ...
      # PercentFemale         -1.395184559 -0.22342140
      # Age                   -0.004244672  0.02650406
      # Genderf               -0.892378982  0.24893195  ** 0 is not in the CI
      # DivisionDiv 2         -0.238643647  0.50781855
      # DivisionDiv 3         -0.521638527  0.20612700
      # IsContinuation1        0.051392402  0.64952788
      # PreviousRequest1      -0.462781545  0.23218910
      # InstTypeOther         -1.342374564 -0.16497641
      # InstTypeUAS/UTE       -1.111066701 -0.12029946
      # InstTypeUni           -0.569378012  0.06228260
      # AmountRequested       -0.143750783  0.27463240
      # Genderf:DivisionDiv 2 -1.102738968  0.41643949
      # Genderf:DivisionDiv 3 -0.598587028  0.73173728
      # PercentFemale:Genderf  0.033131445  2.15695362
      
      # For OverallGrade ~ ApplicantTrack
      #                           2.5 %      97.5 %
      # .....
      # PercentFemale         -1.154164469 -0.13713767
      # Age                   -0.008583641  0.01799574
      # Genderf               -1.168951717 -0.18483020 ** there appears to be an influence of gender in a
      # DivisionDiv 2         -0.378666572  0.27265816    negative way
      # DivisionDiv 3         -0.724775558 -0.09500520
      # IsContinuation1        0.239925669  0.75690091
      # PreviousRequest1      -0.185109578  0.41980901
      # InstTypeOther         -0.651346032  0.37019513
      # InstTypeUAS/UTE       -0.270301230  0.61058140
      # InstTypeUni           -0.404266725  0.14850529
      # AmountRequested       -0.100085502  0.25945678
      # Genderf:DivisionDiv 2 -0.207977363  1.07084032
      # Genderf:DivisionDiv 3 -0.065855669  1.07878897
      # PercentFemale:Genderf  0.472881107  2.30013902
      # 
      