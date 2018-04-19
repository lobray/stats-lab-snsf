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
library(ROCR)
library(car)
library(caTools)
library(caret)

# Get the Regression data
  external_regression_data<-prepare_data_external_log_regression(final.apps,final.external)

# Average the grade for ScientificRelevance ans Suitability
  external_regression_data$ProposalCombined <- round((external_regression_data$ScientificRelevance+
                                                        external_regression_data$Suitability)/2, 0)
  
  #response must be an order factor for odrinal Regression
  
  external_regression_data$ProposalCombined <- as.ordered(external_regression_data$ProposalCombined)
  external_regression_data$ApplicantTrack <- as.ordered(external_regression_data$ApplicantTrack)
  external_regression_data$OverallGrade <- as.ordered(external_regression_data$OverallGrade)
  str(external_regression_data)


# Fitting the Model for the project evaluation-------------------------------------------------------
## Run an Ordinal Logistic Regression

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
      
      
      
      
# Models for Overall Grade ------------------------------------------------

      fit1 <- polr(OverallGrade ~ Age+Gender+Division+ApplicantTrack, 
                   method="logistic",data=external_regression_data)
      fit2 <- polr(OverallGrade ~ Age+Gender+Division+ApplicantTrack+Gender:Division, 
                   method="logistic",data=external_regression_data)
      summary(fit2)
      
      deviance(fit1)-deviance((fit2))
      pchisq(3.139259, fit2$edf-fit1$edf, lower=FALSE)
      # [1] 0.2081223 -> not significant but need to include Gender:Division
      
      fit3 <- polr(OverallGrade ~ Age+Gender+Division+ApplicantTrack+Gender:Division+
                     PercentFemale, 
                   method="logistic",data=external_regression_data)
      summary(fit3)
      
      deviance(fit2)-deviance((fit3))
      pchisq(0.9582698, fit3$edf-fit2$edf, lower=FALSE)
      # [1] 0.3276232-> do not include PercentFemale
      
      fit4 <- polr(OverallGrade ~ Age+Gender+Division+ApplicantTrack+Gender:Division+
                     IsContinuation, 
                   method="logistic",data=external_regression_data)
      summary(fit4)
      
      deviance(fit2)-deviance((fit4))
      pchisq(16.81665, fit4$edf-fit2$edf, lower=FALSE)
      # [1] 4.117049e-05-> include IsContinuation
      
      fit5 <- polr(OverallGrade ~ Age+Gender+Division+ApplicantTrack+Gender:Division+
                     IsContinuation+InstType, 
                   method="logistic",data=external_regression_data)
      summary(fit5)
      
      deviance(fit4)-deviance((fit5))
      pchisq( 2.581763, fit5$edf-fit4$edf, lower=FALSE)
      # [1] 0.4606957-> not include InstType
      
      fit6 <- polr(OverallGrade ~ Age+Gender+Division+ApplicantTrack+Gender:Division+
                     IsContinuation+log(AmountRequested), 
                   method="logistic",data=external_regression_data)
      summary(fit6)
      
      deviance(fit4)-deviance((fit6))
      pchisq( 0.7346335, fit6$edf-fit4$edf, lower=FALSE)
      # [1] 0.3913854-> not include AmountRequested
      
      OvGrAppMod<-fit4
      
      
      
      leslie_reg_proposal <- glm(SimplifiedOverallGrade ~  
                                   ProposalCombined , data=external_regression_data, family="binomial")
      
      
      
# Model Diagnostic OG ~ ApplTrck ------------------------------------------

      coef(summary(OvGrAppMod))
      
      # p value calculation
      ctable<-coef(summary(OvGrAppMod))
      p <- pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
      (ctable<-cbind(ctable,"p value"=p)) 
      
      # Probalilities
      (prob<-exp(cbind(OR=coef(OvGrAppMod), confint(OvGrAppMod))))
      
      # Confusion Matrices 
      pred<-predict(OvGrAppMod,external_regression_data) # class prediction
      tab<- table(pred,external_regression_data$OverallGrade)
      
      # Miss Clasification Error  MCE
      (mce <- 1- sum(diag(tab))/sum(tab)) #[1] 0.4109673
      
      MCE<-c()
      ACC<-c()
      M=4
      for ( i in 1:M){
        
        # Resample the data with replacement and obtained In Bag indices
        ind<-createResample(external_regression_data[,'OverallGrade'],1)[[1]]
        InBag <- unique(ind)
        
        # Split the data
        Train<-final.data[ind,]
        Test<- final.data[-InBag,]
        
        # Fit the model in the Train data
        #B.Model <- polr(formula, 
                        # method="logistic",
                        # data=Train,
                        # Hess = TRUE)
        B.Model <- update(OvGrAppMod,.~.,data=Train)
        
        # Confusion Matrix Out of Bag 
        pred<-predict(B.Model,Test) # class prediction
        tab<- table(pred,Test[,'OverallGrade'])
        
        # Miss Clasification Error  MCE
        MCE[i] <- 1- sum(diag(tab))/sum(tab)
        
        # Out of Sample Accuracy
        ACC[i] <- sum(diag(tab))/sum(tab)
      } 
      
      # Average out of sample MCE
      BOOTMCE<- mean(MCE)
      
      # Average Accuracy
      AveAcc<- mean(ACC)
      
     
      hist(MCE,col="springgreen2", 
           main= "Out of Sample Missclassification Error",
           cex.main=0.8, sub=paste("Number of iterations ",M),cex.sub=0.8)
      hist(ACC,col="springgreen2", 
           main= "Out of Sample Accuracy ",
           cex.main=0.8, sub=paste("Number of iterations ",M),cex.sub=0.8)
      
      
      
      OvGrApplicTrackfit<- ModelDiagnostic(TestModel = OvGrAppMod,outcomeName = 'OverallGrade', M=10)
      