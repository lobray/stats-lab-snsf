#######################################################################
####           Ordinal Regressions for external reviewers     #########
#######################################################################

# In this file all grades are considered as ordered factors.

# Regression 1: ProposalCombine~all
# Regression 2: ApplicantTrack ~all
# Regression 3: OverallGrade ~ ApplicantTrack + ProposalCombine
# Does gender make a different impact in the models
# See whether gender + applicant or gender + proposal have a bigger impact on the OverallGrade



# Initialize functions and get External Regression Data ------------------

load("snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

library(ordinal)
library(ggplot2)
library(MASS)
library(effects)
library(coin)

rm(applications,reviews,referee_grades, test)


# Get the Regression data----
external_regression_data<-prepare_data_external_log_regression(final.apps,final.external)

external_regression_data$logAmount<-log(external_regression_data$AmountRequested)

# Select the final variables

data<- subset(external_regression_data,select = -c(ProjectID, OverallGrade, AmountRequested, IsApproved,
                                                   ScientificRelevance, Suitability))

str(data)


# COMBINE PROPOSAL -------------------------------------------------------
## Run an Ordinal Logistic Regression with the ordinal package

    # Visualization----
      ggplot(external_regression_data, aes(x = ProposalCombined, y = PercentFemale, col=Gender)) +
        geom_jitter(alpha = .5) +
        #facet_grid(InstType ~ Division, margins = TRUE) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    
    
    # Fit a full model----
      fit1 <- clm(ProposalCombined ~Gender*(Division+PercentFemale)+Age+Division+IsContinuation+
                    PreviousRequest+InstType+Semester+logAmount,
                  data=data)
    # fit a null Model
      fit.null<- clm(ProposalCombined~1, data=data)
      diff.df<-fit.null$df.residual- fit1$df.residual
      
      LR<-(-2*(fit.null$logLik-fit1$logLik))
      1-pchisq(LR,df=diff.df) # 0   I am not sure this is a good thing
      
      # I am not using Gender:Applicant track here as ApplicantTrack is not in the model
      summary(fit1)  # cond.H very high
      
    # Veall and Zimmerman R^2 for models with ordial data ----
      # Ref: Generalized Linear Models and Extentions. W.Hardin and J.Hilbe
      # https://books.google.ch/books?id=tOeqO6Hs-6gC&lpg=PP1&hl=el&pg=PA52&redir_esc=y#v=onepage&q&f=false
      # R^2=[(delta-1)/(delta -R_m^2)]*R_m^2
      # where delta=n/(2*loglikelihood(null))
      # and R_m^2 = [1-logLik.model/loglik.null]
      
      n<-length(data$ProposalCombined)
      d<-n/(2*fit.null$logLik)
      R2.M <- 1-fit1$logLik/fit.null$logLik
      PseudoR2<- ((d-1)/(d-R2.M))*R2.M
      
      
    # Test the equidistant Thresholds--------  
      # Are treshold Equidistant
      diff(coef(fit1)[1:5]) # no
      
      # Try Model with equidistant threshold
      fm2<-clm(ProposalCombined ~Gender*(Division+PercentFemale)+Age+Division+IsContinuation+
                 PreviousRequest+InstType+Semester+logAmount,
              data=data, threshold = "equidistant") 
      summary(fm2)
      
      #Compare the two models to see if there is evidence against equidistant thresholds
      anova(fit1,fm2)
      
      #p-value is almost 0, there is evidence against equidistant thresholds. Therefore,
      # continue using flexible thresholds
      
    # Variable Selection -----
    
      drop1(fit1, test = "Chi")
      # Gender:Division is the first candidate to leave out, then Semester, PreviousRequest and Age
      
      fit1<-update(fit1,.~.-Gender:Division)
      fit1<-update(fit1,.~.-Semester)
      fit1<-update(fit1,.~.-PreviousRequest)
      fit1<-update(fit1,.~.-Age)
      
      
      summary(fit1)
      drop1(fit1, test = "Chi")
      
    # After variable Selection, the final model----
      Model.Prop<- clm(ProposalCombined ~ Gender + Division + PercentFemale + IsContinuation + 
                    InstType + logAmount + Gender:PercentFemale,data=data)
      summary(Model.Prop) #cond.H still high
      
      table(data$ProposalCombined)
      
      drop1(Model.Prop, test = "Chi")       # Cannot be imporved
                                        
      confint(Model.Prop)
      
      # Odds ratio and confidence intervals for Odds Ratio
      round(exp(cbind(OR=Model.Prop$beta,confint(Model.Prop))), 2)
      
      convergence(Model.Prop) 
      
    # Variable Importance----
      # Not sure which meassure to use here. I'll try with LogLikelihood

      outcomeName <- 'ProposalCombined'
      predictorNames <- c('Gender', 'Division', 'PercentFemale','IsContinuation',
                            'InstType', 'logAmount')
      predictions <- predict(object=Model.Prop, data[,predictorNames], type="class")
        # A quick check
          table(data$ProposalCombined,predictions$fit)
          #Still classifies in mostly in category 4
          sum(predictions$fit=="4") # 923 out of 1623 ???
          sum(external_regression_data$ProposalCombined=="4") #[1] 671

          # Initialize vectors for randomization
          ll.Reference<-Model.Prop$logLik                           # Reference LogLikelihood
          k <- length(coef(Model.Prop))                             # Number of estimated parameters
          AIC.Reference<-(-2*Model.Prop$logLik+2*k)                 # Reference AIC 
          
          # We shuffle and refit. If we have a smaller logLike -> the model is worst -> Variable is important
          # We shuffle and refit. If we have a bigger AIC -> the model is worst -> variable is important
          shuffletimes <- 100  #number of interactions
          
          featuresMeanLR <- featuresMeanAIC <- ll.featuresProportions<-AIC.featuresProportions<-c()
          AIC.percentvariation <-c()
          for (feature in predictorNames) {
            featureLl <- c()
            featureAIC<-c()
            shuffledData <- data[,c(outcomeName, predictorNames)]
            for (iter in 1:shuffletimes) {
              shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
              Model.tmp <- update(Model.Prop,.~.,data=shuffledData)
              featureLl <- c(featureLl,Model.tmp$logLik)
              featureAIC <- c(featureAIC, (-2*Model.tmp$logLik+2*k) )
            }
            featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
            featuresMeanAIC<- c(featuresMeanAIC, mean(featureAIC-AIC.Reference))
            ll.featuresProportions<-c(ll.featuresProportions,mean(ll.Reference>featureLl))
            AIC.featuresProportions<-c(AIC.featuresProportions,mean(AIC.Reference<featureAIC))
            AIC.percentvariation <- c(AIC.percentvariation, (mean(featureAIC)/AIC.Reference-1)*100)
            # Proportions -> In how many runs does the Reference is less than the shuffled
          }

          # Shuffle.Result.Prop <- data.frame('feature'=predictorNames, 'Diff.Ll'=featuresMeanLR,
          #                              'Proportion.Ll'=ll.featuresProportions, 'Diff.AIC'=featuresMeanAIC,
          #                              'Proportion.AIC'=AIC.featuresProportions)
          
          Shuffle.Result.Prop <- data.frame('feature'=predictorNames, 'Diff.AIC'=featuresMeanAIC,
                                            'Proportion.AIC'=AIC.featuresProportions,
                                            'Percent.AIC'=AIC.percentvariation)
          
          Shuffle.Result.Prop <- Shuffle.Result.Prop[order(Shuffle.Result.Prop$'Diff.AIC', decreasing=TRUE),]
          print(Shuffle.Result.Prop)
          
       
       # doing it like this, it turns out that all the variables are important. But this is to be expected
       # As we have previously select the variables.
       # Just to play around I will do the same, but with the original model and see if we get
       # the same result, i.e. the same important variables
       
       predictorNames2 <- c('Gender', 'Division', 'PercentFemale', 'Age','IsContinuation',
                           'PreviousRequest','InstType', 'Semester','logAmount')
      
       # Initialize vectors for randomization
       ll.Reference<-fit1$logLik                           # Reference LogLikelihood
       k <- length(coef(fit1))                             # Number of estimated parameters
       AIC.Reference<-(-2*fit1$logLik+2*k)                 # Reference AIC 
       
       # We shuffle and refit. If we have a smaller logLike -> the model is worst -> Variable is important
       # We shuffle and refit. If we have a bigger AIC -> the model is worst -> variable is important
       shuffletimes <- 100  #number of interactions
       
       featuresMeanLR <- featuresMeanAIC <- ll.featuresProportions<-AIC.featuresProportions<-c()
       AIC.percentvariation <-c()
       for (feature in predictorNames2) {
         featureLl <- c()
         featureAIC<-c()
         shuffledData <- data[,c(outcomeName, predictorNames2)]
         for (iter in 1:shuffletimes) {
           shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
           Model.tmp <- update(fit1,.~.,data=shuffledData)
           featureLl <- c(featureLl,Model.tmp$logLik)
           featureAIC <- c(featureAIC, (-2*Model.tmp$logLik+2*k) )
         }
         featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
         featuresMeanAIC<- c(featuresMeanAIC, mean(featureAIC-AIC.Reference))
         ll.featuresProportions<-c(ll.featuresProportions,mean(ll.Reference>featureLl))
         AIC.featuresProportions<-c(AIC.featuresProportions,mean(AIC.Reference<featureAIC))
         AIC.percentvariation <- c(AIC.percentvariation, (mean(featureAIC)/AIC.Reference-1)*100)
         # Proportions -> In how many runs does the Reference is less than the shuffled
       }
       
       # Shuffle.Result.Prop2 <- data.frame('feature'=predictorNames2, 'Diff.Ll'=featuresMeanLR,
       #                                   'Proportion.Ll'=ll.featuresProportions, 'Diff.AIC'=featuresMeanAIC,
       #                                   'Proportion.AIC'=AIC.featuresProportions)
       
       Shuffle.Result.Prop2 <- data.frame('feature'=predictorNames2, 'Diff.AIC'=featuresMeanAIC,
                                         'Proportion.AIC'=AIC.featuresProportions,
                                         'Percent.AIC'=AIC.percentvariation)
       
       Shuffle.Result.Prop2 <- Shuffle.Result.Prop2[order(Shuffle.Result.Prop2$'Diff.AIC', decreasing=TRUE),]
       print(Shuffle.Result.Prop2)
       
       # We do get the same variables as important. The one with the largest effect is Amount Requested.
       # Interesting. Remeber we are evaluating Project Assessment
    
       mycol=colorRampPalette(c("red","green"))(length(predictorNames2))
       par(las=2,mfrow=c(1,1),mai=c(2,2,2,2))
       barplot(Shuffle.Result.Prop2[,4], names.arg = Shuffle.Result.App2[,1],
               horiz = TRUE,col = mycol,cex.names = 0.7,
               main="Variable Importance of Combined Project Assessment",
               xlab = "Percent of variation on the AIC",
               xlim=c(0,1.2),
               cex.axis = 0.6,
               cex.main=0.8)
  

    # Effects ---- 
       
       # Effect of Gender on the Combined Project Grade
       
       par(mfrow=c(1,2))
       # 1. get the effects and prepare a matrix to plot
       gender.prob<-Effect("Gender", mod=Model.Prop)
       gender.prob<-gender.prob$prob
       row.names(gender.prob)<-levels(external_regression_data$Gender)
       colnames(gender.prob)<-levels(external_regression_data$ProposalCombined)
       
       # 2. Plot
       x<-as.numeric(levels(external_regression_data$ProposalCombined))
       plot(x,gender.prob[1,], ylab="Probabilities", 
            xlab="Grade",main="Effect of Gender on Combined Project Grade",
            type="l", col="blue", cex.main=0.8)
       lines(x,gender.prob[2,], col="green")
       legend("topleft", legend=c("male","female"), lty=1,
              col=c("blue","green"),bty="n")
       
       
       # Plot of ApplicantTrack vs Gender to try to understand better the above
       barplot(t(with(external_regression_data,prop.table(table(ProposalCombined,Gender),2))),
               beside = TRUE,
               col=c("blue","green" ))
       legend("topleft", legend=c("male","female"), pch=15,
              col=c("blue","green"),bty="n")
       
       # If I understand correctly, the difference in probability if the effect on gender
       diff<- round((gender.prob[2,]-gender.prob[1,])*100,2)
       diff
       # 1     2     3     4     5     6 
       # 1.43  1.51  1.42  0.96 -0.51 -4.82 
       # Project were the main Applicant is a woman are 1.43% more likely of getting 1's,
       # and 4.82% less likely of getting 6?
       par(mfrow=c(1,1))
       
       # Plot the effect of log Amount
       plot(Effect("logAmount",mod=Model.Prop))
       
       
       
       
# APPLICANT TRACK  ---- 
    # Visualization----
       ggplot(external_regression_data, aes(x = ApplicantTrack, y = PercentFemale, col=Gender)) +
         geom_jitter(alpha = .5) +
         #facet_grid(InstType ~ Division, margins = TRUE) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))    
       
    # Fit a full model ----
      fit2 <- clm(ApplicantTrack ~Gender*(Division+PercentFemale)+Age+Division+IsContinuation+
                    PreviousRequest+InstType+Semester+logAmount,
                    data=data)
      
      summary(fit2)  # cond.H very high 
      
    # Variable Selection ----      
            
            drop1(fit2, test = "Chi")
            # Gender:Division is the first candidate to leave out, then Semester, PreviousRequest and Age
            
            fit2<-update(fit2,.~.-Gender:Division)
            fit2<-update(fit2,.~.-PreviousRequest)
            fit2<-update(fit2,.~.-Semester)
            fit2<-update(fit2,.~.-Age)
            
            
    # After variable Selection, the final model ----
            Model.App<- clm(ApplicantTrack ~ Gender + Division + PercentFemale + IsContinuation + 
                          InstType + logAmount + Gender:PercentFemale,data=data)
            summary(Model.App) #cond.H still high
            # In this Model, Gender is Significant, 
            
            # fit a null Model.App
            fit.null<- clm(ApplicantTrack~1, data=data)
            fit.null$df.residual- Model.App$df.residual
            
            LR<--2*(fit.null$logLik-Model.App$logLik)
            1-pchisq(LR,df=10)
            
            anova(fit.null,Model.App) 
            # gives the same result. Our model explains better than the empty model
            
            drop1(Model.App, test = "Chi")         # Cannot be imporved
            
            
            # Odds ratio and confidence intervals for Odds Ratio
              round(exp(cbind(OR=Model.App$beta,confint(Model.App))), 2)
              
            # Any of the c.i. include 1 (only Division 3). 
            # There is an slightly effect of Gender in the Application Track grade
              
              convergence(Model.App)  #looks ok
            
  
    # Variable Importance----
            # Not sure which meassure to use here. I'll try with LogLikelihood
            
            outcomeName <- 'ApplicantTrack'
            predictorNames <- c('Gender', 'Division', 'PercentFemale','IsContinuation',
                                'InstType', 'logAmount')
            predictions <- predict(object=Model.App, data[,predictorNames], type="class")
            # A quick check
            table(data$ApplicantTrack,predictions$fit)
            #Still classifies mostly in category 4
            sum(predictions$fit=="4") # 248 out of 1623 ???
            
            # Initialize vectors for randomization
            ll.Reference<-Model.App$logLik                           # Reference LogLikelihood
            k <- length(coef(Model.App))                             # Number of estimated parameters
            AIC.Reference<-(-2*Model.App$logLik+2*k)                 # Reference AIC 
            
            # We shuffle and refit. If we have a smaller logLike -> the model is worst -> Variable is important
            # We shuffle and refit. If we have a bigger AIC -> the model is worst -> variable is important
            shuffletimes <- 100  #number of interactions
            
            featuresMeanLR <- featuresMeanAIC <- ll.featuresProportions<-AIC.featuresProportions<-c()
            AIC.percentvariation <-c()
            for (feature in predictorNames) {
              featureLl <- c()
              featureAIC<-c()
              shuffledData <- data[,c(outcomeName, predictorNames)]
              for (iter in 1:shuffletimes) {
                shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
                Model.tmp <- update(Model.App,.~.,data=shuffledData)
                featureLl <- c(featureLl,Model.tmp$logLik)
                featureAIC <- c(featureAIC, (-2*Model.tmp$logLik+2*k) )
              }
              featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
              featuresMeanAIC<- c(featuresMeanAIC, mean(featureAIC-AIC.Reference))
              ll.featuresProportions<-c(ll.featuresProportions,mean(ll.Reference>featureLl))
              AIC.featuresProportions<-c(AIC.featuresProportions,mean(AIC.Reference<featureAIC))
              AIC.percentvariation <- c(AIC.percentvariation, (mean(featureAIC)/AIC.Reference-1)*100)
              # Proportions -> In how many runs does the Reference is less than the shuffled
            }
            
            # Shuffle.Result <- data.frame('feature'=predictorNames, 'Diff.Ll'=featuresMeanLR,
            #                              'Proportion.Ll'=ll.featuresProportions, 'Diff.AIC'=featuresMeanAIC,
            #                              'Proportion.AIC'=AIC.featuresProportions)
            # Shuffle.Result <- Shuffle.Result[order(Shuffle.Result$'Diff.Ll', decreasing=TRUE),]
            # print(Shuffle.Result)
            
            
            Shuffle.Result.App <- data.frame('Feature'=predictorNames, 'Diff.AIC'=featuresMeanAIC,
                                              'Proportion.AIC'=AIC.featuresProportions,
                                              'Percent.AIC'=AIC.percentvariation)
            Shuffle.Result.App <- Shuffle.Result.App[order(Shuffle.Result.App$'Diff.AIC', decreasing=TRUE),]
            print(Shuffle.Result.App)
            
            # doing it like this, it turns out that all the variables are important. But this is to be expected
            # as we have previously select the variables.
            # Just to play around I will do the same, but with the original model and see if we get
            # the same result,i.e. the same important variables
            
            predictorNames2 <- c('Gender', 'Division', 'PercentFemale', 'Age','IsContinuation',
                                 'PreviousRequest','InstType', 'Semester','logAmount')
            
            # Initialize vectors for randomization
            ll.Reference<-fit2$logLik                           # Reference LogLikelihood
            k <- length(coef(fit2))                             # Number of estimated parameters
            AIC.Reference<-(-2*fit2$logLik+2*k)                 # Reference AIC 
            shuffletimes <- 100                                 # Number of interactions
            
            featuresMeanLR <- featuresMeanAIC <- ll.featuresProportions<-AIC.featuresProportions<-c()
            AIC.percentvariation <-c()
            for (feature in predictorNames2) {
              featureLl <- c()
              featureAIC<- c()
              shuffledData <- data[,c(outcomeName, predictorNames2)]
              for (iter in 1:shuffletimes) {
                shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
                Model.tmp <- update(fit2,.~.,data=shuffledData)
                featureLl <- c(featureLl,Model.tmp$logLik)
                featureAIC <- c(featureAIC, (-2*Model.tmp$logLik+2*k) )
              }
              featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
              featuresMeanAIC<- c(featuresMeanAIC, mean(featureAIC-AIC.Reference))
              ll.featuresProportions<-c(ll.featuresProportions,mean(ll.Reference>featureLl))
              AIC.featuresProportions<-c(AIC.featuresProportions,mean(AIC.Reference<featureAIC))
              AIC.percentvariation <- c(AIC.percentvariation, (mean(featureAIC)/AIC.Reference-1)*100)
            }
            
            # Shuffle.Result2 <- data.frame('feature'=predictorNames2, 'Diff.Ll'=featuresMeanLR,
            #                               'Proportion.Ll'=ll.featuresProportions, 'Diff.AIC'=featuresMeanAIC,
            #                               'Proportion.AIC'=AIC.featuresProportions)
            
            Shuffle.Result.App2 <- data.frame('Feature'=predictorNames2, 'Diff.AIC'=featuresMeanAIC,
                                             'Proportion.AIC'=AIC.featuresProportions,
                                             'Percent.AIC'=AIC.percentvariation)
            Shuffle.Result.App2 <- Shuffle.Result.App2[order(Shuffle.Result.App2$'Diff.AIC', decreasing=TRUE),]
            print(Shuffle.Result.App2)
            
            mycol=colorRampPalette(c("red","geen"))(length(predictorNames2))
            
            par(las=2,mfrow=c(1,1),mai=c(2,2,2,2))
            barplot(Shuffle.Result.App2[,4], names.arg = Shuffle.Result.App2[,1],
                    horiz = TRUE,col = mycol,cex.names = 0.7,
                    main="Variable Importance of Applicant Track",
                    xlab = "Percent of variation on the AIC",
                    xlim=c(0,1.2),
                    cex.axis = 0.6,
                    cex.main=0.8)
            # We do get the same results...
            # Institution Type is the most important variable ApplicantTrack
            
            
    # Effects ---- 
            
            # Effect of Gender on the Applicant Track grade
            par(mfrow=c(1,2))
            # 1. get the effects and prepare a matrix to plot
            gender.prob<-Effect("Gender", mod=Model.App)
            gender.prob<-gender.prob$prob
            row.names(gender.prob)<-levels(external_regression_data$Gender)
            colnames(gender.prob)<-levels(external_regression_data$ApplicantTrack)
            
            # 2. Plot
            x<-as.numeric(levels(external_regression_data$ApplicantTrack))
            plot(x,gender.prob[1,], ylab="Probabilities", 
                 xlab="Grade",main="Effect of Gender on Applicant Track grade",
                 type="l", col="blue", cex.main=0.8)
            lines(x,gender.prob[2,], col="green")
            legend("topleft", legend=c("male","female"), lty=1,
                   col=c("blue","green"),bty="n")
            
            # Plot of ApplicantTrack vs Gender to try to understand better the above
            barplot(t(with(external_regression_data,prop.table(table(ApplicantTrack,Gender),2))),
                    beside = TRUE,
                    col=c("blue","green" ))
            legend("topleft", legend=c("male","female"), pch=15,
                   col=c("blue","green"),bty="n")
            
            # If I understand correctly, the difference in probability if the effect on gender
            diff<- round((gender.prob[2,]-gender.prob[1,])*100,2)
            diff
            # 1     2     3     4     5     6 
            # 0.44  0.63  0.92  1.48  2.49 -5.96 
            # womans are 0.44% more likely of getting 1's, and 5.96% less likely of getting 6?
           
              
            
            
# OVERALL GRADE----
            
    data<- subset(external_regression_data,select = -c(ProjectID, AmountRequested, IsApproved,ScientificRelevance,Suitability))  
    
    # Visualization----
        ggplot(external_regression_data, aes(x = OverallGrade, y = ApplicantTrack, col=ProposalCombined)) +
          geom_jitter(alpha = .5) +
          #facet_grid(InstType ~ Division, margins = TRUE) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))    
              
    # Fit a full model ----  
            
    fit3 <- clm(OverallGrade ~Gender*(Division+PercentFemale)+ ProposalCombined+ApplicantTrack+
                  Age+IsContinuation+PreviousRequest+InstType+Semester+logAmount,
                  data=data)
    
    summary(fit3)
    
    # Variable Selection -----
    
    drop1(fit3, test = "Chi")
    
    fit3<-update(fit3,.~.-InstType)
    fit3<-update(fit3,.~.-Gender:Division)
    fit3<-update(fit3,.~.-logAmount)
    fit3<-update(fit3,.~.-PreviousRequest)
    fit3<-update(fit3,.~.-Semester)
    fit3<-update(fit3,.~.-Division)
    fit3<-update(fit3,.~.-Age)
    
    # It seems like Gender:Female has some effect
    
    # After variable Selection, the final model ----   
    
    Model.Overall<-clm(OverallGrade ~ Gender + PercentFemale + ProposalCombined + ApplicantTrack + 
                         IsContinuation + Gender:PercentFemale , data=data)
    summary(Model.Overall)
    
    # fit a null Model
    fit.null<- clm(OverallGrade~1, data=data)
    fit.null$df.residual- Model.Overall$df.residual
    
    LR<--2*(fit.null$logLik-Model.Overall$logLik)
    1-pchisq(LR,df=10)
    
    anova(fit.null,Model.Overall)  # gives the same result. Our model explains better than the empty model
    
    # Odds ratio and confidence intervals for Odds Ratio
    round(exp(cbind(OR=Model.Overall$beta,confint(Model.Overall))), 2)
    
    convergence(Model.Overall)
    
    # Variable Importance----
    # Not sure which meassure to use here. I'll try with LogLikelihood
    
    outcomeName <- 'OverallGrade'
    predictorNames <- c('Gender', 'PercentFemale','ProposalCombined','ApplicantTrack','IsContinuation')
    
    # Initialize vectors for randomization
    ll.Reference<-Model.Overall$logLik                           # Reference LogLikelihood
    k <- length(coef(Model.Overall))                             # Number of estimated parameters
    AIC.Reference<-(-2*Model.Overall$logLik+2*k)                 # Reference AIC 
    
    # We shuffle and refit. If we have a smaller logLike -> the model is worst -> Variable is important
    # We shuffle and refit. If we have a bigger AIC -> the model is worst -> variable is important
    shuffletimes <- 10  #number of interactions
    
    featuresMeanLR <- featuresMeanAIC <- ll.featuresProportions<-AIC.featuresProportions<-c()
    AIC.percentvariation <-c()
    for (feature in predictorNames) {
      featureLl <- c()
      featureAIC<-c()
      shuffledData <- data[,c(outcomeName, predictorNames)]
      for (iter in 1:shuffletimes) {
        shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
        Model.tmp <- update(Model.Overall,.~.,data=shuffledData)
        featureLl <- c(featureLl,Model.tmp$logLik)
        featureAIC <- c(featureAIC, (-2*Model.tmp$logLik+2*k) )
      }
      featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
      featuresMeanAIC<- c(featuresMeanAIC, mean(featureAIC-AIC.Reference))
      ll.featuresProportions<-c(ll.featuresProportions,mean(ll.Reference>featureLl))
      AIC.featuresProportions<-c(AIC.featuresProportions,mean(AIC.Reference<featureAIC))
      AIC.percentvariation <- c(AIC.percentvariation, (mean(featureAIC)/AIC.Reference-1)*100)
      # Proportions -> In how many runs does the Reference is less than the shuffled
    }
    
    #Ov.Shuffle.Result <- data.frame('feature'=predictorNames, 'Diff.Ll'=featuresMeanLR,
    #                              'Proportion.Ll'=ll.featuresProportions, 'Diff.AIC'=featuresMeanAIC,
    #                              'Proportion.AIC'=AIC.featuresProportions)
    Ov.Shuffle.Result <- data.frame('Feature'=predictorNames, 'Diff.AIC'=featuresMeanAIC,
                                      'Proportion.AIC'=AIC.featuresProportions,
                                      'Percent.AIC'=AIC.percentvariation)
    
    Ov.Shuffle.Result <- Ov.Shuffle.Result[order(Ov.Shuffle.Result$'Diff.AIC', decreasing=TRUE),]
    print(Ov.Shuffle.Result)
    
    # doing it like this, it turns out that all the variables are important. But this is to be expected
    # as we have previously select the variables.
    # Just to play around I will do the same, but with the original model and see if we get
    # the same result,i.e. the same important variables
    
    fit3 <- clm(OverallGrade ~Gender*(Division+PercentFemale)+ ProposalCombined+ApplicantTrack+
                  Age+IsContinuation+PreviousRequest+InstType+Semester+logAmount,
                data=data)
    
    predictorNames2 <- c('Gender', 'Division', 'PercentFemale','ProposalCombined','ApplicantTrack', 'Age','IsContinuation',
                         'PreviousRequest','InstType', 'Semester','logAmount')
    
    # Initialize vectors for randomization
    ll.Reference<-fit3$logLik                           # Reference LogLikelihood
    k <- length(coef(fit3))                             # Number of estimated parameters
    AIC.Reference<-(-2*fit3$logLik+2*k)                 # Reference AIC 
    shuffletimes <- 100                                 # Number of interactions
    
    featuresMeanLR <- featuresMeanAIC <- ll.featuresProportions<-AIC.featuresProportions<-c()
    AIC.percentvariation <-c()
    for (feature in predictorNames2) {
      featureLl <- c()
      featureAIC<- c()
      shuffledData <- data[,c(outcomeName, predictorNames2)]
      for (iter in 1:shuffletimes) {
        shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
        Model.tmp <- update(fit3,.~.,data=shuffledData)
        featureLl <- c(featureLl,Model.tmp$logLik)
        featureAIC <- c(featureAIC, (-2*Model.tmp$logLik+2*k) )
      }
      featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
      featuresMeanAIC<- c(featuresMeanAIC, mean(featureAIC-AIC.Reference))
      ll.featuresProportions<-c(ll.featuresProportions,mean(ll.Reference>featureLl))
      AIC.featuresProportions<-c(AIC.featuresProportions,mean(AIC.Reference<featureAIC))
      AIC.percentvariation <- c(AIC.percentvariation, (mean(featureAIC)/AIC.Reference-1)*100)
    }
    
    # Ov.Shuffle.Result2 <- data.frame('feature'=predictorNames2, 'Diff.Ll'=featuresMeanLR,
    #                               'Proportion.Ll'=ll.featuresProportions, 'Diff.AIC'=featuresMeanAIC,
    #                               'Proportion.AIC'=AIC.featuresProportions)
   
    Ov.Shuffle.Result2 <- data.frame('Feature'=predictorNames2, 'Diff.AIC'=featuresMeanAIC,
                                    'Proportion.AIC'=AIC.featuresProportions,
                                    'Percent.AIC'=AIC.percentvariation)
    Ov.Shuffle.Result2 <- Ov.Shuffle.Result2[order(Ov.Shuffle.Result2$'Diff.AIC', decreasing=TRUE),]
    print(Ov.Shuffle.Result2)
    
    
    # We do get the same results...
    # Proposal Combined is more important than ApplicantTrack, and there is some effect of gender
    
    mycol=colorRampPalette(c("red","blue"))(length(predictorNames2))
    par(las=2,mfrow=c(1,1),mai=c(2,2,2,2))
    barplot(Ov.Shuffle.Result[,4], names.arg = Ov.Shuffle.Result[,1],
            horiz = TRUE,col = mycol,cex.names = 0.6,
            main="Variable Importance of OverallGrade",
            xlab = "Percent of variation on the AIC",
            xlim = c(0,80),
            cex.axis = 0.7,
            cex.main=0.8)
    
    # Effects ---- 
    
    # Effect of Gender on the OverallGrade grade
    par(mfrow=c(1,2))
    # 1. get the effects and prepare a matrix to plot
    gender.prob<-Effect("Gender", mod=Model.Overall)
    gender.prob<-gender.prob$prob
    row.names(gender.prob)<-levels(external_regression_data$Gender)
    colnames(gender.prob)<-levels(external_regression_data$OverallGrade)
    gender.prob
    
    # 2. Plot
    x<-as.numeric(levels(external_regression_data$OverallGrade))
    plot(x,gender.prob[1,], ylab="Probabilities", 
         xlab="Grade",main="Effect of Gender on Overall Grade",
         type="l", col="blue", cex.main=0.8)
    lines(x,gender.prob[2,], col="green")
    legend("topleft", legend=c("male","female"), lty=1,
           col=c("blue","green"),bty="n")
    
    
    # Plot of ApplicantTrack vs Gender to try to understand better the above
    barplot(t(with(external_regression_data,prop.table(table(OverallGrade,Gender),2))),
            beside = TRUE,
            col=c("blue","green" ))
    legend("topleft", legend=c("male","female"), pch=15,
           col=c("blue","green"),bty="n")
    
    # If I understand correctly, the difference in probability if the effect on gender
    diff<- round((gender.prob[2,]-gender.prob[1,])*100,2)
    diff
    # 1     2     3     4     5     6 
    # 0.02  1.46  0.73 -2.19 -0.02  0.00 
    # womans are 0.02% more likely of getting 1's, and 5.96% less likely of getting 6?
    par(mfrow=c(1,1))
    
    
    
  # DIFFERENCE IN EVALUATION BETWEEN MALE AND FEMALE ----
    
    r.tab<-prop.table(table(external_reviews$ReviewerGender,external_reviews$OverallGrade),1)
    #mycol<-colorRampPalette(c("red", "green"))(6)
    par(las=1,mai=c(1,2,2,1))
    barplot(r.tab, beside = TRUE, col = c("pink","lightblue"), 
              main=paste("Difference on how Female and Male grade \n","Overall Grade"),
            sub="External Reviewers")
    legend("topleft",legend = c("female","male"), pch=15,
           col=c("pink","lightblue"), bty="n")
    
    # Women seem to be stricter than men (OverallGrade)
    
    r.tab<-prop.table(table(external_reviews$ReviewerGender,external_reviews$ApplicantTrack),1)
    #mycol<-colorRampPalette(c("red", "green"))(6)
    barplot(r.tab, beside = TRUE, col = c("pink","lightblue"), 
            main=paste("Difference on how Female and Male grade \n","Applicant Track"),
            sub="External Reviewers")
    legend("topleft",legend = c("female","male"), pch=15,
           col=c("pink","lightblue"), bty="n")
    
    
    # Women on average give less outstanding and excellent grades for ApplicantTrack
    
    r.tab<-prop.table(table(external_reviews$ReviewerGender,external_reviews$ScientificRelevance),1)
    #mycol<-colorRampPalette(c("red", "green"))(6)
    barplot(r.tab, beside = TRUE, col = c("pink","lightblue"), 
             main=paste("Difference on how Female and Male grade \n","Scientific Relevance"),
            sub="External Reviewers")
    legend("topleft",legend = c("female","male"), pch=15,
           col=c("pink","lightblue"), bty="n")
    
    r.tab<-prop.table(table(external_reviews$ReviewerGender,external_reviews$Suitability),1)
    #mycol<-colorRampPalette(c("red", "green"))(6)
    barplot(r.tab, beside = TRUE, col = c("pink","lightblue"), 
            main=paste("Difference on how Female and Male grade \n","Suitability"),
            sub="External Reviewers")
    legend("topleft",legend = c("female","male"), pch=15,
           col=c("pink","lightblue"), bty="n")
    
    # Women on average give less outstanding and excellent grades for ProposalCombined
    
  # LINEAR-BY-LINEAR association----
  

    ## Ranking:
    
    data <- external_regression_data
    data$OverallGrade <- ifelse(data$OverallGrade < 4, 3, data$OverallGrade)
    
    table(data$Gender, data$OverallGrade)
    cTab <- xtabs(~ Gender +OverallGrade, data=data)
    
    mycol<-colorRampPalette(c("green","blue"))(4)
    lbl<-lbl_test(cTab)
    lbl<-round(pvalue(lbl),5)
    
    spineplot(cTab, col=mycol, main=paste("Independence Test \n", "External Step"),
              cex.main=0.8,xlab=paste0("Gender \n", "p-value = ", lbl))
    
    ## p-value=0.0001658 -> reject H0 -> no independence of Overall Grade on Gender
    
    # Compare to chi-square test without ordered categories
    chisq_test(cTab)  # p-value = 0.0006229
    
    ## ApplicantTrack:
    
    data$ApplicantTrack <- ifelse(data$ApplicantTrack < 4, 3, data$ApplicantTrack)
    table(data$Gender, data$ApplicantTrack)
    
    t <- xtabs(~ Gender+ApplicantTrack, data=data)
    
    lbl<-lbl_test(t)
    lbl<-round(pvalue(lbl),5)
    spineplot(t,col=mycol,main=paste("Independence Test \n", "External Step"),
              cex.main=0.8,xlab=paste0("Gender \n", "p-value = ", lbl))
    
    # p-value = 9.481e-06 -> reject H0 -> no dependence of Applicant Track on gender
    
    # Compare to chi-square test without ordered categories
    chisq_test(t)   # p-value = 0.0001358
    
    
    ## Project:
    
    data$ProposalCombined <- ifelse(data$ProposalCombined < 4, 3, data$ProposalCombined)
    table(data$Gender, data$ProposalCombined)
    
    t <- xtabs(~ Gender+ProposalCombined, data=data)
    lbl<-lbl_test(t)
    lbl<-round(pvalue(lbl),5)
    spineplot(t, col=mycol,main=paste("Independence Test \n", "External Step"),
              cex.main=0.8,xlab=paste0("Gender \n", "p-value = ", lbl))
    
    # p-value = 0.0005938 -> reject H0 -> no dependence of ProposalCombined and gender
    
    # Compare to chi-square test without ordered categories
    chisq_test(t)   # p-value = 0.0006203
    
# Kappa Approach ----
    library(biostatUZH) 
    library(psy)        
    library(psych) 
    
    # Weights
    
      linear.weights <- matrix(c(1, 0.8, 0.6, 0.4, 0.2, 0, 0.8, 1, 0.8, 0.6, 0.4, 0.2, 
                                 0.6, 0.8, 1, 0.8, 0.6, 0.4, 0.4, 0.6, 0.8, 1, 0.8, 0.6,
                                 0.2, 0.4, 0.6, 0.8, 1, 0.8, 0, 0.2, 0.4, 0.6, 0.8, 1), 
                               nrow=6, ncol=6, byrow=T)
      
      
      custom.weights <- matrix(c(1, 0.8, 0.6, 0, 0, 0, 0, 1, 0.8, 0.6, 0, 0, 
                               0.6, 0.8, 1, 0.8, 0.6, 0, 0, 0.6, 0.8, 1, 0.8, 0.6,
                               0, 0, 0.6, 0.8, 1, 0.8, 0, 0, 0, 0.6, 0.8, 1), 
                             nrow=6, ncol=6, byrow=T)
    
    
    # OverallGrade vs Applicant Track
      
      kappa.matrix <- as.matrix(table('Overall Grade'=external_regression_data$OverallGrade,
                                      'Applicant Track'=external_regression_data$ApplicantTrack))
      plot(kappa.matrix, main= "Agreement Between Grades")
      kappa <- cohen.kappa(kappa.matrix, w=custom.weights)
      kappa  # 0.52  # with custom. weights 0.55
      
      
    # OverallGrade vs Scientific Relevance
     
      kappa.matrix <- as.matrix(table('Overall Grade'=external_regression_data$OverallGrade,
                                      'Scientific Relevance'=external_regression_data$ScientificRelevance))
      plot(kappa.matrix, main= "Agreement Between Grades")
      kappa <- cohen.kappa(kappa.matrix, w=custom.weights)
      kappa  # 0.71  # with custom. weights 0.73

    
    # OverallGrade vs Scientific Relevance
      
      kappa.matrix <- as.matrix(table('Overall Grade'=external_regression_data$OverallGrade,
                                      'Suitability'=external_regression_data$Suitability))
      plot(kappa.matrix, main= "Agreement Between Grades")
      kappa <- cohen.kappa(kappa.matrix, w=custom.weights)
      kappa  # 0.67  # with custom. weights 0.70
    
    # OverallGrade vs Combined Project Assessment
      
      kappa.matrix <- as.matrix(table('Overall Grade'=external_regression_data$OverallGrade,
                                      'Proposal Combined'=external_regression_data$ProposalCombined))
      plot(kappa.matrix, main= "Agreement Between Grades")
      kappa <- cohen.kappa(kappa.matrix, w=custom.weights)
      kappa  # 0.79   # with custom. weights 0.80 
      