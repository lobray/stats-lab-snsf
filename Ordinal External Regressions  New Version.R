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

rm(applications,reviews,referee_grades, test)


# Get the Regression data
external_regression_data<-prepare_data_external_log_regression(final.apps,final.external)

external_regression_data$logAmount<-log(external_regression_data$AmountRequested)

# Select the final variables

data<- subset(external_regression_data,select = -c(ProjectID, OverallGrade, AmountRequested, IsApproved,
                                                   ScientificRelevance, Suitability))

str(data)

# Fitting the Model for the Combined Proposal-------------------------------------------------------
## Run an Ordinal Logistic Regression with the ordinal package

    # Visualization----
      ggplot(external_regression_data, aes(x = ProposalCombined, y = PercentFemale, col=Gender)) +
        geom_jitter(alpha = .5) +
        #facet_grid(InstType ~ Division, margins = TRUE) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    
    
    # fit a full model----
      fit1 <- clm(ProposalCombined ~Gender*(Division+PercentFemale)+Age+Division+IsContinuation+
                    PreviousRequest+InstType+Semester+logAmount,
                  data=data)
    # fit a null Model
      fit.null<- clm(ProposalCombined~1, data=data)
      diff.df<-fit.null$df.residual- fit1$df.residual
      
      LR<-(-2*(fit.null$logLik-fit1$logLik))
      1-pchisq(LR,df=diff.df) # 0   I am nto sure this is a good thing
      
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
      
    # Select Variables -----
    
      drop1(fit1, test = "Chi")
      # Gender:Division is the first candidate to leave out, then Semester, PreviousRequest and Age
      
      fit1<-update(fit1,.~.-Gender:Division)
      fit1<-update(fit1,.~.-Semester)
      fit1<-update(fit1,.~.-PreviousRequest)
      fit1<-update(fit1,.~.-Age)
      
      
      summary(fit1)
      drop1(fit1, test = "Chi")
      
      # After variable Selection, the final model
      Model<- clm(ProposalCombined ~ Gender + Division + PercentFemale + IsContinuation + 
                    InstType + logAmount + Gender:PercentFemale,data=data)
      summary(Model) #cond.H still high
      
      table(data$ProposalCombined)
      
      drop1(Model, test = "Chi")
      # Cannot be imporved
      
      confint(Model, type = "Wald")
      
      # Odds ratio and confidence intervals for Odds Ratio
      round(exp(Model$beta), 1)
      round(exp(confint(Model, type = "Wald")), 1) # There are some huge intervals
      
      # Use deviance as goodness of fit??
      
      # Effect plots, we should explore other ways to plot this. This graphs are not very
      # Informative
      plot(Effect("Gender", mod=Model))    # There most be a better way of plotting this
      plot(Effect("logAmount", mod=Model)) # The  higher the amount the likely it is to have a 
                                           # higher grade.
                                            
      plot(Effect("Division", mod=Model))  
      plot(Effect("InstType", mod=Model))
      plot(Effect("IsContinuation",mod=Model))
      
    # Variable Importance----
      # Not sure which meassure to use here. I'll try with LogLikelihood, I don't know
      # how to estimate the Deviance for this models. Any one knows?

      outcomeName <- 'ProposalCombined'
      predictorNames <- c('Gender', 'Division', 'PercentFemale','IsContinuation',
                            'InstType', 'logAmount')
      predictions <- predict(object=Model, data[,predictorNames], type="class")
        # A quick check
          table(data$ProposalCombined,predictions$fit)
          #Still classifies in mostly in category 4
          sum(predictions$fit=="4") # 1530 out of 1623 ???

      # Initialize vectors for randomization
      ll.Reference<-Model$logLik
      # If we shuffle and refit. If we have a smaller logLike -> the model is worst?
      shuffletimes <- 100  #number of interactions

      featuresMeanLR <- featuresProportions<-c()
      for (feature in predictorNames) {
        featureLl <- c()
        shuffledData <- data[,c(outcomeName, predictorNames)]
        for (iter in 1:shuffletimes) {
          shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
          Model.tmp <- update(Model,.~.,data=shuffledData)
          featureLl <- c(featureLl,Model.tmp$logLik)
        }
        featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
        featuresProportions<-c(featuresProportions,mean(ll.Reference>featureLl))
        # IF the new likelihood is smaller than the ref. -> we have a worst model
        # In how many runs does the likelihood is less
      }

       PseudoRShuffle <- data.frame('feature'=predictorNames, 'Average.Diff'=featuresMeanLR,
                                    'Proportion'=featuresProportions)
       PseudoRShuffle <- PseudoRShuffle[order(PseudoRShuffle$'Average.Diff', decreasing=TRUE),]
       print(PseudoRShuffle)
       
       #doing it like this, it turns out that all the variables are important. But this is to be expected
       # As we have previously select the variables.
       # Just to play around I will do the same, but with the original model and see if we get
       # the same result
       
       predictorNames2 <- c('Gender', 'Division', 'PercentFemale', 'Age','IsContinuation',
                           'PreviousRequest','InstType', 'Semester','logAmount')
       ll.Reference<-fit1$logLik
       # If we shuffle and refit. If we have a smaller logLike -> the model is worst?
      
       shuffletimes <- 100  #number of interactions
       
       featuresMeanLR <- featuresProportions<-c()
       for (feature in predictorNames2) {
         featureLl <- c()
         shuffledData <- data[,c(outcomeName, predictorNames2)]
         for (iter in 1:shuffletimes) {
           shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
           Model.tmp <- update(fit1,.~.,data=shuffledData)
           featureLl <- c(featureLl,Model.tmp$logLik)
         }
         featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
         featuresProportions<-c(featuresProportions,mean(ll.Reference>featureLl))
         # IF the new likelihood is smaller than the ref. -> we have a worst model
         # In how many runs does the likelihood is less
       }
       
       PseudoRShuffle2 <- data.frame('feature'=predictorNames2, 'Average.Diff'=featuresMeanLR,
                                    'Proportion'=featuresProportions)
       PseudoRShuffle2 <- PseudoRShuffle2[order(PseudoRShuffle2$'Average.Diff', decreasing=TRUE),]
       print(PseudoRShuffle2)
       
       # We do get the same variables as important. The one with the largest effect is Amount Requested.
       # Interesting. Remeber we are evaluating Project Assessment
    
       
  

  # Fitting the Model for ApplicantTrack  ---- 
      
      fit2 <- clm(ApplicantTrack ~Gender*(Division+PercentFemale)+Age+Division+IsContinuation+
                    PreviousRequest+InstType+Semester+logAmount,
                  data=data)
      
      summary(fit2)  # cond.H very high 
      # Variable Selection ----      
            
            drop1(fit2, test = "Chi")
            # Gender:Division is the first candidate to leave out, then Semester, PreviousRequest and Age
            
            fit2<-update(fit2,.~.-Age)
            summary(fit2)
            drop1(fit2, test = "Chi")
            
      # After variable Selection, the final model ----
            Model<- clm(ApplicantTrack ~ Gender + Division + PercentFemale + IsContinuation + 
                          InstType + logAmount + Gender:PercentFemale,data=data)
            summary(Model) #cond.H still high
            
            # fit a null Model
            fit.null<- clm(ApplicantTrack~1, data=data)
            fit.null$df.residual- Model$df.residual
            
            LR<-fit.null$logLik/Model$logLik
            1-pchisq(LR,df=10)
            
            prop.table(table(data$ApplicantTrack,data$Gender),2)
            
            drop1(Model, test = "Chi")
            # Cannot be imporved
            
            confint(Model, type = "Wald")
            
            # Odds ratio and confidence intervals for Odds Ratio
            round(exp(Model$beta), 1)
            round(exp(confint(Model, type = "Wald")), 1) # There are some huge intervals
  
  # Variable Importance----
            # Not sure which meassure to use here. I'll try with LogLikelihood, I don't know
            # how to estimate the Deviance for this models. Any one knows?
            
            outcomeName <- 'ApplicantTrack'
            predictorNames <- c('Gender', 'Division', 'PercentFemale','IsContinuation',
                                'InstType', 'logAmount')
            predictions <- predict(object=Model, data[,predictorNames], type="class")
            # A quick check
            table(data$ProposalCombined,predictions$fit)
            #Still classifies in mostly in category 4
            sum(predictions$fit=="4") # 1530 out of 1623 ???
            
            # Initialize vectors for randomization
            ll.Reference<-Model$logLik
            # If we shuffle and refit. If we have a smaller logLike -> the model is worst?
            shuffletimes <- 100  #number of interactions
            
            featuresMeanLR <- featuresProportions<-c()
            for (feature in predictorNames) {
              featureLl <- c()
              shuffledData <- data[,c(outcomeName, predictorNames)]
              for (iter in 1:shuffletimes) {
                shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
                Model.tmp <- update(Model,.~.,data=shuffledData)
                featureLl <- c(featureLl,Model.tmp$logLik)
              }
              featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
              featuresProportions<-c(featuresProportions,mean(ll.Reference>featureLl))
              # IF the new likelihood is smaller than the ref. -> we have a worst model
              # In how many runs does the likelihood is less
            }
            
            PseudoRShuffle <- data.frame('feature'=predictorNames, 'Average.Diff'=featuresMeanLR,
                                         'Proportion'=featuresProportions)
            PseudoRShuffle <- PseudoRShuffle[order(PseudoRShuffle$'Average.Diff', decreasing=TRUE),]
            print(PseudoRShuffle)
            
            #doing it like this, it turns out that all the variables are important. But this is to be expected
            # As we have previously select the variables.
            # Just to play around I will do the same, but with the original model and see if we get
            # the same result
            
            predictorNames2 <- c('Gender', 'Division', 'PercentFemale', 'Age','IsContinuation',
                                 'PreviousRequest','InstType', 'Semester','logAmount')
            ll.Reference<-fit2$logLik
            # If we shuffle and refit. If we have a smaller logLike -> the model is worst?
            
            shuffletimes <- 100  #number of interactions
            
            featuresMeanLR <- featuresProportions<-c()
            for (feature in predictorNames2) {
              featureLl <- c()
              shuffledData <- data[,c(outcomeName, predictorNames2)]
              for (iter in 1:shuffletimes) {
                shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
                Model.tmp <- update(fit2,.~.,data=shuffledData)
                featureLl <- c(featureLl,Model.tmp$logLik)
              }
              featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
              featuresProportions<-c(featuresProportions,mean(ll.Reference>featureLl))
              # IF the new likelihood is smaller than the ref. -> we have a worst model
              # In how many runs does the likelihood is less
            }
            
            PseudoRShuffle2 <- data.frame('feature'=predictorNames2, 'Average.Diff'=featuresMeanLR,
                                          'Proportion'=featuresProportions)
            PseudoRShuffle2 <- PseudoRShuffle2[order(PseudoRShuffle2$'Average.Diff', decreasing=TRUE),]
            print(PseudoRShuffle2)
            
            # We do get the same variables as important. The one with the largest effect is Amount Requested.
            # Interesting. Remeber we are evaluating Project Assessment
            
            
            
            
            
      # Effects ----      
            # Effect plots, we should explore other ways to plot this. This graphs are not very
            # Informative
            plot(Effect("Gender", mod=Model))    # There most be a better way of plotting this
            plot(Effect("logAmount", mod=Model)) # The  higher the amount the likely it is to have a 
            # higher grade.
            
            plot(Effect("Division", mod=Model))  
            plot(Effect("InstType", mod=Model))
            plot(Effect("IsContinuation",mod=Model))
            
         
            
# Fitting OverallGrade~ApplicantTrack + ProposalCombine + All
            
    data<- subset(external_regression_data,select = -c(ProjectID, AmountRequested, IsApproved,
                                                               ScientificRelevance, Suitability))      
    
    fit3 <- clm(OverallGrade ~Gender*(Division+PercentFemale)+ ApplicantTrack+ProposalCombined+Age+Division+IsContinuation+
                          PreviousRequest+InstType+Semester+logAmount,
                        data=data)
    
    summary(fit3)
    
    # Select Variables -----
    
    drop1(fit3, test = "Chi")
    # Gender:Division is the first candidate to leave out, then Semester, PreviousRequest and Age
    
    fit3<-update(fit3,.~.-logAmount)
    summary(fit3)
    drop1(fit3, test = "Chi")
    
    Model3<-clm(OverallGrade ~ ApplicantTrack + ProposalCombined , data=data)
    summary(Model3)
    
    #Odd Ratios
    round(exp(Model3$beta), 2)  # The odd ratio for proposal combined is Higher. 
    