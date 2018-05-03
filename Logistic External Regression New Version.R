#####################################################################
######      Regressions for external reviews - version 2      #######
#####################################################################

# Initialize data
load("snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

library(biostatUZH)
library(psy)
library(psych)
library(caret)

rm(applications,reviews,referee_grades, test)

# Obtain Regression Data
external_regression_data<-prepare_data_external_log_regression(final.apps,final.external)

str(external_regression_data)
summary(external_regression_data)


# Logistic Regression -----------------------------------------------------


external_regression_data$logAmount<-log(external_regression_data$AmountRequested)
data<- subset(external_regression_data,select = -c(ProjectID, OverallGrade, AmountRequested))

barplot(table(data$IsApproved,data$ApplicantTrack),col=c("red","green"), beside=TRUE,
        main = "Applicant Track Grade vs. Approval")

legend("topleft",bty="n", legend=c("Not Approved","Approved"),lty=2,col=c("red","green"))

# As we define the grades as ordered factors, contrast.poly() is used as default to compute the
# coefficient estimates

# Fit first model with all variables and interactions
  fit <- glm(IsApproved ~ .+Gender:Division+
               Gender:PercentFemale+Gender:ApplicantTrack,data=data, 
             family="binomial")
  summary(fit)
  # We have NA, and coefficients with large variances. A model diagnostic will help solve this 
  # Issue

# Model Diagnostic
    # Predicted values
      pred<- predict(fit, data=data, type = "response")
      fvl <- predict(fit, type="link")
      IsApproved<-data$IsApproved

    # Residual plot
      plot(fvl, IsApproved, type="n", xlab="linear predictor",
           ylab="Approval Result")
      points(fvl[IsApproved==0], IsApproved[IsApproved==0])
      points(fvl[IsApproved==1], IsApproved[IsApproved==1], col="red")
      lines(sort(fvl+1), sort(pred+1), lty=3)
      title("Result vs. Linear Predictor")  
      
    # Kappa Statistic
      # Define weight matrix
        linear.weights <- matrix(c(1, 0.8, 0.8, 1), 
                               nrow=2, ncol=2, byrow=T)
      
        kappa.matrix <- as.matrix(table(data$IsApproved,pred>=0.5))
        kappa <- cohen.kappa(kappa.matrix, w=linear.weights)  # 0.5
        kappa
    # Pseudos-R^2
        (PsR22<-(1-exp((fit$dev-fit$null)/1623))/(1-exp(-fit$null/1623))) #[1] 0.4198219
    
    
    # optimize model see if we can drop some variables
      drop1(Model, test="Chisq")
      
      # Removing interaction between ApplicantTrack and gender reduces the AIC the most
      # I will remove this variable
        Model<-update(Model,.~.-ApplicantTrack:Gender) 
        Model<-update(Model,.~.-Division:Gender)   
        Model<-update(Model,.~.-InstType)
        Model<-update(Model,.~.-logAmount)
        Model<-update(Model,.~.-PercentFemale:Gender)
        Model<-update(Model,.~.-PreviousRequest)
        summary(Model)
        # We stop before removing Gender
        # No more NA, but still very large variances
        
        # Pseudos-R^2
        (PsR22<-(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))) #[1]  0.4165827
        # Although AIC is less, the pseudo r's are worst.
        Model$formula
        
# No matter what assumption of contrast we use the model is not good. I am guessing 
# this is because we have a perfect separation problem:
        
        with(data,table(IsApproved,ApplicantTrack))

# I will combine applicant track in levels ">=3","4","5","6"
        data$ApplicantTrack<-ifelse(data$ApplicantTrack<=3,3,data$ApplicantTrack)
        data$ApplicantTrack<-factor(data$ApplicantTrack, ordered=TRUE)
        
# For ScientificRelevance
        with(data,table(IsApproved,ScientificRelevance)) # Same Thing
        data$ScientificRelevance<-ifelse(data$ScientificRelevance<=3,3,data$ScientificRelevance)
        data$ScientificRelevance<-factor(data$ScientificRelevance, ordered=TRUE)
        
# For Suitability
        with(data,table(IsApproved,Suitability)) # Same Thing
        data$Suitability<-ifelse(data$Suitability<=3,3,data$Suitability)
        data$Suitability<-factor(data$Suitability, ordered=TRUE)
        
# Fit again with new definition of variables:
        
        Model <- glm(IsApproved ~ ApplicantTrack + ScientificRelevance + 
                       Suitability + PercentFemale + Age + Gender*Division + InstType*Division + IsContinuation + 
                       Semester,data=data, 
                   family="binomial")
        summary(Model)
        # We have no NA's now, and the variance are reasonable
        # The problem seems to be fixed
        
        # Model Diagnostic
        # Predicted values
        pred<- predict(Model, data=data, type = "response")
        fvl <- predict(Model, type="link")
        IsApproved<-data$IsApproved
        
        # Residual plot
        plot(fvl, IsApproved, type="n", xlab="linear predictor",
             ylab="Approval Result")
        points(fvl[IsApproved==0], IsApproved[IsApproved==0])
        points(fvl[IsApproved==1], IsApproved[IsApproved==1], col="red")
        lines(sort(fvl+1), sort(pred+1), lty=3)
        title("Result vs. Linear Predictor")  
        
        # Kappa Statistic
        # Define weight matrix
        linear.weights <- matrix(c(1, 0.8, 0.8, 1), 
                                 nrow=2, ncol=2, byrow=T)
        
        kappa.matrix <- as.matrix(table(data$IsApproved,pred>=0.5))
        kappa <- cohen.kappa(kappa.matrix, w=linear.weights) 
        kappa  # again 0.51
        
        # Pseudos-R^2
        (1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623)) #[1] 0.412507
 
        # optimize model
        drop1(Model, test="Chisq")
        Model <- update(Model, .~.-Age)
        # Model <- update(Model, .~.-Suitability)
        # Again, Removing interaction between ApplicantTrack and gender reduces the AIC the most
        # I will remove this variable
        

        summary(Model)
        # Pseudos-R^2
        1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623)) #[1] 0.4142912
        # Although AIC is less, the pseudo r's are worst. But is to be expected, as the more
        # Variables the less the deviance, and therfore, the less proportion explained
      
        # In the following I will work with this final model
        Model$call
        # glm(formula = IsApproved ~ ApplicantTrack + ScientificRelevance + Suitability + 
        # PercentFemale + Age + Gender + Division + IsContinuation + 
        #  Semester, 
        # family = "binomial", data = data)
       
# Effect Visualization ----------------------------------------------------

library(effects)
library(car)        
library(MASS)        
library(splines)
library(ggplot2)

# Obtain all the effects        
  eff.fit <- allEffects(Model)
  
# Obtain Gender effect as a separate data frame        
  GendDivEff<-as.data.frame(eff.fit[[6]])               

# Plot different things
  #Global Gender effect on the approval of applications  
  plot(Effect(focal.predictors = c("Gender"), Model),
     rug=FALSE,style="stacked")
  
  # Effect of gender in approval, per division
    plot(predictorEffects(Model, ~ Gender))
    # Lines are almost horizotal, an indication of no gender effect. Good news!!
  # All effects at once
    plot(eff.fit)


# Visualization with effects package
  plot(Effect(c("Gender","ApplicantTrack"), mod=Model))
  plot(Effect(c("Gender","ScientificRelevance"), mod=Model))
  plot(Effect(c("Gender","Suitability"), mod=Model))

# Plots Gender effect accross divisions 
# another way, still not good Anz ideas to make plots look nicer?

# ggplot(GendDivEff[,which(GendDivEff$Division=="Div 1")],aes(x=Gender, y=fit))+
#  geom_line()

# ggplot(GendDivEff, aes(x=Gender, y=fit)) + 
#  geom_segment(data=GendDivEff, mapping=aes(x=Gender, y=lower, xend=Gender, yend=upper, colour=Division)) 


# Relative variable importance --------------------------------------------

# Reference Score
calc_pseudo_r <- function(Model,n) {
  (1-exp((Model$dev-Model$null)/n))/(1-exp(-Model$null/n))
}

# Set Predictors and output variables
  outcomeName <- 'IsApproved'
  predictorNames <- setdiff(c('ApplicantTrack','ScientificRelevance', 'Suitability',
                              'PercentFemale','Age','Gender','Division','InstType',
                              'IsContinuation', 'Semester'),outcomeName)

# To see variable importance based on pseudo r^2
  refR2<-calc_pseudo_r(Model,nrow(data))
  PseudoRShuffle <- NULL
  shuffletimes <- 50  #number of interactions
  
  featuresMeanR2 <- c()
  for (feature in predictorNames) {
    featureR2 <- c()
    shuffledData <- data
    for (iter in 1:shuffletimes) {
      shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
      Model.tmp <- update(Model,.~.,data=shuffledData)
      featureR2 <- c(featureR2,calc_pseudo_r(Model.tmp,nrow(shuffledData)))
    }
    featuresMeanR2 <- c(featuresMeanR2, mean(featureR2-refR2))
  }  
  
  PseudoRShuffle <- data.frame('feature'=predictorNames, 'importance'=featuresMeanR2)
  PseudoRShuffle <- PseudoRShuffle[order(PseudoRShuffle$importance, decreasing=TRUE),]
  print(PseudoRShuffle)
