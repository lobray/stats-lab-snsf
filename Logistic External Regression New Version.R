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
        # 1
          (PsR21<-1-fit$dev/fit$null)  #[1] 0.2730928
        # 2
          (PsR22<-(1-exp((fit$dev-fit$null)/1623))/(1-exp(-fit$null/1623))) #[1] 0.4198219
    
    
    # optimize model see if we can drop some variables
      drop1(fit, test="Chisq")
      
      # Removing interaction between ApplicantTrack and gender reduces the AIC the most
      # I will remove this variable
      
        Model<-update(fit,.~.-ApplicantTrack:Gender)      
        summary(Model)
        # No more NA, but still very large variances
        
        # Pseudos-R^2
        # 1
        (PsR21<-1-Model$dev/Model$null)  #[1] 0.2719115
        # 2
        (PsR22<-(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))) #[1] 0.4183261
        # Although AIC is less, the pseudo r's are worst.
        
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
        
        Model <- glm(IsApproved ~ .+Gender:Division+
                     Gender:PercentFemale+Gender:ApplicantTrack,data=data, 
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
        kappa  # again 0.5
        
        # Pseudos-R^2
        # 1
        (Ps2R21<-1-Model$dev/Model$null)  #0.2697538
        (PsR21-Ps2R21)  # [1] 0.00215778 We have a sligthly worst proportion of deviance explained
        # 2
        (Ps2R22<-(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))) #[1] 0.4198219
        (PsR22-Ps2R22)  #[1] 0.002738762
        
        # optimize model
        drop1(fit, test="Chisq")
        
        # Again, Removing interaction between ApplicantTrack and gender reduces the AIC the most
        # I will remove this variable
        
        Model<-update(Model,.~.-ApplicantTrack:Gender)      
        summary(Model)
        # Pseudos-R^2
        # 1
        (Ps2R21<-1-Model$dev/Model$null)  # [1] 0.2687348
        # 2
        (PsR22<-(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))) #[1] 0.4142912
        # Although AIC is less, the pseudo r's are worst. But is to be expected, as the more
        # Variables the less the deviance, and therfore, the less proportion explained
      
        # In the following I will work with this final model
        Model$call
        # glm(formula = IsApproved ~ ApplicantTrack + ScientificRelevance + 
        # Suitability + PercentFemale + Age + Gender + Division + IsContinuation + 
        # PreviousRequest + InstType + Semester + logAmount + Gender:Division, 
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
  GendDivEff<-as.data.frame(eff.fit[[11]])               

# Plot different things
  #Global Gender effect on the approval of applications  
  plot(Effect(focal.predictors = c("Gender"), Model),
     rug=FALSE,style="stacked")
  
  # Effect per Gender, on Is Approved by Division
    plot(eff.fit[[11]])
  
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
# ggplot(GendDivEff,aes(x=Gender, y=fit, col=factor(Division)))+
#   geom_line()
  

# Relative variable importance --------------------------------------------

# Reference Score
calc_pseudo_r <- function(Model,n) {
  (1-exp((Model$dev-Model$null)/n))/(1-exp(-Model$null/n))
}

# Area under the curve. For logistic regression this makes sense as a meassure for the variable 
# importance. Here a code to calculate it taken from 

GetROC_AUC = function(probs, true_Y){
  # AUC approximation
  # http://stackoverflow.com/questions/4903092/calculate-auc-in-r
  # ty AGS
  probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
  val = unlist(probsSort$x)
  idx = unlist(probsSort$ix) 
  
  roc_y = true_Y[idx];
  stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
  stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)   
  
  auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
  return(auc)
}

# Set Predictors and output variables
  outcomeName <- 'IsApproved'
  predictorNames <- setdiff(names(data),outcomeName)

# Predictions
  predictions <- predict(object=Model, data[,predictorNames], type="response")
  
# Reference AUC approximation
  refAUC <- GetROC_AUC(predictions,data[,outcomeName])
  print(paste('AUC score:', refAUC))
  
# Shuffle predictions for variable importance
  AUCShuffle <- NULL
  shuffletimes <- 100  #number of interactions
  
  featuresMeanAUCs <- c()
  for (feature in predictorNames) {
    featureAUCs <- c()
    shuffledData <- data[,predictorNames]
    for (iter in 1:shuffletimes) {
      shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
      predictions <- predict(object=Model, shuffledData[,predictorNames], type="response")
      featureAUCs <- c(featureAUCs,GetROC_AUC(predictions, data[,outcomeName]))
    }
    featuresMeanAUCs <- c(featuresMeanAUCs, mean(featureAUCs < refAUC))
  }  
  
  AUCShuffle <- data.frame('feature'=predictorNames, 'importance'=featuresMeanAUCs)
  AUCShuffle <- AUCShuffle[order(AUCShuffle$importance, decreasing=TRUE),]
  print(AUCShuffle)
  # Equally important are ApplicantTrack, Scientific Relevance, Suitability, Is continuation
  # Follow by InstType, Gender Division, and so on
  
  # This graph needs some work
  barplot(AUCShuffle[,2], names.arg = AUCShuffle[,1],horiz = TRUE, 
          col=rainbow(30),
          main="Relative Importance of the Variables")
  
# To see variable importance based on pseudo r^2
  refR2<-calc_pseudo_r(Model,nrow(data))
  PseudoRShuffle <- NULL
  shuffletimes <- 100  #number of interactions
  
  featuresMeanR2 <- c()
  for (feature in predictorNames) {
    featureR2 <- c()
    shuffledData <- data
    for (iter in 1:shuffletimes) {
      shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
      Model.tmp <- update(Model,.~.,data=shuffledData)
      featureR2 <- c(featureR2,calc_pseudo_r(Model.tmp,nrow(shuffledData)))
    }
    featuresMeanR2 <- c(featuresMeanR2, mean(featureR2 < refR2))
  }  
  
  PseudoRShuffle <- data.frame('feature'=predictorNames, 'importance'=featuresMeanR2)
  PseudoRShuffle <- PseudoRShuffle[order(PseudoRShuffle$importance, decreasing=TRUE),]
  print(PseudoRShuffle)

  # To compare both methods
    Comp<- cbind(AUC=AUCShuffle[,2],PseudoR2=PseudoRShuffle[,2])
    row.names(Comp)<-AUCShuffle[,1]
    
    
# Last approach for looking at the variable importance, using caret::varImp
    library(caret)
    C.varImp<- data.frame(varImp(Model))
   
    
    
  