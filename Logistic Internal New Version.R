setwd("~/StatLab")
load("snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

# Libraries
library(effects)    
library(biostatUZH) 
library(psy)        
library(psych)      

rm(applications,reviews,referee_grades, test)

#################################################################################
#####    Logistic regression IsApproved (ORDERED grades on 6 levels)        #####
#################################################################################

## Prepare the data:

  internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
  # internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))
  internal_regression_data$logAmount <- log(internal_regression_data$AmountRequested)
  data<- subset(internal_regression_data,select = -c(ProjectID, Ranking, AmountRequested))

## Data visualization:

  # Visualization of Approval vs ApplicantTrack by internal referees
  barplot(table(data$IsApproved,data$ApplicantTrack),col=c("red","green"), beside=TRUE,
        main = "Applicant Track Grade vs. Approval")
  legend("topleft",bty="n", legend=c("Not Approved","Approved"),lty=2,col=c("red","green"))

  # Visualization of Approval vs ProjectAssessment by internal referees
  barplot(table(data$IsApproved,data$ProjectAssessment),col=c("red","green"), beside=TRUE,
        main = "Applicant Track Grade vs. Project")
  legend("topleft",bty="n", legend=c("Not Approved","Approved"),lty=2,col=c("red","green"))

## Fit the model:

  Model <- glm(IsApproved ~. +Gender:Division+Gender:PercentFemale+Gender:ApplicantTrack,
             data=data, family="binomial")
  summary(Model)
  # There is one NA for the interaction Gender:ApplicantTrack^5 and SE are huge

## Model Diagnostic:

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

  # Tuckey-Anscombe plot
  xx <- fvl
  yy <- residuals(Model, type="deviance")
  plot(xx, yy, pch=20, main="Tukey-Anscombe Plot")
  lines(loess.smooth(xx, yy, family="gaussian"), col="red")
  abline(h=0, lty=3, col="grey")

  # Both plots don't look good and this confirm that the Model we fitted is not optimal
  # as we already noticed from the standard errors size


## Kappa Statistic:

  linear.weights <- matrix(c(1, 0.8, 0.8, 1), nrow=2, ncol=2, byrow=T)
  kappa.matrix <- as.matrix(table(data$IsApproved,pred>=0.5))
  kappa <- cohen.kappa(kappa.matrix, w=linear.weights)
  kappa  

  # 0.73: substantial agreement between the predicted response and the real IsApproved

## Pseudos-R^2:

  (1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623)) 

  #[1] 0.6963247

## optimize model see if we can drop some variables:

  drop1(Model, test="Chisq")
  Model <- update(Model, .~.-InstType)
  Model <- update(Model, .~.-Gender:Division)
  Model<-update(Model,.~.-ApplicantTrack:Gender)      
  Model <- update(Model, .~.-PreviousRequest)
  Model <- update(Model, .~.-Gender:PercentFemale)

  # We stop here, before removing Gender
  summary(Model)
  
  # No more NAs but still huge p-values

## Pseudos-R^2:

  (1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))
  
  #[1] 0.6926307, only a bit smaller

## The model is not good, probably because of a perfect separation problem:

  with(data,table(IsApproved,ApplicantTrack))
  with(data,table(IsApproved,ProjectAssessment))


#################################################################################
#### Logistic regression IsApproved (ORDERED grades SIMPLIFIED on 4 levels)  ####
#################################################################################


## Prepare the data:

  internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
  # internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))
  internal_regression_data$logAmount <- log(internal_regression_data$AmountRequested)
  data<- subset(internal_regression_data,select = -c(ProjectID, Ranking, AmountRequested))

## Consider grades as ordered factors and simplify them:

  data$ApplicantTrack <- ifelse(data$ApplicantTrack<4,3,data$ApplicantTrack)
  data$ApplicantTrack <- as.ordered(factor(data$ApplicantTrack))
  data$ProjectAssessment <- ifelse(data$ProjectAssessment<4,3,data$ProjectAssessment)
  data$ProjectAssessment <- as.ordered(factor(data$ProjectAssessment))

## Fit the model:

  Model <- glm(IsApproved ~ Gender + Division + Age + Semester + IsContinuation + 
                 PercentFemale + ApplicantTrack + ProjectAssessment + logAmount,
               data=data, family="binomial")
  summary(Model)

  # There are no NAs and now the SE are ok

## Model Diagnostic:

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

  # Tuckey-Anscombe plot
  xx <- fvl
  yy <- residuals(Model, type="deviance")
  plot(xx, yy, pch=20, main="Tukey-Anscombe Plot")
  lines(loess.smooth(xx, yy, family="gaussian"), col="red")
  abline(h=0, lty=3, col="grey")


## Kappa Statistic:

  linear.weights <- matrix(c(1, 0.8, 0.8, 1), nrow=2, ncol=2, byrow=T)
  kappa.matrix <- as.matrix(table(data$IsApproved,pred>=0.5))
  kappa <- cohen.kappa(kappa.matrix, w=linear.weights)
  kappa

  # 0.74: substancial agreement between predicted response and IsApproved

## Pseudos-R^2:

  (1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))

  #[1] 0.6870244, only 0.01 smaller than using 6 levels

## Model optimization:

  drop1(Model, test="Chisq")
  Model <- update(Model, .~.-ApplicantTrack:Gender)
  
  # The model cannot be improved (I should remove Gender)

## Confidence Intervals:
  confint(Model, type="Wald")


## Effect Visualization:

  eff.fit <- allEffects(Model)
  
  # Gender effect:
  plot(Effect(focal.predictors = c("Gender"), Model),
       rug=FALSE,style="stacked")
  
  # All effects:
  plot(eff.fit)
  
  plot(Effect(c("Gender","ApplicantTrack"), mod=Model))
  plot(Effect(c("Gender","ProjectAssessment"), mod=Model))
  plot(Effect(c("Gender","Division"), mod=Model))
  

## Relative importance:
  
  # Reference Score
  calc_pseudo_r <- function(Model,n) {
    (1-exp((Model$dev-Model$null)/n))/(1-exp(-Model$null/n))
  }
  
  # Set Predictors and output variables
  outcomeName <- 'IsApproved'
  predictorNames <- setdiff(c('ApplicantTrack','ProjectAssessment', 'logAmount',
                              'PercentFemale','Age','Gender','Division',
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

  # The most important variable is ProjectAssessment
  # Then ApplicantTrack and IsContinuation (and PercentFemale)
  # logAmount and Gender are the least important (Good!)

