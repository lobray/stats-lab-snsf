source("Cleaning Functions.R")
source("Data for Regression.R")

library(vcd) 
library(corrplot)
library(caTools)
library(ROCR)
library(pROC)
library(car)

# Multinomial regression for internal reviews: ProjectAssessment

internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
internal_regression_data$ProjectAssessment <- factor(internal_regression_data$ProjectAssessment)

library(MASS)
fit <- polr(ProjectAssessment ~ Gender + Division + InstType, 
            method="logistic",data=internal_regression_data)
summary(fit)

fit2 <- polr(ProjectAssessment ~ Gender + Division + InstType + IsContinuation, 
             method="logistic",data=internal_regression_data)
summary(fit2)

deviance(fit)-deviance((fit2))
pchisq(65.50466, fit2$edf-fit$edf, lower=FALSE)
# [1] 5.797671e-16 -> include IsContinuation

fit3 <- polr(ProjectAssessment ~ Gender + Division + InstType + IsContinuation + 
               Age, method="logistic",data=internal_regression_data)
summary(fit3)
deviance(fit2)-deviance((fit3))
pchisq(2.412097, fit3$edf-fit2$edf, lower=FALSE)
# [1] 0.120401 -> don't include Age

fit4 <- polr(ProjectAssessment ~ Gender + Division + InstType + IsContinuation + 
               log(AmountRequested), method="logistic",data=internal_regression_data)
summary(fit4)

deviance(fit2)-deviance((fit4))
pchisq(41.69483, fit4$edf-fit2$edf, lower=FALSE)
# [1] 1.066907e-10 -> include log(AmountRequested)

fit5 <- polr(ProjectAssessment ~ Gender + Division + InstType + IsContinuation + 
               log(AmountRequested)+PercentFemale, method="logistic",data=internal_regression_data)
summary(fit5)

deviance(fit4)-deviance((fit5))
pchisq(7.92891, fit5$edf-fit4$edf, lower=FALSE)
# [1] .00486511 -> include PercentFemale

fit6 <- polr(ProjectAssessment ~ Gender + Division + InstType + IsContinuation + 
               log(AmountRequested)+PercentFemale+Gender:Division, 
             method="logistic",data=internal_regression_data)
summary(fit6)
deviance(fit5)-deviance((fit6))
pchisq(0.5522981, fit6$edf-fit5$edf, lower=FALSE)
# [1] 0.7586998 -> include Gender:Division ??

fit7 <- polr(ProjectAssessment ~ Gender + Division + InstType + IsContinuation + 
               log(AmountRequested)+PercentFemale+PreviousRequest, 
             method="logistic",data=internal_regression_data)
summary(fit7)
deviance(fit5)-deviance((fit7))
pchisq(1.237963, fit7$edf-fit5$edf, lower=FALSE)
# [1] 0.6363692 -> include Gender:PercentFemale ??


#spliting the data into train and test set
split<-sample.split(internal_regression_data$ProjectAssessment, SplitRatio = 0.8)
Train<-subset(internal_regression_data, split=="TRUE")
Test <-subset(internal_regression_data, split=="FALSE")


InternalMultiModel<- function(data=internal_regression_data, train=Train, 
                              test=Test,SplitRatio = 0.8){ 

  # fitting the model
  Model <- polr(ProjectAssessment ~ Gender+Division+InstType+IsContinuation+
                  log(AmountRequested)+PercentFemale+ApplicantTrack,
                  method="logistic", data=Train)
  
  
  # Testing the Treshold
  # par(mfrow=c(1,2))
  predictor<-predict(Model, Test, type="class")
  # Unfortunately ROCR only support evaluation of binary classification tasks
  # ROCRPred<- prediction(predictor,labels=Test$ProposalCombined)
  # ROCRPerf<- performance(ROCRPred,"tpr","fpr")
  # plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))
  
  # with respect to accuracy
  # ROCRACC<- performance(ROCRPred,"acc")
  # plot(ROCRACC)  
  
  # Confusion Matrices
  AccTable<-table(ActualValue=Test$ProjectAssessment,Prediction=predictor)
  accuracy<-(sum(diag(AccTable))/sum(AccTable))
  
  # Return
  print(paste("Regresion for Internal Reviews.   "))
  return(list(Model= summary(Model), 
              #`Confidence Intervals`=confint(Model),
              `Confusion Matrix`=AccTable,
              `Percentage of data used for Training`=paste(SplitRatio*100,"%"),
              Accuracy=paste(round(accuracy,2)*100,"%")))
  
  
}

Model_fitted <- InternalMultiModel()
Model_fitted
# Accuracy 32%
# Including ApplicantTrack accuracy 45%

##########################################################################################

# Multinomial regression for internal reviews: ApplicantTrack

internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
internal_regression_data$ApplicantTrack <- factor(internal_regression_data$ApplicantTrack)

library(MASS)
fit <- polr(ApplicantTrack ~ Gender + Division + InstType, 
            method="logistic",data=internal_regression_data)
summary(fit)

fit2 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation, 
             method="logistic",data=internal_regression_data)
summary(fit2)

deviance(fit)-deviance((fit2))
pchisq(31.11318, fit2$edf-fit$edf, lower=FALSE)
# include IsContinuation

fit3 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
               Age, method="logistic",data=internal_regression_data)
summary(fit3)
deviance(fit2)-deviance((fit3))
pchisq(1.207117, fit3$edf-fit2$edf, lower=FALSE)
# [1] 0.27 -> don't include Age

fit4 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
               log(AmountRequested), method="logistic",data=internal_regression_data)
summary(fit4)

deviance(fit2)-deviance((fit4))
pchisq(76.57975, fit4$edf-fit2$edf, lower=FALSE)
# -> include log(AmountRequested)

fit5 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
               log(AmountRequested)+PercentFemale, method="logistic",data=internal_regression_data)
summary(fit5)

deviance(fit4)-deviance((fit5))
pchisq(12.75694, fit5$edf-fit4$edf, lower=FALSE)
# [1] .0003 -> include PercentFemale

fit6 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
               log(AmountRequested)+PercentFemale+Gender:Division, 
             method="logistic",data=internal_regression_data)
summary(fit6)
deviance(fit5)-deviance((fit6))
pchisq(6.298236, fit6$edf-fit5$edf, lower=FALSE)
# [1] 0.04288994 -> include Gender:Division 

fit7 <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
               log(AmountRequested)+PercentFemale+Gender:Division+Gender:PercentFemale, 
             method="logistic",data=internal_regression_data)
summary(fit7)
deviance(fit6)-deviance((fit7))
pchisq(0.0304, fit7$edf-fit5$edf, lower=FALSE)
# [1] 0.6363692 -> don't include PreviousRequest


#spliting the data into train and test set
split<-sample.split(internal_regression_data$ApplicantTrack, SplitRatio = 0.8)
Train<-subset(internal_regression_data, split=="TRUE")
Test <-subset(internal_regression_data, split=="FALSE")


InternalMultiModel<- function(data=internal_regression_data, train=Train, 
                              test=Test,SplitRatio = 0.8){ 
  
  # fitting the model
  Model <- polr(ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
                  log(AmountRequested)+PercentFemale+Gender:Division+ProjectAssessment, 
                method="logistic",data=Train)
  
  
  # Testing the Treshold
  # par(mfrow=c(1,2))
  predictor<-predict(Model, Test, type="class")
  # Unfortunately ROCR only support evaluation of binary classification tasks
  # ROCRPred<- prediction(predictor,labels=Test$ProposalCombined)
  # ROCRPerf<- performance(ROCRPred,"tpr","fpr")
  # plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))
  
  # with respect to accuracy
  # ROCRACC<- performance(ROCRPred,"acc")
  # plot(ROCRACC)  
  
  # Confusion Matrices
  AccTable<-table(ActualValue=Test$ApplicantTrack,Prediction=predictor)
  accuracy<-(sum(diag(AccTable))/sum(AccTable))
  
  # Return
  print(paste("Regresion for Internal Reviews.   "))
  return(list(Model= summary(Model), 
              #`Confidence Intervals`=confint(Model),
              `Confusion Matrix`=AccTable,
              `Percentage of data used for Training`=paste(SplitRatio*100,"%"),
              Accuracy=paste(round(accuracy,2)*100,"%")))
  
  
}

Model_fitted <- InternalMultiModel()
Model_fitted
# Accuracy 46%
# Including ProjectAssessment accuracy 54%