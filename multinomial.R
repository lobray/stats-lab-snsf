source("Cleaning Functions.R")
source("Data for Regression.R")

library(vcd) 
library(corrplot)
library(caTools)
library(ROCR)
library(pROC)
library(car)

external_regression_data<-prepare_data_external_log_regression(final.apps,final.external)
external_regression_data$ProposalCombined <- round((external_regression_data$ScientificRelevance+
                                                      external_regression_data$Suitability)/2, 0)
external_regression_data$ProposalCombined <- factor(external_regression_data$ProposalCombined)

library(MASS)
fit <- polr(ProposalCombined ~ Gender + Division + InstType, 
            method="logistic",data=external_regression_data)
summary(fit)

fit2 <- polr(ProposalCombined ~ Gender + Division + InstType + IsContinuation, 
             method="logistic",data=external_regression_data)
summary(fit2)

deviance(fit)-deviance((fit2))
pchisq(40.60535, fit2$edf-fit$edf, lower=FALSE)
# [1] 1.862959e-10 -> include IsContinuation

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
# [1] 0.9370876 -> include Gender:Division

fit7 <- polr(ProposalCombined ~ Gender + Division + InstType + IsContinuation + 
               log(AmountRequested)+PercentFemale+Gender:Division+Gender:PercentFemale, 
             method="logistic",data=external_regression_data)
summary(fit7)
deviance(fit6)-deviance((fit7))
pchisq(4.800679, fit7$edf-fit6$edf, lower=FALSE)
# [1] 0.02844852 -> include Gender:PercentFemale




ExternalMultiModel<- function(data=external_regression_data,Div="All",
                               SplitRatio=0.8, cutoff=0.5){ 
  
  if (Div == "All"){ 
    final.data<- data
  }
  else {
    final.data<- subset(data,Division==Div, select = -(Division)) 
  }
  
  
  #spliting the data into train and test set
  
  if (SplitRatio<1){
    split<-sample.split(final.data$ProposalCombined, SplitRatio = 0.8)
    Train<-subset(final.data, split=="TRUE")
    Test <-subset(final.data, split=="FALSE") 
  } else {
    Test<-Train<-final.data
  }
  
  # fitting the model
  
  #ExternalModel<-function(Train,Test,cutoff){
  
  # Cutoff
  cutoff<-cutoff
  
  # Optimize the model
  
  # full<- glm(IsApproved ~ .-(ProjectID), data=Train, family="binomial")
  # Model  <-step(full, direction = "backward", trace=0)
  # Model <- step(empty,scope=list(lower=empty,upper=full), direction="both",
  #     trace=0)
  Model <- polr(ProposalCombined ~ Gender + Division + InstType + IsContinuation + 
                  log(AmountRequested)+PercentFemale+Gender:Division+Gender:PercentFemale, 
                method="logistic",data=Train)

  
  
  # Testing the Treshold
  par(mfrow=c(1,2))
  predictor<-predict(Model, Test, type="class")
  # Unfortunately ROCR only support evaluation of binary classification tasks
  # ROCRPred<- prediction(predictor,labels=Test$ProposalCombined)
  # ROCRPerf<- performance(ROCRPred,"tpr","fpr")
  # plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))
  
  # with respect to accuracy
  # ROCRACC<- performance(ROCRPred,"acc")
  # plot(ROCRACC)  
  
  # Confusion Matrices
  AccTable<-table(ActualValue=Test$ProposalCombined,Prediction=predictor)
  accuracy<-(sum(diag(AccTable))/sum(AccTable))
  
  # Return
  print(paste("Regresion for External Reviews.   ", "Division: ", Div))
  return(list(Model= summary(Model), 
              #`Confidence Intervals`=confint(Model),
              `Confusion Matrix`=AccTable,
              `Percentage of data used for Training`=paste(SplitRatio*100,"%"),
              Accuracy=paste(round(accuracy,2)*100,"%")))
  
  
}

Model_fitted <- ExternalMultiModel()
Model_fitted
