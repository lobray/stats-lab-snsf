
#################################################################
###########  Regression Analysis - Board reviews  ###############
#################################################################

library(vcd) 
library(corrplot)
library(caTools)
library(ROCR)
library(pROC)
library(car)

### Initialize data

load("/home/leslie/Desktop/StatsLab/snsf_data.RData")
source("/home/leslie/Desktop/StatsLab/stats-lab-snsf/Cleaning Functions.R")
source("/home/leslie/Desktop/StatsLab/stats-lab-snsf/Data for Regression.R")

board_data <- prepare_data_board_log_regression(final.apps, internal = final.internal, external = final.external)

board_log_regression <- glm(board_data$IsApproved ~ Gender + Division + Age + AmountRequested + 
                              Ranking + OverallGrade + Gender:Division, family="binomial", data = board_data)

board_log_regression2 <- glm(board_data$IsApproved ~ Gender + Division + Age + AmountRequested + 
                            OverallGrade + Gender:Division, family="binomial", data = board_data)

board_log_regression1 <- glm(board_data$IsApproved ~ Gender + Division + Age + AmountRequested + 
                              Ranking + Gender:Division, family="binomial", data = board_data)

board_log_regression4 <- glm(board_data$IsApproved ~ Gender + Ranking + OverallGrade, family="binomial", data = board_data)



1-pchisq(board_log_regression1$dev-board_log_regression$dev, df=(board_log_regression1$df.res-board_log_regression$df.res))

anova(board_log_regression4, board_log_regression, test="Chisq")

summary(board_log_regression)

assess_board_model<- function(data=board_data, Div="All",
                      SplitRatio=0.8, cutoff=0.5 ){ 
  
  if (Div == "All"){ 
    final.data <- data
  }
  else {
    final.data<- subset(data,Division==Div, select = -(Division)) 
  }
  
  ### spliting the data into train and test set
  
  if (SplitRatio<1){
    split<-sample.split(final.data$IsApproved, SplitRatio = SplitRatio)
    Train<-subset(final.data, split=="TRUE")
    Test <-subset(final.data, split=="FALSE") 
  } else {
    Test<-Train<-final.data
  }
  
  #### fitting the model
  
  # Cutoff
  cutoff <- cutoff
  
  # Optimize the model
  
  Model <- glm(Train$IsApproved ~ .-(ProjectID),data=Train, 
               family="binomial")
  
  ### Testing the Treshold
  par(mfrow=c(1,2))
  predictor<-predict(Model, Test, type="response")
  ROCRPred<- prediction(predictor,Test$IsApproved)
  ROCRPerf<- performance(ROCRPred,"tpr","fpr")
  plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))
  
  ### with respect to accuracy
  ROCRACC<- performance(ROCRPred,"acc")
  plot(ROCRACC)  
  
  ### Confusion Matrices
  AccTable<-table(ActualValue=Test$IsApproved,Prediction=predictor>=cutoff)
  accuracy<-(sum(diag(AccTable))/sum(AccTable))
  
  ### Return
  print(paste("Regresion for External Reviews.   ", "Division: ", Div))
  return(list(Model= summary(Model), 
              #`Confidence Intervals`=confint(Model),
              `Confusion Matrix`=AccTable,
              `Percentage of data used for Training`=paste(SplitRatio*100,"%"),
              `Accuracy`=paste(round(accuracy,2)*100,"%")))
}


board.all <- assess_board_model(data=board_data, Div="All",
                        SplitRatio=1,cutoff=0.5)

