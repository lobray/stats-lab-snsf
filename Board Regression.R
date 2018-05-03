

#################################################################
###########  Regression Analysis - Board reviews  ###############
#################################################################

library(vcd) 
library(corrplot)
library(caTools)
library(ROCR)
library(pROC)
library(car)
library(caret)

### Initialize data

load("/home/leslie/Desktop/StatsLab/snsf_data.RData")
source("/home/leslie/Desktop/StatsLab/stats-lab-snsf/Cleaning Functions.R")
source("/home/leslie/Desktop/StatsLab/stats-lab-snsf/Data for Regression.R")


board_data <- prepare_data_board_log_regression(final.apps, internal = final.internal, external = final.external)



board_log_regression <- glm(board_data$IsApproved ~ Gender + Division + Age + IsContinuation + InstType + AmountRequested + 
                              Ranking + OverallGrade + Gender:Division, family="binomial", data = board_data)


### In fitting the regression, the summary shos that OverallGrade, Ranking, InstType and Age are all significant predictors.
summary(board_log_regression)

### In checking for correlation among coefficients, we only don't see any alarming values (VIF > 5). 
vif(board_log_regression)

### Check diagnostics: residuals
plot(resid(board_log_regression, type="pearson"))  # residuals appaer to be expectatino 0, with a few extreme outliers
plot(resid(board_log_regression, type="deviance")) # expectation 0, though it appears some structure, does this mean fit new model?


############ Test out other models, to do a comparision

board_log_regression2 <- glm(board_data$IsApproved ~ Gender + Division + Age + AmountRequested + 
                            OverallGrade + Gender:Division, family="binomial", data = board_data)

board_log_regression1 <- glm(board_data$IsApproved ~ Gender + Division + Age + AmountRequested + 
                              Ranking + Gender:Division, family="binomial", data = board_data)

board_log_regression4 <- glm(board_data$IsApproved ~ Gender + Ranking + OverallGrade, family="binomial", data = board_data)



1-pchisq(board_log_regression1$dev-board_log_regression$dev, df=(board_log_regression1$df.res-board_log_regression$df.res))

anova(board_log_regression4, board_log_regression, test="Chisq")

summary(board_log_regression)





#####################################################################
#################### CV to assess model fit ##########################
#####################################################################


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


permute_values <- function(data_vector) {
  n <- length(data_vector)
  permuted_vector <- data_vector
  permuted_matrix <- matrix(0, nrow=n, ncol=100000)
  for (i in 1:100000) {
    permuted_matrix[,i] <- sample(n, n, x=permuted_vector)
  }
  
  return(permuted_matrix)
}

# Function to calculate variable importance via permutation
calc_variable_importance <- function(predictors = predictors, data = board_data, regression_object) {
  variable_imp_matrix <- matrix(0, nrow=length(predictors), ncol=2)
  variable_imp_matrix[,1] <- predictors
  print(variable_imp_matrix)
  base_pseudo_r <- calc_pseudo_r(regression_object)
  
  for (i in predictors) {
    permuted_board_data <- data
    permuted_predictor <- permute_values(permuted_board_data[,i])
    pseudo_r <- c(length=ncol(permuted_predictor))
    
    
    for (j in 1:ncol(permuted_predictor)) {
      permuted_board_data[,j] <- permuted_predictor[,j]
      
      # run regression on permuted value
      permuted_board_log_regression <- glm(permuted_board_data$IsApproved ~ Gender + Division + Age +
                                             IsContinuation + InstType + AmountRequested +
                                             Ranking + OverallGrade + Gender:Division, family="binomial", data = permuted_board_data)
      # calculate pseudo r squared
      pseudo_r[j] <- calc_pseudo_r(permuted_board_log_regression)  
      
    }
    
    variable_imp_matrix[which(variable_imp_matrix[,1] == i),2] <- mean(permuted_pseudo_r) - (base_pseudo_r)
  } 
  colnames(variable_imp_matrix) <- c("Variable", "Difference to Base Pseudo R^2")
  return(variable_imp_matrix)
}


aa <- (calc_variable_importance(predictors=predictors, data=board_data, regression_object=board_log_regression))

Do CV to see if model fit is decent:
  ```{r}
#####################################################################
#################### CV to assess model fit ##########################
#####################################################################


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
board.all$Accuracy

```

