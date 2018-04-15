
#################################################################
###########  Regression Analysis - External reviews  ############
#################################################################

library(vcd)
library(corrplot)

# Initialize data
load("~/MSc Statistics/StatsLab/Analysis/SNFS Data/snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

rm(applications,reviews,referee_grades)

# Obtain Regression Data
external_regression_data<-prepare_data_external_log_regression(final.external)


# Regression function -----------------------------------------------------


ExternalReviewModel<- function(data=external_regression_data,Div="All",
                               SplitRatio=0.8, cutoff=0.5 ){ 
  data<- data[,-1]
  if (Div == "All"){ 
    final.data<- data
  }
  else {
    final.data<- subset(data,Division==Div, select = -(Division)) 
  }
  
  
  #spliting the data into train and test set
  
  if (SplitRatio<1){
    split<-sample.split(final.data$IsApproved, SplitRatio = SplitRatio)
    Train<-subset(final.data, split=="TRUE")
    Test <-subset(final.data, split=="FALSE") 
  } else {
    Test<-Train<-final.data
  }
  
  # fitting the model
  
    # Cutoff
      cutoff<-cutoff
  
  # Optimize the model
  
  # full<- glm(IsApproved ~ .-(ProjectID), data=Train, family="binomial")
  # Model  <-step(full, direction = "backward", trace=0)
  # Model <- step(empty,scope=list(lower=empty,upper=full), direction="both",
  #     trace=0)
  Model <- glm(Train$IsApproved ~ .,data=Train, 
               family="binomial")
  
  
  # Testing the Treshold
  par(mfrow=c(1,2))
  predictor<-predict(Model, Test, type="response")
  ROCRPred<- prediction(predictor,Test$IsApproved)
  ROCRPerf<- performance(ROCRPred,"tpr","fpr")
  plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))
  
  # with respect to accuracy
  ROCRACC<- performance(ROCRPred,"acc")
  plot(ROCRACC)  
  
  # Confusion Matrices
  AccTable<-table(ActualValue=Test$IsApproved,Prediction=predictor>=cutoff)
  accuracy<-(sum(diag(AccTable))/sum(AccTable))
  
  # Return
  print(paste("Regresion for External Reviews.   ", "Division: ", Div))
  par(mfrow=c(1,1))
  Res.data<-list(Train,Test)
  return(list(Regression=Model,
              Model= summary(Model), 
              `Confidence Intervals`=confint(Model),
              `Confusion Matrix`=AccTable,
              `Percentage of data used for Training`=paste(SplitRatio*100,"%"),
              Accuracy=paste(round(accuracy,2)*100,"%")))
  
  
}


# Fit the regression ------------------------------------------------------

# Regression

fit <- ExternalReviewModel(SplitRatio = 1) #it is not considering Overall Grade
fit   # There are still NA's in the conf. intervals

# Model analysis
  Model<-fit$Regression 
  
# Are covariates correlated
  vif(Model)  #seems ok
  # Model matrix, to spread factors into dummy variables
  X<-model.matrix(Model)
  X<-X[,-1]   # Eliminate the intercept
  Xc<-cor(X)   # Compute correlation matrix
  
  # Plot the correlation matrix  
  
  # correlation test
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(X,method="spearman")
  head(p.mat[, 1:5])
  
  corrplot(Xc, type="upper", order="hclust", 
           p.mat = p.mat, sig.level = 0.01,insig = "blank")
  
  
# Model Diagnostic

    fvl <- predict(Model, type="link")
    fpr <- predict(Model, type="response")
    IsApproved<-external_regression_data$IsApproved

# Residual plot
    plot(fvl, IsApproved, type="n", xlab="linear predictor",
          ylab="Approval Result")
    points(fvl[IsApproved==0], IsApproved[IsApproved==0])
    points(fvl[IsApproved==1], IsApproved[IsApproved==1], col="red")
    lines(sort(fvl+1), sort(fpr+1), lty=3)
    title("Result vs. Linear Predictor")   # Confirms what Leslie found

# there are some outliers
    id<-which(fvl<(-5))

# Ignore outliers to better visualize the residual plot
    #It looks fine to me, what do you think
    xx <- fvl[-id]
    yy <- residuals(Model, type="deviance")[-id]
    
    plot(xx, yy, pch=20, main="Tukey-Anscombe Plot")
    lines(loess.smooth(xx, yy, family="gaussian"), col="red")
    abline(h=0, lty=3, col="grey")

 # Analysing outliers
    obs<-external_regression_data[id,]
    obs<-obs$ProjectID
  
  #This are observations for which residuals are greater than 2 and need a closer look
  dat<-subset(apps, ProjectID%in%obs)
  dat<-merge(dat,external_reviews, by="ProjectID")
  
  # Tables 
  structable(~ Gender+Division+IsApproved, data = dat) # all rejected
  structable(~ InstType+PreviousRequest+IsContinuation,data=dat) # all Previous Request
  cotabplot(~Gender+GradeFinal, data=dat) # 
  
  
 #Inference
  #from the script "Applied Statistical Regression - AS 2017" from Dr. Marcel Dettling
  #coeficient of determination
   1-Model$dev/Model$null # ?? This does not look nice
   #[1] 0.0505608
   #or
   (1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))
   # [1] 0.09013418
   
  #optimize model
   drop1(Model, test="Chisq")
   #we can consider eliminating some variables
 

   
# Residuals: see if there are Pearson residuals greater than 2 (need a closer look)
   R<-residuals(Model, type = "pearson")
   idR<-which(abs(R)>2)
   R[idR]
   obs<-external_regression_data[idR,]
   obs<-obs$ProjectID
   
   #This are observations for which residuals are greater than 2 and need a closer look
   dat<-subset(apps, ProjectID%in%obs)
   dat<-merge(dat,external_reviews, by="ProjectID")
   
   
   structable(~ Gender+Division+IsApproved, data = dat) 
   structable(~ InstType+PreviousRequest+IsContinuation,data=dat) 
   cotabplot(~Gender+GradeFinal, data=dat) # All B adn B+
  
  # I do not see anithing special      
    
   