######################################################################♦
###########   Testing the Confidence intervals for gender     ########
######################################################################

# This file fits a general model with the external data
# estimates confidence intervals for the coefficients
# resamples several times the data with replacement
# For each sample, calculates Wald CI of gender and prooves if 0 is inside the CI


load("snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

library(caret)

# Get the Regression data
external_regression_data<-prepare_data_external_log_regression(final.apps,final.external)

data<-external_regression_data[,-1] # Take out Project ID
Model<- glm(IsApproved ~ .-OverallGrade+Gender:Division+Gender:PercentFemale+Gender:InstType,
            data=data, 
            family="binomial")


set.seed( 1)
ex.n <- 100       #  100 bootstraps 
alpha <- .05      #  95% confidence intervals

plot( 1:ex.n, 1:ex.n, type='n', ylim=c(-2.5,1.5), xaxt='n', 
      ylab="Confidence Intervals",
      xlab=paste("number of bootstraps = ", ex.n),
      main="CI for gender")
abline(h=0)
set.seed(77)
strap<- createResample(data[,'IsApproved'],times= ex.n, list=FALSE)
count<-c()
for ( i in 1:ex.n){
  id<-strap[,i]  
  B.Model<- glm(IsApproved ~ .-OverallGrade+Gender:Division+Gender:PercentFemale+Gender:InstType,
                  data=data[id,], 
                  family="binomial")

  CI<-confint.default(B.Model)
  ci<-CI[row.names(CI)=="Genderf"]
  lines( c(i,i), ci, col=ifelse( ci[1]>0|ci[2]<0, 2, 1))
  count[i]<-ifelse( ci[1]>0|ci[2]<0, 1, 0)
}

# I will plot in blue the original CI at the middle
OCI<-confint.default(Model)
(OCI<-OCI[row.names(OCI)=="Genderf"])
lines(c(ex.n/2,ex.n/2),OCI, col="blue", lwd=4)

# It looks that it is not so clear wether Gender has an influence or not in the model
# There are many CI where zero is outside the confidence interval (43% of the times. We will expect that only
# 5% wouldn't contain zero)
sum(count)/ex.n

