setwd("~/StatLab")
load("snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

# Libraries
library(vcd) 
library(car)
library(effects)
library(MASS)
library(psych)
library(splines)
library(ggplot2)


#################################################################################
#####         Logistic regression IsApproved (NUMERIC grades)              ######
#################################################################################

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
# The following line is just for me (Chiara):
# internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

# Scale data before fitting the model to get standardized coefficients
data <- internal_regression_data
data$ProjectAssessment <- scale(data$ProjectAssessment,center = T, scale = T)
data$ApplicantTrack <- scale(data$ApplicantTrack, center = T, scale = T)

Model <- glm(IsApproved ~. -ProjectID-Ranking+ Age:ApplicantTrack+Gender:Division+ApplicantTrack:Gender,
             data=data, family="binomial")
summary(Model)

# Coeffcients interpretation
c.gender <- (exp(coef(Model)[2])-1)*100
c.cont <- (exp(coef(Model)[7])-1)*100
c.appltrack <- exp(coef(Model)[14])
c.project <- exp(coef(Model)[15])
# CI:
confint(Model)

# vif(Model) 

# Diagnostic plots
fvl <- predict(Model, type="link")
fpr <- predict(Model, type="response")
IsApproved<-internal_regression_data$IsApproved

plot(fvl, IsApproved, type="n", xlab="linear predictor", ylab="Approval Result")
points(fvl[IsApproved==0], IsApproved[IsApproved==0])
points(fvl[IsApproved==1], IsApproved[IsApproved==1], col="red")
lines(sort(fvl+1), sort(fpr+1), lty=3)
title("Result vs. Linear Predictor")

xx <- fvl
yy <- residuals(Model, type="deviance")
plot(xx, yy, pch=20, main="Tukey-Anscombe Plot")
lines(loess.smooth(xx, yy, family="gaussian"), col="red")
abline(h=0, lty=3, col="grey")

# Goodness of fit: pseudo-R2
pseudoR <- (1-exp((Model$dev-Model$null)/nrow(data)))/(1-exp(-Model$null/nrow(data)))
pseudoR

# Visualization with effects package
plot(Effect("Gender", mod=Model))
plot(Effect(c("ApplicantTrack"), mod=Model))
plot(Effect("ProjectAssessment", mod=Model))
plot(Effect(c("Gender","ApplicantTrack"), mod=Model))
plot(Effect(c("Gender","ProjectAssessment"), mod=Model))
plot(Effect("AmountRequested", mod=Model))
plot(Effect("Division", mod=Model))
plot(Effect("Age",mod=Model))
plot(Effect("InstType", mod=Model))
plot(Effect("PercentFemale",mod=Model))
plot(Effect("IsContinuation",mod=Model))
plot(Effect("PreviousRequest",mod=Model))
plot(Effect("Semester",mod=Model))

# Relative importance: shuffle columns and see how poorer is the fit
data2 <- data
data2$ProjectAssessment <- sample(data$ProjectAssessment,size=nrow(data2), replace=F)

Model_1 <- glm(IsApproved ~. -ProjectID-Ranking+ Age:ApplicantTrack+Gender:Division+ApplicantTrack:Gender,
               data=data2, family="binomial")
summary(Model_1)
pseudoR_1 <- (1-exp((Model_1$dev-Model_1$null)/nrow(data)))/(1-exp(-Model_1$null/nrow(data)))
pseudoR_1
# 0.3056

data3 <- data
data3$ApplicantTrack <- sample(data$ApplicantTrack,size=nrow(data2), replace=F)

Model_2 <- glm(IsApproved ~. -ProjectID-Ranking+ Age:ApplicantTrack+Gender:Division+ApplicantTrack:Gender,
               data=data3, family="binomial")
summary(Model_2)
pseudoR_2 <- (1-exp((Model_2$dev-Model_2$null)/nrow(data)))/(1-exp(-Model_2$null/nrow(data)))
pseudoR_2  # 0.6817






#################################################################################
#####    Logistic regression IsApproved (ORDERED grades on 6 levels)        #####
#################################################################################

# Prepare the data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
# internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

# Take the log of AmountRequested
internal_regression_data$logAmount <- log(internal_regression_data$AmountRequested)


# Visualization of Approval vs ApplicantTrack by internal referees
data<- subset(internal_regression_data,select = -c(ProjectID, Ranking, AmountRequested))
barplot(table(data$IsApproved,data$ApplicantTrack),col=c("red","green"), beside=TRUE,
        main = "Applicant Track Grade vs. Approval")
legend("topleft",bty="n", legend=c("Not Approved","Approved"),lty=2,col=c("red","green"))


# Visualization of Approval vs ProjectAssessment by internal referees
data<- subset(internal_regression_data,select = -c(ProjectID, Ranking, AmountRequested))
barplot(table(data$IsApproved,data$ProjectAssessment),col=c("red","green"), beside=TRUE,
        main = "Applicant Track Grade vs. Project")
legend("topleft",bty="n", legend=c("Not Approved","Approved"),lty=2,col=c("red","green"))


# Consider grades as ordered factors
# data$ApplicantTrack <- ifelse(data$ApplicantTrack<4,3,data$ApplicantTrack)
data$ApplicantTrack <- as.ordered(factor(data$ApplicantTrack))
# data$ProjectAssessment <- ifelse(data$ProjectAssessment<4,3,data$ProjectAssessment)
data$ProjectAssessment <- as.ordered(factor(data$ProjectAssessment))


# Fit the model
Model <- glm(IsApproved ~. + Gender:ApplicantTrack+Gender:Division+Gender:PercentFemale,
              data=data, family="binomial")
summary(Model)
# There is one NA for the interaction Gender:ApplicantTrack^5

# Confidence Intervals:
confint(Model)


# Model Diagnostic
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
# The plot doesn't look good...

# Tuckey-Anscombe plot
xx <- fvl
yy <- residuals(Model, type="deviance")
plot(xx, yy, pch=20, main="Tukey-Anscombe Plot")
lines(loess.smooth(xx, yy, family="gaussian"), col="red")
abline(h=0, lty=3, col="grey")
# It doesn't look good either...


# Kappa Statistic
linear.weights <- matrix(c(1, 0.8, 0.8, 1), nrow=2, ncol=2, byrow=T)
kappa.matrix <- as.matrix(table(data$IsApproved,pred>=0.5))
kappa <- cohen.kappa(kappa.matrix, w=linear.weights)
kappa  # 0.73

# Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha, levels = levels)
# 
# Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
# lower estimate upper
# unweighted kappa  0.70     0.73  0.77
# weighted kappa    0.71     0.73  0.76
# 
# Number of subjects = 1623

# Pseudos-R^2
(PsR22<-(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))) #[1] 0.6963247

# optimize model see if we can drop some variables
drop1(Model, test="Chisq")

Model<-update(Model,.~.-ApplicantTrack:Gender)      
summary(Model)
# No more NAs but still huge p-values

# Pseudos-R^2
(PsR22<-(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))) #[1] 0.6933831
# Only a bit smaller



#################################################################################
#### Logistic regression IsApproved (ORDERED grades SIMPLIFIED on 4 levels)  ####
#################################################################################
# Put together grades from 1 to 3: 4 levels in total


# Prepare the data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
# internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

# Take the log of AmountRequested
internal_regression_data$logAmount <- log(internal_regression_data$AmountRequested)

# Consider grades as ordered factors and simplify them
data<- subset(internal_regression_data,select = -c(ProjectID, Ranking, AmountRequested))
data$ApplicantTrack <- ifelse(data$ApplicantTrack<4,3,data$ApplicantTrack)
data$ApplicantTrack <- as.ordered(factor(data$ApplicantTrack))
data$ProjectAssessment <- ifelse(data$ProjectAssessment<4,3,data$ProjectAssessment)
data$ProjectAssessment <- as.ordered(factor(data$ProjectAssessment))


# Fit the model
Model <- glm(IsApproved ~. + Gender:ApplicantTrack+Gender:Division+Gender:PercentFemale,
             data=data, family="binomial")
summary(Model)
# There are no NAs, better p-values

# Confidence Intervals:
confint(Model)


# Model Diagnostic
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
# The plot doesn't look good either: the two groups overlap

# Tuckey-Anscombe plot
xx <- fvl
yy <- residuals(Model, type="deviance")
plot(xx, yy, pch=20, main="Tukey-Anscombe Plot")
lines(loess.smooth(xx, yy, family="gaussian"), col="red")
abline(h=0, lty=3, col="grey")
# Could be considered ok?

# Kappa Statistic
linear.weights <- matrix(c(1, 0.8, 0.8, 1), nrow=2, ncol=2, byrow=T)
kappa.matrix <- as.matrix(table(data$IsApproved,pred>=0.5))
kappa <- cohen.kappa(kappa.matrix, w=linear.weights) # 0.74
kappa

# Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha, levels = levels)
# 
# Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
# lower estimate upper
# unweighted kappa  0.70     0.74  0.77
# weighted kappa    0.71     0.74  0.76
# 
# Number of subjects = 1623

# Pseudos-R^2
(PsR22<-(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))) #[1] 0.6896911
# A bit smaller than before (using 6 levels)

drop1(Model, test="Chisq")
Model <- update(Model, .~.-ApplicantTrack:Gender)
# Remove InstType?

# Pseudos-R^2
(PsR22<-(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))) #[1] 0.6886228

# It doesn't look very good, because those observations that are wrongly predicted:
# id<-which(fvl<(-1) & IsApproved==1)    # Approved with ProjectAssessment=3
# id<-which(fvl>2 & IsApproved==0)       # NotApproved with ProjectAssessment 5 or 6


# Visualization with effects package
plot(Effect("Gender", mod=Model))
plot(Effect(c("ApplicantTrack"), mod=Model))
plot(Effect("ProjectAssessment", mod=Model))
plot(Effect(c("Gender","ApplicantTrack"), mod=Model))
plot(Effect(c("Gender","ProjectAssessment"), mod=Model))
plot(Effect("logAmount", mod=Model))
plot(Effect("Division", mod=Model))
plot(Effect("Age",mod=Model))
plot(Effect("InstType", mod=Model))
plot(Effect("PercentFemale",mod=Model))
plot(Effect("IsContinuation",mod=Model))
plot(Effect("Semester",mod=Model))

# I first did it manually to check which one was most important between Track and Project
# Relative importance: shuffle columns and see how poorer is the fit
data2 <- data
data2$ProjectAssessment <- sample(data$ProjectAssessment,size=nrow(data2), replace=F)

Model_1 <- glm(IsApproved ~. + Age:ApplicantTrack+Gender:Division+ApplicantTrack:Gender,
               data=data2, family="binomial")
summary(Model_1)
pseudoR_1 <- (1-exp((Model_1$dev-Model_1$null)/nrow(data)))/(1-exp(-Model_1$null/nrow(data)))
pseudoR_1
# 0.3092

data3 <- data
data3$ApplicantTrack <- sample(data$ApplicantTrack,size=nrow(data2), replace=F)

Model_2 <- glm(IsApproved ~. + Age:ApplicantTrack+Gender:Division+ApplicantTrack:Gender,
               data=data3, family="binomial")
summary(Model_2)
pseudoR_2 <- (1-exp((Model_2$dev-Model_2$null)/nrow(data)))/(1-exp(-Model_2$null/nrow(data)))
pseudoR_2  # 0.6835


# I used here Carla's function to check all the variables at the same time
calc_pseudo_r <- function(Model,n) {
  (1-exp((Model$dev-Model$null)/n))/(1-exp(-Model$null/n))
}

outcomeName <- 'IsApproved'
predictorNames <- setdiff(names(data),outcomeName)
predictions <- predict(object=Model, data[,predictorNames], type="response")

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

# Gender is the least important variable!
# IsContinuation, ApplicantTrack and ProjectAssessment are the most important
# PercentFemale is important too!

# However, using AUC the result is different... 
# Which one should we use??




#################################################################################
######   Ordinal regression for ProjectAssessment (ORDERED on 6 levels)    ######
#################################################################################

library(ordinal)

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
# internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

# Response variable should be ordered
data <- internal_regression_data

# I'm not sure if we should use 4 levels as before (to be consistent) or if we can keep 6 levels here
# since it's not working too bad...
# data$ProjectAssessment <- ifelse(data$ProjectAssessment<4,3,data$ProjectAssessment)

data$ProjectAssessment <- as.ordered(data$ProjectAssessment)

# I remove variables that were not significant in order to reduce the 
# condition number of the Hessian that otherwise was even higher (e+06)
# (I chose variables using AIC in drop1 function)

OrdinalModel <- clm(ProjectAssessment ~ Gender+Division+InstType+IsContinuation+
                      log(AmountRequested)+PercentFemale,
                    data=data)
summary(OrdinalModel)

drop1(OrdinalModel, test = "Chi")
# Can't reduce AIC amymore

confint(OrdinalModel, type = "Wald")

# Odds ratio and confidence intervals for Odds Ratio
round(exp(OrdinalModel$beta), 1)
round(exp(confint(OrdinalModel, type = "Wald")), 1)

# Use deviance as goodness of fit??

# Effect plots
plot(Effect("Gender", mod=OrdinalModel))
plot(Effect("AmountRequested", mod=OrdinalModel))
plot(Effect("Division", mod=OrdinalModel))
plot(Effect("InstType", mod=OrdinalModel))
plot(Effect("PercentFemale",mod=OrdinalModel))
plot(Effect("IsContinuation",mod=OrdinalModel))



#################################################################################
######     Ordinal regression for ApplicantTrack (ORDERED on 6 levels)     ######
#################################################################################

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
# internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

library(ordinal)

# Response variable should be ordered
data <- internal_regression_data
# Same question as before:
#data$ApplicantTrack <- ifelse(data$ApplicantTrack < 4, 3, data$ApplicantTrack)
data$ApplicantTrack <- as.ordered(data$ApplicantTrack)

# I remove variables that were not significant in order to reduce the 
# condition number of the Hessian that otherwise was even higher (e+06)
# (I chose variables using AIC in drop1 function)

OrdinalModel2 <- clm(ApplicantTrack ~ Gender+Division+InstType+IsContinuation+
                       Gender:Division+log(AmountRequested)+PercentFemale,
                     data=data)
summary(OrdinalModel2)

drop1(OrdinalModel2, test = "Chi")
# Cannot be imporved

confint(OrdinalModel2, type = "Wald")

# Odds ratio and confidence intervals for Odds Ratio
round(exp(OrdinalModel2$beta), 1)
round(exp(confint(OrdinalModel2, type = "Wald")), 1)

# Use deviance as goodness of fit??

# Effect plots
plot(Effect("Gender", mod=OrdinalModel2))
plot(Effect("AmountRequested", mod=OrdinalModel2))
plot(Effect("Division", mod=OrdinalModel2))
plot(Effect("InstType", mod=OrdinalModel2))
plot(Effect("IsContinuation",mod=OrdinalModel2))



#################################################################################
######   Ordinal regression for ProjectAssessment (ORDERED on 3 levels)    ######
#################################################################################

# I tried this to make visualization more understandable,
# because with 6 levels it's a bit confusing...
# However the model I obtain is the same and the number of Hessian is still very big,
# so probably it doesn't make sense to include this (and the following) regression

# Change levels of ApplicantTrack:
# 1 if ProjectAssessment is 1 or 2
# 2 if ProjectAssessment is 3 or 4
# 3 if ProjectAssessment is 5 or 6
data <- internal_regression_data
data$ProjectAssessment <- ifelse(data$ProjectAssessment<3,1,data$ProjectAssessment)
data$ProjectAssessment <- ifelse(data$ProjectAssessment<5 & data$ProjectAssessment>2,2,data$ProjectAssessment)
data$ProjectAssessment <- ifelse(data$ProjectAssessment>4,3,data$ProjectAssessment)
data$ProjectAssessment <- as.ordered(data$ProjectAssessment)
# table(data$ProjectAssessment)

# I remove variables that were not significant in order to reduce the 
# condition number of the Hessian that otherwise was even higher (e+06)
# (I chose variables using AIC in drop1 function)

OrdinalModel_a <- clm(ProjectAssessment ~ Gender+Division+InstType+IsContinuation+
                        log(AmountRequested)+PercentFemale,
                      data=data)
summary(OrdinalModel_a)

drop1(OrdinalModel_a, test = "Chi")
confint(OrdinalModel_a, type = "Wald")

# Odds ratio and confidence intervals for Odds Ratio
round(exp(OrdinalModel_a$beta), 1)
round(exp(confint(OrdinalModel_a, type = "Wald")), 1)

# Effect plots
plot(Effect("Gender", mod=OrdinalModel_a))
plot(Effect("AmountRequested", mod=OrdinalModel_a))
plot(Effect("Division", mod=OrdinalModel_a))
plot(Effect("InstType", mod=OrdinalModel_a))
plot(Effect("IsContinuation",mod=OrdinalModel_a))

#################################################################################
######     Ordinal regression for ApplicantTrack  (ORDERED on 3 levels)    ######
#################################################################################
# Same as before...


# Change levels of ApplicantTrack:
# 1 if ApplicantTrack is 1 or 2
# 2 if ApplicantTrack is 3 or 4
# 3 if ApplicantTrack is 5 or 6
data <- internal_regression_data
data$ApplicantTrack <- ifelse(data$ApplicantTrack<3,1,data$ApplicantTrack)
data$ApplicantTrack <- ifelse(data$ApplicantTrack<5 & data$ApplicantTrack>2,2,data$ApplicantTrack)
data$ApplicantTrack <- ifelse(data$ApplicantTrack>4,3,data$ApplicantTrack)
data$ApplicantTrack <- as.ordered(data$ApplicantTrack)
# table(data$ApplicantTrack)

# I remove variables that were not significant in order to reduce the 
# condition number of the Hessian that otherwise was even higher (e+06)
# (I chose variables using AIC in drop1 function)

OrdinalModel_b <- clm(ApplicantTrack ~ Gender+Division+InstType+IsContinuation+
                        Gender:Division+log(AmountRequested)+PercentFemale,
                      data=data)
summary(OrdinalModel_b)

drop1(OrdinalModel_b, test = "Chi")
confint(OrdinalModel_b, type = "Wald")

# Odds ratio and confidence intervals for Odds Ratio
round(exp(OrdinalModel_b$beta), 1)
round(exp(confint(OrdinalModel_b, type = "Wald")), 1)

# Effect plots
plot(Effect("Gender", mod=OrdinalModel_b))
plot(Effect("AmountRequested", mod=OrdinalModel_b))
plot(Effect("Division", mod=OrdinalModel_b))
plot(Effect("InstType", mod=OrdinalModel_b))
plot(Effect("IsContinuation",mod=OrdinalModel_b))




#################################################################################
#####  Logistic regression for Ranking  (predictors ORDERED on 3 levels)   ######
#################################################################################

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
# internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

data <- internal_regression_data
data$ApplicantTrack <- ifelse(data$ApplicantTrack<4,3,data$ApplicantTrack)
data$ApplicantTrack <- as.ordered(factor(data$ApplicantTrack))
data$ProjectAssessment <- ifelse(data$ProjectAssessment<4,3,data$ProjectAssessment)
data$ProjectAssessment <- as.ordered(factor(data$ProjectAssessment))

data$SimplifiedRanking <- ifelse(data$Ranking < 4, 0, 1)
data$SimplifiedRanking <- factor(data$SimplifiedRanking)
data$logAmount <- log(data$AmountRequested)
data <- subset(data,select = -c(ProjectID, Ranking, AmountRequested, IsApproved))

leslie_reg_proposal <- glm(SimplifiedRanking ~ . + Gender:Division +
                             Gender:ApplicantTrack + Gender:PercentFemale,
                           data=data, family="binomial")
summary(leslie_reg_proposal)
# No NAs, huge p-values

drop1(leslie_reg_proposal, test = "Chi")

leslie_reg_proposal <- update(leslie_reg_proposal, .~.-Division:Gender)
summary(leslie_reg_proposal)

confint(leslie_reg_proposal, type = "Wald")

# Odds ratio and confidence intervals for Odds Ratio
round(exp(leslie_reg_proposal$coef), 3)
round(exp(confint(leslie_reg_proposal, type = "Wald")), 1)

# Diagnostic plots
fvl <- predict(leslie_reg_proposal, type="link")
pred <- predict(leslie_reg_proposal, type="response")
SimplRanking <-data$SimplifiedRanking

plot(fvl, SimplRanking, type="n", xlab="linear predictor", ylab="Approval Result")
points(fvl[SimplRanking==0], SimplRanking[SimplRanking==0])
points(fvl[SimplRanking==1], SimplRanking[SimplRanking==1], col="red")
lines(sort(fvl+1), sort(pred+1), lty=3)
title("Result vs. Linear Predictor")
# This looks pretty bad

xx <- fvl
yy <- residuals(leslie_reg_proposal, type="deviance")
plot(xx, yy, pch=20, main="Tukey-Anscombe Plot")
lines(loess.smooth(xx, yy, family="gaussian"), col="red")
abline(h=0, lty=3, col="grey")
# This looks quite bad too

# Goodness of fit: pseudo-R2
pseudoR <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR
# 0.8294524

# Effect plots: useless in this case...
plot(Effect("Gender", mod=leslie_reg_proposal))
plot(Effect("ProjectAssessment", mod=leslie_reg_proposal))
plot(Effect("ApplicantTrack", mod=leslie_reg_proposal))
plot(Effect(c("Gender","PercentFemale"), mod=leslie_reg_proposal))
plot(Effect(c("Gender","Division"), mod=leslie_reg_proposal))
# Basically only ProjectAssessment have an effect...

# Kappa Statistic
linear.weights <- matrix(c(1, 0.8, 0.8, 1), nrow=2, ncol=2, byrow=T)
kappa.matrix <- as.matrix(table(data$SimplifiedRanking,pred>=0.5))
kappa <- cohen.kappa(kappa.matrix, w=linear.weights)
kappa  # 0.88

# Call: cohen.kappa1(x = x, w = w, n.obs = n.obs, alpha = alpha, levels = levels)
# 
# Cohen Kappa and Weighted Kappa correlation coefficients and confidence boundaries 
# lower estimate upper
# unweighted kappa  0.85     0.88  0.90
# weighted kappa    0.88     0.88  0.88
# 
# Number of subjects = 1623



# Relative importance by hand:
# Relative importance: shuffle columns and see how poorer is the fit
data2 <- data
data2$ProjectAssessment <- sample(data2$ProjectAssessment,size=nrow(data2), replace=F)

leslie_reg_proposal <- glm(SimplifiedRanking ~ . + Gender:Division +
                             Gender:ApplicantTrack + Gender:PercentFemale,
                           data=data2, family="binomial")
summary(leslie_reg_proposal)
pseudoR_1 <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR_1


data3 <- data
data3$ApplicantTrack <- sample(data3$ApplicantTrack,size=nrow(data2), replace=F)

leslie_reg_proposal <- glm(SimplifiedRanking ~ . + Gender:Division +
                             Gender:ApplicantTrack + Gender:PercentFemale,
                           data=data3, family="binomial")
summary(leslie_reg_proposal)
pseudoR_2 <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR_2 


# Function for relative importance:
calc_pseudo_r <- function(Model,n) {
  (1-exp((Model$dev-Model$null)/n))/(1-exp(-Model$null/n))
}


outcomeName <- 'SimplifiedRanking'
predictorNames <- setdiff(names(data),outcomeName)
predictions <- predict(object=leslie_reg_proposal, data[,predictorNames], type="response")

refR2<-calc_pseudo_r(leslie_reg_proposal,nrow(data))
PseudoRShuffle <- NULL
shuffletimes <- 100  #number of interactions

featuresMeanR2 <- c()
for (feature in predictorNames) {
  featureR2 <- c()
  shuffledData <- data
  for (iter in 1:shuffletimes) {
    shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
    Model.tmp <- update(leslie_reg_proposal,.~.,data=shuffledData)
    featureR2 <- c(featureR2,calc_pseudo_r(Model.tmp,nrow(shuffledData)))
  }
  featuresMeanR2 <- c(featuresMeanR2, mean(featureR2 < refR2))
}  

PseudoRShuffle <- data.frame('feature'=predictorNames, 'importance'=featuresMeanR2)
PseudoRShuffle <- PseudoRShuffle[order(PseudoRShuffle$importance, decreasing=TRUE),]
print(PseudoRShuffle)

# Gender importance: 0.16 (second last)
# Most important variables are: ProjectAssessment, ApplicantTrack, Semester, IsContinuation
# Least important: Division



#################################################################################
#####     Logistic regression for Ranking (using NUMERIC predictors)        #####
#################################################################################

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
# internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

data <- internal_regression_data
data$SimplifiedRanking <- ifelse(data$Ranking < 4, 0, 1)
data$SimplifiedRanking <- factor(data$SimplifiedRanking)
data$ApplicantTrack <- as.numeric(data$ApplicantTrack)
data$ProjectAssessment <- as.numeric(data$ProjectAssessment)
data$logAmount <- log(data$AmountRequested)
data <- subset(data, select = -c(Ranking, ProjectID, AmountRequested, IsApproved))

leslie_reg_proposal <- glm(SimplifiedRanking ~ . + Gender:Division +
                             Gender:ApplicantTrack + Gender:PercentFemale,
                           data=data, family="binomial")
summary(leslie_reg_proposal)


drop1(leslie_reg_proposal, test = "Chi")
confint(leslie_reg_proposal, type = "Wald")

# Odds ratio and confidence intervals for Odds Ratio
# round(exp(leslie_reg_proposal$coef), 3)
# round(exp(confint(leslie_reg_proposal, type = "Wald")), 1)

# Diagnostic plots
fvl <- predict(leslie_reg_proposal, type="link")
fpr <- predict(leslie_reg_proposal, type="response")
SimplRanking <-data$SimplifiedRanking

plot(fvl, SimplRanking, type="n", xlab="linear predictor", ylab="Approval Result")
points(fvl[SimplRanking==0], SimplRanking[SimplRanking==0])
points(fvl[SimplRanking==1], SimplRanking[SimplRanking==1], col="red")
lines(sort(fvl+1), sort(fpr+1), lty=3)
title("Result vs. Linear Predictor")

xx <- fvl
yy <- residuals(leslie_reg_proposal, type="deviance")
plot(xx, yy, pch=20, main="Tukey-Anscombe Plot")
lines(loess.smooth(xx, yy, family="gaussian"), col="red")
abline(h=0, lty=3, col="grey")

# Goodness of fit: pseudo-R2
pseudoR <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR
# 0.8279846

# Effect plots: these make sense...
plot(Effect("Gender", mod=leslie_reg_proposal))
plot(Effect("logAmount", mod=leslie_reg_proposal))
plot(Effect("Division", mod=leslie_reg_proposal))
plot(Effect("InstType", mod=leslie_reg_proposal))

# By hand:
# Relative importance: shuffle columns and see how poorer is the fit
data2 <- data
data2$ProjectAssessment <- sample(data2$ProjectAssessment,size=nrow(data2), replace=F)

leslie_reg_proposal <- glm(SimplifiedRanking ~ . + Gender:Division +
                             Gender:ApplicantTrack + Gender:PercentFemale,
                           data=data2, family="binomial")
summary(leslie_reg_proposal)
pseudoR_1 <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR_1
# 0.31197

data3 <- data
data3$ApplicantTrack <- sample(data3$ApplicantTrack,size=nrow(data2), replace=F)

leslie_reg_proposal <- glm(SimplifiedRanking ~ . + Gender:Division +
                             Gender:ApplicantTrack + Gender:PercentFemale,
                           data=data3, family="binomial")
summary(leslie_reg_proposal)
pseudoR_2 <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR_2  # 0.8232



# Function for relative importance:
calc_pseudo_r <- function(Model,n) {
  (1-exp((Model$dev-Model$null)/n))/(1-exp(-Model$null/n))
}


outcomeName <- 'SimplifiedRanking'
predictorNames <- setdiff(names(data),outcomeName)
predictions <- predict(object=leslie_reg_proposal, data[,predictorNames], type="response")

refR2<-calc_pseudo_r(leslie_reg_proposal,nrow(data))
PseudoRShuffle <- NULL
shuffletimes <- 100  #number of interactions

featuresMeanR2 <- c()
for (feature in predictorNames) {
  featureR2 <- c()
  shuffledData <- data
  for (iter in 1:shuffletimes) {
    shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
    Model.tmp <- update(leslie_reg_proposal,.~.,data=shuffledData)
    featureR2 <- c(featureR2,calc_pseudo_r(Model.tmp,nrow(shuffledData)))
  }
  featuresMeanR2 <- c(featuresMeanR2, mean(featureR2 < refR2))
}  

PseudoRShuffle <- data.frame('feature'=predictorNames, 'importance'=featuresMeanR2)
PseudoRShuffle <- PseudoRShuffle[order(PseudoRShuffle$importance, decreasing=TRUE),]
print(PseudoRShuffle)

# Gender importance: 0.09 (second last)
# Most important variables are: ProjectAssessment, ApplicantTrack, Semester, IsContinuation
# Least important: Division
# (As before... even if figures are different...)




#################################################################################
#####  Ordinal regression for Ranking using 3 levels (FACTOR response)      #####
#################################################################################

# Change levels of Ranking:
# 1 if Ranking is 1 or 2
# 2 if Ranking is 3 or 4
# 3 if Ranking is 5 or 6

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
# internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

data <- internal_regression_data
data$Ranking <- ifelse(data$Ranking<3,1,data$Ranking)
data$Ranking <- ifelse(data$Ranking<5 & data$Ranking>2,2,data$Ranking)
data$Ranking <- ifelse(data$Ranking>4,3,data$Ranking)
data$Ranking <- as.ordered(data$Ranking)
# table(data$Ranking)

# I remove variables that were not significant in order to reduce the 
# condition number of the Hessian that otherwise was even higher (e+06)
# (I chose variables using AIC in drop1 function)

OrdinalModel_b <- clm(Ranking ~ Gender + Division + IsContinuation+
                        ProjectAssessment + ApplicantTrack+ Gender:ApplicantTrack + Gender:PercentFemale + 
                        Gender:Division, data=data)
summary(OrdinalModel_b)

drop1(OrdinalModel_b, test = "Chi")
OrdinalModel_b <- update(OrdinalModel_b, .~.-Gender:PercentFemale)
summary(OrdinalModel_b)

confint(OrdinalModel_b, type = "Wald")

# Odds ratio and confidence intervals for Odds Ratio
round(exp(OrdinalModel_b$beta), 1)
round(exp(confint(OrdinalModel_b, type = "Wald")), 1)

# Effect plots
plot(Effect("Gender", mod=OrdinalModel_b))
plot(Effect("ApplicantTrack",mod=OrdinalModel_b))
plot(Effect("ProjectAssessment", mod=OrdinalModel_b))
plot(Effect("IsContinuation",mod=OrdinalModel_b))




########################################################
### DIFFERENCE IN EVALUATION BETWEEN MALE AND FEMALE ###
########################################################

r.tab<-prop.table(table(internal_reviews$RefereeGender,internal_reviews$Ranking),1)
#mycol<-colorRampPalette(c("red", "green"))(6)
barplot(r.tab, beside = TRUE, col = c("pink","lightblue"), 
        legend.text = c("Female", "Male"),
        main="Difference on how Female and Male grade")

# Also in internal referees women seem to be strichter than men (Ranking)

r.tab<-prop.table(table(internal_reviews$RefereeGender,internal_reviews$ApplicantTrack),1)
#mycol<-colorRampPalette(c("red", "green"))(6)
barplot(r.tab, beside = TRUE, col = c("pink","lightblue"), 
        legend.text = c("Female", "Male"),
        main="Difference on how Female and Male grade")

# Women on average give less outstanding and excellent grades for ApplicantTrack

r.tab<-prop.table(table(internal_reviews$RefereeGender,internal_reviews$ProjectAssessment),1)
#mycol<-colorRampPalette(c("red", "green"))(6)
barplot(r.tab, beside = TRUE, col = c("pink","lightblue"), 
        legend.text = c("Female", "Male"),
        main="Difference on how Female and Male grade")

# Women on average give less outstanding and excellent grades for ProjectAssessment




####################################
### Linear-by-Linear association ###
####################################
