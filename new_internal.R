setwd("~/StatLab")
load("snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

# Libraries
library(vcd) 
library(corrplot)
library(caTools)
library(ROCR)
library(pROC)
library(car)
library(effects)
library(MASS)


#######################################################
### Logistic regression IsApproved (NUMERIC grades) ###
#######################################################

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

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

# Confidence Intervals
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






#######################################################
### Logistic regression IsApproved (ORDERED grades) ###
#######################################################

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

# Consider grades as factors
data <- internal_regression_data
data$ApplicantTrack <- ifelse(data$ApplicantTrack<4,3,data$ApplicantTrack)
data$ApplicantTrack <- as.ordered(factor(data$ApplicantTrack))
data$ProjectAssessment <- ifelse(data$ProjectAssessment<4,3,data$ProjectAssessment)
data$ProjectAssessment <- as.ordered(factor(data$ProjectAssessment))

Model2 <- glm(IsApproved ~. -ProjectID-Ranking+ Age:ApplicantTrack+Gender:Division,
             data=data, family="binomial")
summary(Model)

# Confidence Intervals
confint(Model)

# Diagnostic plots
fvl <- predict(Model, type="link")
fpr <- predict(Model, type="response")
IsApproved<-internal_regression_data$IsApproved

plot(fvl, IsApproved, type="n", xlab="linear predictor", ylab="Approval Result")
points(fvl[IsApproved==0], IsApproved[IsApproved==0])
points(fvl[IsApproved==1], IsApproved[IsApproved==1], col="red")
lines(sort(fvl+1), sort(fpr+1), lty=3)
title("Result vs. Linear Predictor")

# It doesn't look very good, because those observations that are wrongly predicted:
#id<-which(fvl<(-1) & IsApproved==1)    # Approved with ProjectAssessment=3
#View(data[id,])

#id<-which(fvl>2 & IsApproved==0)       # NotApproved with ProjectAssessment 5 or 6
#View(data[id,])

xx <- fvl
yy <- residuals(Model, type="deviance")
plot(xx, yy, pch=20, main="Tukey-Anscombe Plot")
lines(loess.smooth(xx, yy, family="gaussian"), col="red")
abline(h=0, lty=3, col="grey")
# Looks pretty good

# Goodness of fit: pseudo-R2
pseudoR <- (1-exp((Model$dev-Model$null)/nrow(data)))/(1-exp(-Model$null/nrow(data)))
pseudoR
# 0.69

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
# 0.3092

data3 <- data
data3$ApplicantTrack <- sample(data$ApplicantTrack,size=nrow(data2), replace=F)

Model_2 <- glm(IsApproved ~. -ProjectID-Ranking+ Age:ApplicantTrack+Gender:Division+ApplicantTrack:Gender,
               data=data3, family="binomial")
summary(Model_2)
pseudoR_2 <- (1-exp((Model_2$dev-Model_2$null)/nrow(data)))/(1-exp(-Model_2$null/nrow(data)))
pseudoR_2  # 0.6835





################################################
### Ordinal regression for ProjectAssessment ###
################################################

library(ordinal)
# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

# Response variable should be ordered
data <- internal_regression_data
#data$ProjectAssessment <- ifelse(data$ProjectAssessment<4,3,data$ProjectAssessment)
data$ProjectAssessment <- as.ordered(data$ProjectAssessment)

# I remove variables that were not significant in order to reduce the 
# condition number of the Hessian that otherwise was even higher (e+06)
# (I chose variables using AIC in drop1 function)

OrdinalModel <- clm(ProjectAssessment ~ Gender+Division+InstType+IsContinuation+
                      log(AmountRequested)+PercentFemale,
                      data=data)
summary(OrdinalModel)

drop1(OrdinalModel, test = "Chi")

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



#############################################
### Ordinal regression for ApplicantTrack ###
#############################################

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

library(ordinal)

# Response variable should be ordered
data <- internal_regression_data
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




###############################################################
### Ordinal regression for ProjectAssessment using 3 levels ###
###############################################################
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

############################################################
### Ordinal regression for ApplicantTrack using 3 levels ###
############################################################

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




######################################################
### Logistic regression for Ranking (with FACTORS) ###
######################################################

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

data <- internal_regression_data
data$ApplicantTrack <- ifelse(data$ApplicantTrack<4,3,data$ApplicantTrack)
data$ApplicantTrack <- as.ordered(factor(data$ApplicantTrack))
data$ProjectAssessment <- ifelse(data$ProjectAssessment<4,3,data$ProjectAssessment)
data$ProjectAssessment <- as.ordered(factor(data$ProjectAssessment))

data$SimplifiedRanking <- ifelse(data$Ranking < 4, 0, 1)

data$SimplifiedRanking <- factor(data$SimplifiedRanking)
leslie_reg_proposal <- glm(SimplifiedRanking ~ Age + Gender + Division + log(AmountRequested)+
                             ProjectAssessment + ApplicantTrack+ PercentFemale+InstType,
                           data=data, family="binomial")
summary(leslie_reg_proposal)


drop1(leslie_reg_proposal, test = "Chi")
confint(leslie_reg_proposal, type = "Wald")

# Odds ratio and confidence intervals for Odds Ratio
round(exp(leslie_reg_proposal$coef), 3)
round(exp(confint(leslie_reg_proposal, type = "Wald")), 1)

# Diagnostic plots
fvl <- predict(leslie_reg_proposal, type="link")
fpr <- predict(leslie_reg_proposal, type="response")
IsApproved<-internal_regression_data$IsApproved

plot(fvl, IsApproved, type="n", xlab="linear predictor", ylab="Approval Result")
points(fvl[IsApproved==0], IsApproved[IsApproved==0])
points(fvl[IsApproved==1], IsApproved[IsApproved==1], col="red")
lines(sort(fvl+1), sort(fpr+1), lty=3)
title("Result vs. Linear Predictor")
# This looks really bad

xx <- fvl
yy <- residuals(leslie_reg_proposal, type="deviance")
plot(xx, yy, pch=20, main="Tukey-Anscombe Plot")
lines(loess.smooth(xx, yy, family="gaussian"), col="red")
abline(h=0, lty=3, col="grey")
# This looks bad too

# Goodness of fit: pseudo-R2
pseudoR <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR
# 0.8255619

# Effect plots: useless in this case...
plot(Effect("Gender", mod=leslie_reg_proposal))
plot(Effect("AmountRequested", mod=leslie_reg_proposal))
plot(Effect("Division", mod=leslie_reg_proposal))
plot(Effect("InstType", mod=leslie_reg_proposal))

# Relative importance: shuffle columns and see how poorer is the fit
data2 <- data
data2$ProjectAssessment <- sample(data2$ProjectAssessment,size=nrow(data2), replace=F)

leslie_reg_proposal <- glm(SimplifiedRanking ~ Age + Gender + Division + log(AmountRequested)+
                             ProjectAssessment + ApplicantTrack+ PercentFemale+InstType+
                             Gender:ApplicantTrack,
                           data=data2, family="binomial")
summary(leslie_reg_proposal)
pseudoR_1 <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR_1
# 0.31197

data3 <- data
data3$ApplicantTrack <- sample(data3$ApplicantTrack,size=nrow(data2), replace=F)

leslie_reg_proposal <- glm(SimplifiedRanking ~ Age + Gender + Division + log(AmountRequested)+
                             ProjectAssessment + ApplicantTrack+ PercentFemale+InstType+
                             Gender:ApplicantTrack,
                           data=data3, family="binomial")
summary(leslie_reg_proposal)
pseudoR_2 <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR_2  # 0.8232




#######################################################
### Logistic regression for Ranking (using NUMERIC) ###
#######################################################

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

data <- internal_regression_data
data$SimplifiedRanking <- ifelse(data$Ranking < 4, 0, 1)

data$SimplifiedRanking <- factor(data$SimplifiedRanking)
leslie_reg_proposal <- glm(SimplifiedRanking ~ Age + Gender + Division + log(AmountRequested)+
                             ProjectAssessment + ApplicantTrack+ PercentFemale+InstType,
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
IsApproved<-internal_regression_data$IsApproved

plot(fvl, IsApproved, type="n", xlab="linear predictor", ylab="Approval Result")
points(fvl[IsApproved==0], IsApproved[IsApproved==0])
points(fvl[IsApproved==1], IsApproved[IsApproved==1], col="red")
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

# Effect plots: these make sense...
plot(Effect("Gender", mod=leslie_reg_proposal))
plot(Effect("AmountRequested", mod=leslie_reg_proposal))
plot(Effect("Division", mod=leslie_reg_proposal))
plot(Effect("InstType", mod=leslie_reg_proposal))

# Relative importance: shuffle columns and see how poorer is the fit
data2 <- data
data2$ProjectAssessment <- sample(data2$ProjectAssessment,size=nrow(data2), replace=F)

leslie_reg_proposal <- glm(SimplifiedRanking ~ Age + Gender + Division + log(AmountRequested)+
                             ProjectAssessment + ApplicantTrack+ PercentFemale+InstType+
                             Gender:ApplicantTrack,
                           data=data2, family="binomial")
summary(leslie_reg_proposal)
pseudoR_1 <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR_1
# 0.31197

data3 <- data
data3$ApplicantTrack <- sample(data3$ApplicantTrack,size=nrow(data2), replace=F)

leslie_reg_proposal <- glm(SimplifiedRanking ~ Age + Gender + Division + log(AmountRequested)+
                             ProjectAssessment + ApplicantTrack+ PercentFemale+InstType+
                             Gender:ApplicantTrack,
                           data=data3, family="binomial")
summary(leslie_reg_proposal)
pseudoR_2 <- (1-exp((leslie_reg_proposal$dev-leslie_reg_proposal$null)/nrow(data)))/(1-exp(-leslie_reg_proposal$null/nrow(data)))
pseudoR_2  # 0.8232




###############################################################
### Ordinal regression for Ranking using 3 levels (NUMERIC) ###
###############################################################

# Change levels of Ranking:
# 1 if Ranking is 1 or 2
# 2 if Ranking is 3 or 4
# 3 if Ranking is 5 or 6

# Prepare data
internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))

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
                        ProjectAssessment + ApplicantTrack+
                        Gender:Division, data=data)
summary(OrdinalModel_b)

drop1(OrdinalModel_b, test = "Chi")
confint(OrdinalModel_b, type = "Wald")

# Odds ratio and confidence intervals for Odds Ratio
round(exp(OrdinalModel_b$beta), 1)
round(exp(confint(OrdinalModel_b, type = "Wald")), 1)

# Effect plots
plot(Effect("Gender", mod=OrdinalModel_b))
plot(Effect("ApplicantTrack",mod=OrdinalModel_b))
plot(Effect("ProjectAssessment", mod=OrdinalModel_b))
plot(Effect("IsContinuation",mod=OrdinalModel_b))



