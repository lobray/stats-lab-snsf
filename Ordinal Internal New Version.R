setwd("~/StatLab")
load("snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

library(ordinal)
library(ggplot2)
library(MASS)

rm(applications,reviews,referee_grades, test)

#########################################################
######   Ordinal regression for ProjectAssessment  ######
#########################################################

## Get the Regression data:

  internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
  # internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))
  internal_regression_data$logAmount <- log(internal_regression_data$AmountRequested)
  data<- subset(internal_regression_data,select = -c(ProjectID, Ranking, AmountRequested))

  # str(data)

## Visualization:
  
  ggplot(internal_regression_data, aes(x = ProjectAssessment, y = PercentFemale, col=Gender)) +
    geom_jitter(alpha = .5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

## Fit full the model:
  
  fit <- clm(ProjectAssessment ~Gender*(Division+PercentFemale)+Age+Division+IsContinuation+
                PreviousRequest+InstType+Semester+logAmount,
              data=data)
  summary(fit)
  # cond.H very large

## Fit the null Model:
  
  fit.null<- clm(ProjectAssessment~1, data=data)
  
  fit.null$df.residual- fit$df.residual
  LR<-fit.null$logLik/fit$logLik
  1-pchisq(LR,df=15)

  # [1] 0.9999997 : null Model seems to be better than the full model
  # The variables we are using have no predictive power for the ProjectAssessment
  # (Good!)
  
  #  anova(fit.null, fit)

## Variable selection (AIC):
  
  drop1(fit, test = "Chi")
  fit <- update(fit, .~.-Gender:Division)
  fit <- update(fit, .~.-Semester)
  fit <- update(fit, .~.-Gender:PercentFemale)
  fit <- update(fit, .~.-PreviousRequest)
  
  # Final model:
  
  Model<- clm(ProjectAssessment ~ Gender + Division + PercentFemale + IsContinuation + 
                Age + logAmount + InstType ,data=data)
  summary(Model)
  
  # Still huge cond.H
  
  fit.null$df.residual- Model$df.residual
  LR<-fit.null$logLik/Model$logLik
  1-pchisq(LR,df=10)

## Confidence intervals:
  
  confint(Model, type = "Wald")
  
  # Odds ratio and confidence intervals for Odds Ratio
  round(exp(Model$beta), 1)
  round(exp(confint(Model, type = "Wald")), 1) # There are some huge intervals

## Effect visualization:
  
  plot(Effect("Gender", mod=Model))
  plot(Effect("logAmount", mod=Model)) 
  # The  higher the amount the likely it is to have a higher grade.
  
  plot(Effect("Division", mod=Model))  
  plot(Effect("InstType", mod=Model))
  plot(Effect("IsContinuation",mod=Model))
  

######################################################
######   Ordinal regression for ApplicantTrack  ######
######################################################
  
  ## Get the Regression data:
  
  internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
  # internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))
  internal_regression_data$logAmount <- log(internal_regression_data$AmountRequested)
  data<- subset(internal_regression_data,select = -c(ProjectID, Ranking, AmountRequested))
  
  # str(data)
  
  ## Visualization:
  
  ggplot(internal_regression_data, aes(x = ApplicantTrack, y = PercentFemale, col=Gender)) +
    geom_jitter(alpha = .5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  ## Fit full the model:
  
  fit <- clm(ApplicantTrack ~Gender*(Division+PercentFemale)+Age+Division+IsContinuation+
               PreviousRequest+InstType+Semester+logAmount,
             data=data)
  summary(fit)
  # cond.H very large
  
  ## Fit the null Model:
  
  fit.null<- clm(ApplicantTrack~1, data=data)
  
  fit.null$df.residual- fit$df.residual
  LR<-fit.null$logLik/fit$logLik
  1-pchisq(LR,df=15)
  
  # [1] 0.9999996 : null Model seems to be better than the full model
  # The variables we are using have no predictive power for the ProjectAssessment
  # (Good!)
  
  #  anova(fit.null, fit)
  
  ## Variable selection (AIC):
  
  drop1(fit, test = "Chi")
  fit <- update(fit, .~.-Gender:PercentFemale)
  fit <- update(fit, .~.-PreviousRequest)
  fit <- update(fit, .~.-Age)
  
  # Final model:
  
  Model<- clm(ApplicantTrack ~ Gender*Division + PercentFemale + IsContinuation + 
                Semester + logAmount + InstType ,data=data)
  summary(Model)
  
  # Still huge cond.H
  
  fit.null$df.residual- Model$df.residual
  LR<-fit.null$logLik/Model$logLik
  1-pchisq(LR,df=12)
  
  ## Confidence intervals:
  
  confint(Model, type = "Wald")
  
  # Odds ratio and confidence intervals for Odds Ratio
  round(exp(Model$beta), 1)
  round(exp(confint(Model, type = "Wald")), 1) # There are some huge intervals
  
  ## Effect visualization:
  
  plot(Effect("Gender", mod=Model))
  plot(Effect("logAmount", mod=Model)) 

  plot(Effect("Division", mod=Model))  
  plot(Effect("InstType", mod=Model))
  plot(Effect("IsContinuation",mod=Model))

  
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
  
  library(coin)
  
  ## Ranking:
  
  data <- internal_regression_data
  data$Ranking <- ifelse(data$Ranking < 4, 3, data$Ranking)
  
  table(data$Gender, data$Ranking)
  cTab <- xtabs(~ Gender + Ranking, data=data)
  
  spineplot(cTab)
  lbl_test(cTab)
  # p-value=5.632e-06 -> reject H0 -> no independence?
  
  # Compare to chi-square test without ordered categories
  chisq_test(cTab)  # p-value = 3.516e-05
  
  
  ## ApplicantTrack:
  
  data$ApplicantTrack <- ifelse(data$ApplicantTrack < 4, 3, data$ApplicantTrack)
  table(data$Gender, data$ApplicantTrack)
  
  t <- xtabs(~ Gender+ApplicantTrack, data=data)
  
  spineplot(t)
  lbl_test(t)
  # p-value = 4.459e-06 -> reject H0 -> no independence?
  
  # Compare to chi-square test without ordered categories
  chisq_test(t)   # p-value = 7.138e-05
  
  
  ## Project:
  
  data$ProjectAssessment <- ifelse(data$ProjectAssessment < 4, 3, data$ProjectAssessment)
  table(data$Gender, data$ProjectAssessment)
  
  t <- xtabs(~ Gender+ProjectAssessment, data=data)
  
  spineplot(t)
  lbl_test(t)
  # p-value = 6.537e-05 -> reject H0 -> no independence?
  
  # Compare to chi-square test without ordered categories
  chisq_test(t)   # p-value = 0.001099
  