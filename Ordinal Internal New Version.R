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

fit <- clm(ProjectAssessment ~Gender*Division+Gender*PercentFemale+Division*PercentFemale+Age+Division+IsContinuation+
             PreviousRequest+InstType+Semester+logAmount,
           data=data)
summary(fit)

# cond.H very large

## Fit the null Model:

fit.null<- clm(ProjectAssessment~1, data=data)

fit.null$df.residual- fit$df.residual
LR<-(-2*(fit.null$logLik-fit$logLik))  # Yesteday, we did this wrong...
1-pchisq(LR,df=17)
# p-value=0

# check with anova function
anova(fit.null,fit)

# So our model is better than the null model... 

# I fit the same model without Gender and compare it with fit:

fit2 <- clm(ProjectAssessment ~ Division*PercentFemale+Age+Division+IsContinuation+
              PreviousRequest+InstType+Semester+logAmount,
            data=data)
summary(fit2)

fit$df.residual- fit2$df.residual
LR<-(-2*(fit2$logLik-fit$logLik))
1-pchisq(LR,df=4)
# p-value= 0.2321687

# check with anova function
anova(fit2,fit)

# We don't need to include Gender in the model -> Good!
# Gender has no predictive power for ProjectAssessment

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

# Interpretation: I found this in clm_tutorial Janine cited in her mail
# "the odds ratio of bitterness being rated in category j or above at warm relative 
#  to cold temperatures is exp(coef(fm1)[5])"

# In our case I think it would be something like:
# the log odds of ProjectAssessment being rated in category j or above decreases by 22% being a female
exp(-0.247031)
# [1] 0.7811165
exp( 0.737721)
# the odds ratio of ProjectAssessment being rated in category j or above when the project is a
# continuation is 2.091164 (it's two times more likely to be rated in j or above)

## Compare this model with the one without Gender:

Model2<- clm(ProjectAssessment ~ Division*PercentFemale + IsContinuation + 
               Age + logAmount + InstType ,data=data)

Model2$df.residual- Model$df.residual
LR<-Model2$logLik/Model$logLik
1-pchisq(LR,df=1)
# 0.3170705

# Again, we don't need to include Gender! 
# No predictive power! (Good)

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

# From the same clm_tutorial: 
# "To determine the accuracy of the parameter estimates we use the convergence method"
convergence(Model)
# The most important information is the number of correct decimals (Cor.Dec) and the number
# of significant digits (Sig.Dig) with which the parameters are determined. 
# The logLik.error shows that the error in the reported value of the log-likelihood
# is below 10???10, which is by far small enough that LR tests based on this model are accurate.


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

fit <- clm(ApplicantTrack ~Gender*(Division+PercentFemale)+Division:PercentFemale+Age+Division+IsContinuation+
             PreviousRequest+InstType+Semester+logAmount,
           data=data)
summary(fit)
# cond.H very large

## Fit the null Model:

fit.null<- clm(ApplicantTrack~1, data=data)

fit.null$df.residual- fit$df.residual
LR<-(-2*(fit.null$logLik-fit$logLik))  # Yesteday, we did this wrong...
1-pchisq(LR,df=17)
# p-value=0

# check with anova function
anova(fit.null,fit)

# So our model is better than the null model... 

# I fit the same model without Gender and compare it with fit:

fit2 <- clm(ApplicantTrack ~ Division*PercentFemale+Age+Division+IsContinuation+
              PreviousRequest+InstType+Semester+logAmount,
            data=data)

fit2$df.residual- fit$df.residual
LR<-(-2*(fit2$logLik-fit$logLik))
1-pchisq(LR,df=4)
# p-value= 0.00121072

# check with anova function
anova(fit2,fit)

# It seems we need to include Gender in the model -> Bad!
# Gender has some predictive power for ApplicantTrack...


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

## Compare this model with the one without Gender:

Model2<- clm(ApplicantTrack ~ Division + PercentFemale + IsContinuation + 
               Semester + logAmount + InstType ,data=data)

Model2$df.residual- Model$df.residual
LR<-Model2$logLik/Model$logLik
1-pchisq(LR,df=3)
# 0.8000558

# Again, we don't need to include Gender! 
# No predictive power for ApplicantTrack (when removing useless variables)! (Good)


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


###############################################
######   Ordinal regression for Ranking  ######
###############################################

## Get the Regression data:

internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
# internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))
internal_regression_data$logAmount <- log(internal_regression_data$AmountRequested)
data<- subset(internal_regression_data,select = -c(ProjectID, AmountRequested))

# str(data)

## Visualization:

ggplot(internal_regression_data, aes(x = Ranking, y = PercentFemale, col=Gender)) +
  geom_jitter(alpha = .5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

## Fit full the model:

fit <- clm(Ranking ~Gender*Division+Gender*PercentFemale+Division*PercentFemale+Age+Division+IsContinuation+
             PreviousRequest+InstType+Semester+logAmount,
           data=data)
summary(fit)

# cond.H very large

## Fit the null Model:

fit.null<- clm(ProjectAssessment~1, data=data)

fit.null$df.residual- fit$df.residual
LR<-(-2*(fit.null$logLik-fit$logLik))  # Yesteday, we did this wrong...
1-pchisq(LR,df=17)
# p-value=0

# check with anova function
anova(fit.null,fit)

# So our model is better than the null model... 

# I fit the same model without Gender and compare it with fit:

fit2 <- clm(Ranking ~ Division*PercentFemale+Age+Division+IsContinuation+
              PreviousRequest+InstType+Semester+logAmount,
            data=data)
summary(fit2)

fit$df.residual- fit2$df.residual
LR<-(-2*(fit2$logLik-fit$logLik))
1-pchisq(LR,df=4)
# p-value= 0.08703665

# check with anova function
anova(fit2,fit)

# We don't need to include Gender in the model -> Good!
# Gender has no predictive power for ProjectAssessment

## Variable selection (AIC):

drop1(fit, test = "Chi")
fit <- update(fit, .~.-Gender:Division)
fit <- update(fit, .~.-Division:PercentFemale)
fit <- update(fit, .~.-Gender:PercentFemale)
fit <- update(fit, .~.-Age)
fit <- update(fit, .~.-Semester)

# Final model:

Model<- clm(Ranking ~ Gender + Division + PercentFemale + IsContinuation + 
              PreviousRequest + InstType + logAmount ,data=data)
summary(Model)

# Still huge cond.H


## Compare this model with the one without Gender:

Model2<- clm(Ranking ~ Division + PercentFemale + IsContinuation + 
               PreviousRequest + InstType + logAmount ,data=data)

Model2$df.residual- Model$df.residual
LR<-Model2$logLik/Model$logLik
1-pchisq(LR,df=1)
# 0.3168946

# Again, we don't need to include Gender! 
# No predictive power! (Good)

