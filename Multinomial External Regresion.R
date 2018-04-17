source("Cleaning Functions.R")
source("Data for Regression.R")

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



