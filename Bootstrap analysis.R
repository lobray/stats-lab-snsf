source("multinomial.R")
source("Multinomial External Regresion.R")
source("multinomial internal.R")

library(boot)

#Internal Dataset

#Function returning an estimate for α
alpha <- function(formula, data, indices) {data=internal_regression_data # allows boot to select sample 
fit <- InternalMultiModel()
return(coef(fit)) 
}

results <- boot(data=internal_regression_data, statistic= alpha, 
                R=250, formula= ApplicantTrack ~ Gender + Division + InstType + IsContinuation + 
                  log(AmountRequested)+PercentFemale+Gender:Division+ProjectAssessment)

#Alternative way
class(fit7)
b <- Boot(fit7, R = 250)
sqrt(diag(vcov(b))) #This returns the bootstrapped standard errors.
set.seed(1)
summary(b)
vcov(b)

confint(b, level=.95, type="norm")
hist(b, legend="separate")
