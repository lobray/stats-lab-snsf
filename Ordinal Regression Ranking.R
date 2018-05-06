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
data<- subset(internal_regression_data,select = -c(ProjectID, AmountRequested))



# Ranking ----

# Visualization----
ggplot(internal_regression_data, aes(x = Ranking, y = ApplicantTrack, col=ProjectAssessment)) +
  geom_jitter(alpha = .5) +
  #facet_grid(InstType ~ Division, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))    

# Fit a full model ----  

fit <- clm(Ranking ~Gender*(Division+PercentFemale)+ ProjectAssessment+ApplicantTrack+
              Age+IsContinuation+PreviousRequest+InstType+Semester+logAmount,
            data=data)

summary(fit)

# Variable Selection -----

drop1(fit, test = "Chi")

fit<-update(fit,.~.-Age)
fit<-update(fit,.~.-Gender:PercentFemale)
fit<-update(fit,.~.-logAmount)
fit<-update(fit,.~.-InstType)
fit<-update(fit,.~.-Gender:Division)

# After variable Selection, the final model ----   

Model.Ranking<-clm(Ranking ~ Gender + Division + PercentFemale + ProjectAssessment + 
                     ApplicantTrack + IsContinuation + PreviousRequest + Semester, data=data)
summary(Model.Ranking)

# fit a null Model
fit.null<- clm(Ranking~1, data=data)
fit.null$df.residual- Model.Ranking$df.residual

LR<--2*(fit.null$logLik-Model.Ranking$logLik)
1-pchisq(LR,df=17)

anova(fit.null,Model.Ranking)  # gives the same result. Our model explains better than the empty model

# Odds ratio and confidence intervals for Odds Ratio
round(exp(cbind(OR=Model.Ranking$beta,confint(Model.Ranking))), 2)

convergence(Model.Ranking)

# Variable Importance----

outcomeName <- 'Ranking'
predictorNames <- c('Gender', 'Division','PercentFemale','ProjectAssessment',
                    'ApplicantTrack','IsContinuation','PreviousRequest','Semester')

# Initialize vectors for randomization
ll.Reference<-Model.Ranking$logLik                           # Reference LogLikelihood
k <- length(coef(Model.Ranking))                             # Number of estimated parameters
AIC.Reference<-(-2*Model.Ranking$logLik+2*k)                 # Reference AIC 

# We shuffle and refit. If we have a smaller logLike -> the model is worst -> Variable is important
# We shuffle and refit. If we have a bigger AIC -> the model is worst -> variable is important
shuffletimes <- 10  #number of interactions

featuresMeanLR <- featuresMeanAIC <- ll.featuresProportions<-AIC.featuresProportions<-c()
AIC.percentvariation <-c()
for (feature in predictorNames) {
  featureLl <- c()
  featureAIC<-c()
  shuffledData <- data[,c(outcomeName, predictorNames)]
  for (iter in 1:shuffletimes) {
    shuffledData[,feature] <- sample(shuffledData[,feature], length(shuffledData[,feature]))
    Model.tmp <- update(Model.Ranking,.~.,data=shuffledData)
    featureLl <- c(featureLl,Model.tmp$logLik)
    featureAIC <- c(featureAIC, (-2*Model.tmp$logLik+2*k) )
  }
  featuresMeanLR <- c(featuresMeanLR, mean((ll.Reference-featureLl)))
  featuresMeanAIC<- c(featuresMeanAIC, mean(featureAIC-AIC.Reference))
  ll.featuresProportions<-c(ll.featuresProportions,mean(ll.Reference>featureLl))
  AIC.featuresProportions<-c(AIC.featuresProportions,mean(AIC.Reference<featureAIC))
  AIC.percentvariation <- c(AIC.percentvariation, (mean(featureAIC)/AIC.Reference-1)*100)
  # Proportions -> In how many runs does the Reference is less than the shuffled
}

#Ov.Shuffle.Result <- data.frame('feature'=predictorNames, 'Diff.Ll'=featuresMeanLR,
#                              'Proportion.Ll'=ll.featuresProportions, 'Diff.AIC'=featuresMeanAIC,
#                              'Proportion.AIC'=AIC.featuresProportions)
Shuffle.Result <- data.frame('Feature'=predictorNames, 'Diff.AIC'=featuresMeanAIC,
                                'Proportion.AIC'=AIC.featuresProportions,
                                'Percent.AIC'=AIC.percentvariation)

Shuffle.Result <- Shuffle.Result[order(Shuffle.Result$'Diff.AIC', decreasing=TRUE),]
print(Shuffle.Result)


mycol=colorRampPalette(c("blue","blue"))(length(predictorNames))
#par(las=2,mfrow=c(1,1),mai=c(2,2,2,2))
barplot(Shuffle.Result[,4], names.arg = Shuffle.Result[,1],
        horiz = TRUE,col = mycol,cex.names = 0.6,
        main="Variable Importance of Ranking",
        xlab = "Percent of variation on the AIC",
        xlim = c(-10,120),
        cex.axis = 0.8,
        cex.main=1)
