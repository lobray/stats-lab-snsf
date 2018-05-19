#####################################################################
######      Regressions for external reviews - version 2      #######
#####################################################################

# Initialize data
load("snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

library(biostatUZH)
library(psy)
library(psych)
library(caret)

rm(applications,reviews,referee_grades, test)

# Obtain Regression Data
external_regression_data<-prepare_data_external_log_regression(final.apps,final.external)

str(external_regression_data)
summary(external_regression_data)


# Logistic Regression -----------------------------------------------------


external_regression_data$logAmount<-log(external_regression_data$AmountRequested)
data<- subset(external_regression_data,select = -c(ProjectID, OverallGrade, AmountRequested, 
                                                   ProposalCombined))

# visualization of grades to check for perfect separation----
  par(mfrow=c(2,2))
  
#ApplicantTrack
  barplot(table(data$IsApproved,data$ApplicantTrack),col=c("red","green"), beside=TRUE,
          main = "Applicant Track Grade vs. Approval")
  
  legend("topleft",bty="n", legend=c("Not Approved","Approved"),lty=2,col=c("red","green"))
  
  # ScientificRelevance
  barplot(table(data$IsApproved,data$ScientificRelevance),col=c("red","green"), beside=TRUE,
          main = "ScientificRelevance vs. Approval")
  
  legend("topleft",bty="n", legend=c("Not Approved","Approved"),lty=2,col=c("red","green"))
  
  # Suitability
  barplot(table(data$IsApproved,data$Suitability),col=c("brown2","turquoise3"), beside=TRUE,
          main = "Suitability vs. Approval")
  
  
  
  
  legend("topleft",bty="n", legend=c("Not Approved","Approved"),lty=2,col=c("red","green"))
  
  # ProposalCombined
  barplot(table(data$IsApproved,data$ProposalCombined),col=c("red","green"), beside=TRUE,
          main = "ProposalCombined vs. Approval")
  
  legend("topleft",bty="n", legend=c("Not Approved","Approved"),lty=2,col=c("red","green"))

  par(mfrow=c(1,1))

# As we define the grades as ordered factors, contrast.poly() is used as default to compute the
# coefficient estimates

# Fit first model with all variables and interactions----
  Model <- glm(IsApproved ~ .+Gender:Division+
               Gender:PercentFemale+Gender:ApplicantTrack+InstType:Division ,data=data, 
             family="binomial")
  summary(Model)
  # We have NA, and coefficients with large variances. 

# Model Diagnostic
    # Predicted values
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
      
    # Kappa Statistic
      # Define weight matrix
        linear.weights <- matrix(c(1, 0.8, 0.8, 1), 
                               nrow=2, ncol=2, byrow=T)
      
        kappa.matrix <- as.matrix(table(data$IsApproved,pred>=0.5))
        kappa <- cohen.kappa(kappa.matrix, w=linear.weights)  # 0.5
        kappa
    # Pseudos-R^2
        (PsR22<-(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))) #[1] 0.4198219
    
    
    # optimize model see if we can drop some variables
      drop1(Model, test="Chisq")
      
      # Removing interaction between ApplicantTrack and gender reduces the AIC the most
      # I will remove this variable
        Model<-update(Model,.~.-ApplicantTrack:Gender) 
        Model<-update(Model,.~.-Division:Gender)   
        #Model<-update(Model,.~.-InstType)  # With the interaction this is significant
        Model<-update(Model,.~.-logAmount)
        Model<-update(Model,.~.-PercentFemale:Gender)
        Model<-update(Model,.~.-PreviousRequest)
        
        drop1(Model,test="Chisq")
        summary(Model)
        # We stop before removing Gender
        # No more NA, but still very large variances
        
        # Pseudos-R^2
        (PsR22<-(1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623))) #[1]  0.4165827
        
        # Although AIC is less, the pseudo r's are worst.
        Model$formula
        
# No matter what assumption of contrast we use the model is not good. I am guessing 
# this is because we have a perfect separation problem:
        
        with(data,table(IsApproved,ApplicantTrack))

# I will combine applicant track in levels ">=3","4","5","6"
        data$ApplicantTrack<-ifelse(data$ApplicantTrack<=3,3,data$ApplicantTrack)
        data$ApplicantTrack<-factor(data$ApplicantTrack, ordered=TRUE)
        
# For ScientificRelevance
        with(data,table(IsApproved,ScientificRelevance)) # Same Thing
        data$ScientificRelevance<-ifelse(data$ScientificRelevance<=3,3,data$ScientificRelevance)
        data$ScientificRelevance<-factor(data$ScientificRelevance, ordered=TRUE)
        
# For Suitability
        with(data,table(IsApproved,Suitability)) # Same Thing
        data$Suitability<-ifelse(data$Suitability<=3,3,data$Suitability)
        data$Suitability<-factor(data$Suitability, ordered=TRUE)
        
# Fit again with new definition and all variables:
        
        Model <- glm(IsApproved ~ .+Gender:Division+
                       Gender:PercentFemale+Gender:ApplicantTrack,data=data, 
                   family="binomial")
        summary(Model)
        # We have no NA's now, and the variance are reasonable
        # The problem seems to be fixed
        
        # Model Diagnostic
        # Predicted values
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
        
        
        # Kappa Statistic
        # Define weight matrix
        linear.weights <- matrix(c(1, 0.8, 0.8, 1), 
                                 nrow=2, ncol=2, byrow=T)
        
        kappa.matrix <- as.matrix(table(data$IsApproved,pred>=0.5))
        kappa <- cohen.kappa(kappa.matrix, w=linear.weights) 
        kappa  # again 0.51
        
        # Pseudos-R^2
        (1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623)) #[1] 0.4155873
 
        # optimize model
        drop1(Model, test="Chisq")
        Model <- update(Model,.~.-ApplicantTrack:Gender)
        Model <- update(Model,.~.-Gender:Division)
        Model <- update(Model,.~.-logAmount)
        Model <- update(Model,.~.- PreviousRequest)
        Model <- update(Model,.~.-PercentFemale:Gender)
        # Again, Removing interaction between ApplicantTrack and gender reduces the AIC the most
        # I will remove this variable
        
        summary(Model)
        # Pseudos-R^2
        (1-exp((Model$dev-Model$null)/1623))/(1-exp(-Model$null/1623)) # [1] 0.4203388
        # Although AIC is less, the pseudo r's are worst. But is to be expected, as the more
        # Variables the less the deviance, and therfore, the less proportion explained
      
        # In the following I will work with this final model
        Model$call
        # glm(formula = IsApproved ~ ApplicantTrack + ScientificRelevance + Suitability + 
        # PercentFemale + Age + Gender + Division + IsContinuation + InstType + 
        # Semester + Division:InstType, 
        # family = "binomial", data = data)
       
# Effect Visualization ----------------------------------------------------

library(effects)
library(car)        
library(MASS)        
library(splines)
library(ggplot2)

# Obtain all the effects        
  eff.fit <- allEffects(Model)
  
# Obtain Gender effect as a separate data frame 
  eff<-Effect("Gender",Model)
  eff<-eff$fit
  prob<- round((exp(eff)/(1+exp(eff)))*100,2)
  
  plot(Effect("Gender",Model),
       confint=list(style="band",alpha=0.3,col="grey"),
       lines=list(col=1))
  # Lines are almost horizotal, an indication of no gender effect. Good news!!
  
  Males.data<- subset(data, Gender=="m")
  Females.data<-subset(data,Gender=="f")
  
  predicted.prob.male<-predict(Model, newdata=Males.data, type="response")
  pp<-mean(predicted.prob.male)
  
  predicted.prob.female<-predict(Model, newdata=Females.data, type="response")
  pf<-mean(predicted.prob.female)
  
Average.Data<- colMeans(data)
  
# Plot different things
  #Gender effect on the approval of applications per division  
      plot(Effect(focal.predictors = c("Gender","Division"), Model),
      rug=FALSE,style="stacked", ci.style="bands")
  
  # All effects at once
    plot(eff.fit,
         confint=list(style="band",alpha=0.2),
         lines=list(col=1,lwd=1))


# Visualization with effects package and others----
  
    plot(Effect(c("Gender","ApplicantTrack"), mod=Model))
  # Males and females is the same graph to compare better. There might be a simpler way..
      # 1. Get the effect
      GApp<-Effect(c("Gender","ApplicantTrack"), mod=Model)
      GApp
      # 2. GApp is a very big list, and I couldn't find a place where they store the
      # values as display (the ones they plot), but I was able to reproduce it.
      GA<-matrix(exp(GApp$fit)/(1+exp(GApp$fit)),nrow = 2,byrow = F)
      rownames(GA)<-levels(data$Gender)
      colnames(GA)<-levels(data$ApplicantTrack)
      
      x<-as.numeric(levels(data$ApplicantTrack))
      plot(x,GA[1,], type="l", col= "blue",
           main="Gender Effect on Applicant Track",
           xlab="Reviewers Grade",
           ylab="Probability of being Approved",
           xaxp=c(3,6,3))
      points(x,GA[1,],pch=20)
      points(x,GA[2,],pch=20)
      lines(x,GA[2,], type="l", col="green")
      legend("topleft", lty=1, bty="n", col=c("blue","green"), legend = c("male","female"))
      
  # Gender vs Scientific Relevance
  plot(Effect(c("Gender","ScientificRelevance"), mod=Model))
      # 1. Get the effect
      GSci<-Effect(c("Gender","ScientificRelevance"), mod=Model)
      GSci
      # 2. GApp is a very big list, and I couldn't find a place where they store the
      # values as display (the ones they plot), but I was able to reproduce it.
      GSc<-matrix(exp(GSci$fit)/(1+exp(GSci$fit)),nrow = 2,byrow = F)
      rownames(GSc)<-levels(data$Gender)
      colnames(GSc)<-levels(data$ScientificRelevance)
      GSc
      # 3. plot
      x<-as.numeric(levels(data$ScientificRelevance))
      plot(x,GSc[1,], type="l", col= "blue",
           main="Gender Effect on Scientific Relevance",
           xlab="Reviewers Grade",
           ylab="Probability of being Approved",
           xaxp=c(3,6,3))
      points(x,GSc[1,],pch=20)
      points(x,GSc[2,],pch=20)
      
      
      lines(x,GSc[2,], type="l", col="green")
      legend("topleft", lty=1, col=c("blue","green"), legend = c("male","female"))
    
  # Gender vs Suitability    
  plot(Effect(c("Gender","Suitability"), mod=Model))
      GSui<-Effect(c("Gender","Suitability"), mod=Model)
      GSui
      # 2. GApp is a very big list, and I couldn't find a place where they store the
      # values as display (the ones they plot), but I was able to reproduce it.
      GSu<-matrix(exp(GSui$fit)/(1+exp(GSui$fit)),nrow = 2,byrow = F)
      rownames(GSu)<-levels(data$Gender)
      colnames(GSu)<-levels(data$Suitability)
      GSu
      # 3. plot
      x<-as.numeric(levels(data$Suitability))
      plot(x,GSu[1,], type="l", col= "blue",
           main="Gender Effect on Suitability",
           xlab="Reviewers Grade",
           ylab="Probability of being Approved",
           xaxp=c(3,6,3))
      points(x,GSu[1,],pch=20)
      points(x,GSu[2,],pch=20)
      lines(x,GSu[2,], type="l", col="green")
      legend("topleft", lty=1, col=c("blue","green"), legend = c("male","female"))
  
  

# Plots Gender effect accross divisions 
# another way, still not good Anz ideas to make plots look nicer?

# ggplot(GendDivEff[,which(GendDivEff$Division=="Div 1")],aes(x=Gender, y=fit))+
#  geom_line()

# ggplot(GendDivEff, aes(x=Gender, y=fit)) + 
#  geom_segment(data=GendDivEff, mapping=aes(x=Gender, y=lower, xend=Gender, yend=upper, colour=Division)) 


# Relative variable importance --------------------------------------------

# Reference Score
calc_pseudo_r <- function(Model,n) {
  (1-exp((Model$dev-Model$null)/n))/(1-exp(-Model$null/n))
}


# Set Predictors and output variables
  outcomeName <- 'IsApproved'
  predictorNames <- c('ApplicantTrack','ScientificRelevance', 'Suitability',
                              'PercentFemale','Age','Gender','Division','InstType',
                              'IsContinuation', 'Semester')

# To see variable importance based on pseudo r^2
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
    featuresMeanR2 <- c(featuresMeanR2,round(mean(featureR2-refR2),4))
  }  
  
  PseudoRShuffle <- data.frame('Feature'=predictorNames, 'Importance'=featuresMeanR2)
  #minor detail, I'll order so that the most important variable is at the top
  PseudoRShuffle <- PseudoRShuffle[order(PseudoRShuffle$Importance, decreasing=TRUE),]
  print(PseudoRShuffle)
  
  PseudoRShuffle<-PseudoRShuffle[order(PseudoRShuffle$Importance, decreasing=FALSE),]
  PseudoRShuffle$Importance<-abs(PseudoRShuffle$Importance)
  mycol=colorRampPalette(c("blue","red"))(length(predictorNames))
  
  par(las=2,mfrow=c(1,1),mai=c(1,2,1,1))
  barplot(PseudoRShuffle[,2], names.arg = PseudoRShuffle[,1],
          horiz = TRUE,col = mycol,cex.names = 0.7,
          main="Variable Importance for Approval",
          xlab = "Percent of variation on the Pseudo R",
          cex.axis = 0.6,
          cex.main=0.8)
