
##############################################
## Effects for Applicant Track  - Internal ###
##############################################
load("snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")

library(ordinal)
library(ggplot2)
library(MASS)

rm(applications,reviews,referee_grades, test)

## Get the Regression data:

internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)
# internal_regression_data$Semester <- revalue(internal_regression_data$Semester, c("ottobre"="Oct", "aprile"="Apr"))
internal_regression_data$logAmount <- log(internal_regression_data$AmountRequested)
data<- subset(internal_regression_data,select = -c(ProjectID, Ranking, AmountRequested))


# Final model:

Model<- clm(ApplicantTrack ~ Gender*Division + PercentFemale + IsContinuation + 
              logAmount + InstType , Hess=TRUE, data=data)
summary(Model)

X<-Model$model[,-1]

# Estimate an average application
  # Division
    table(X$Division)
    round(mean(as.numeric(X$Division)),0) #2
    
  # PercentFemale
    table(X$PercentFemale)
    round(mean(as.numeric(X$PercentFemale)),0) #0
    
  # IsContinuation
    table(X$IsContinuation)  #0
    
  # logAmount
    mean(X$logAmount) #13.02518
    
  # InstType
    table(X$InstType)  #Uni
   

#Predicted probabilities for woman
    newdata<-data.frame(Gender="f",
                        Division="Div 2",
                        PercentFemale=0,
                        IsContinuation="0",
                        logAmount=13.02518,
                        InstType="Uni")


fitted <- predict(Model,newdata=newdata, type = "cum.prob")
f.cum.prob<-fitted$cprob2
fitted <- predict(Model,newdata=newdata)
f.prob<-fitted$fit

#Predicted probabilities for men
newdata<-data.frame(Gender="m",
                    Division="Div 2",
                    PercentFemale=0,
                    IsContinuation="0",
                    logAmount=13.02518,
                    InstType="Uni")

fitted <- predict(Model,newdata=newdata, type = "cum.prob")
m.cum.prob<-fitted$cprob2
fitted <- predict(Model,newdata=newdata)
m.prob<-fitted$fit

#cumulative Probabilities
gender.cum.prob.track<-rbind(m.cum.prob,
                             f.cum.prob)


rownames(gender.cum.prob.track)<-c("Male","Female")
colnames(gender.cum.prob.track)<-c("poor","average","good", "very good",
                               "excellent","outstanding")

gender.cum.prob.track<-t(gender.cum.prob.track)

gender.cum.prob.track<-cbind(round(gender.cum.prob.track,3),
                             Difference=round((gender.cum.prob.track[,1]-gender.cum.prob.track[,2]),3))
gender.cum.prob.track<-as.data.frame(gender.cum.prob.track)

ggplot(gender.cum.prob.track) +
  geom_line(aes(1:6, Male, colour = "Male")) +
  geom_line(aes(1:6, Female, colour = "Female"))+
  theme(legend.title=element_blank(),legend.position = "bottom")+
  xlab("")+ylab("Cumulative Probability")+
  ggtitle("Cumulative Probability for Applicant Track")



# If it helps to understand, the same as before but for Probabilities, not cumulatives

gender.prob.track<-rbind(m.prob,
                        f.prob)


rownames(gender.prob.track)<-c("Male","Female")
colnames(gender.prob.track)<-c("poor","average","good", "very good",
                                   "excellent","outstanding")

gender.prob.track<-t(gender.prob.track)

gender.prob.track<-cbind(round(gender.prob.track,3),
                             Difference=round((gender.prob.track[,1]-gender.prob.track[,2]),3))
gender.prob.track<-as.data.frame(gender.prob.track)

ggplot(gender.prob.track) +
  geom_line(aes(1:6, Male, colour = "Male")) +
  geom_line(aes(1:6, Female, colour = "Female"))+
  theme(legend.title=element_blank(),legend.position = "bottom")+
  xlab("")+ylab("Cumulative Probability")+
  ggtitle("Predicted Probability for Applicant Track")
