##################################################################
#####  Set of Functions for cleaning the data                #####
##################################################################


# libraries used 
library(tidyr)
library(plotly)
library(ggplot2)
library(lsmeans)
library(lubridate)
library(ISOcodes)
library(stringr)



# APPLICATIONS ------------------------------------------------------------

d.apps<- function(data){
  
  apps<-data    #make a copy  
  
  ### Simplify a few variable names in apps data set
  
  colnames(apps)[colnames(apps)=="ResponsibleApplicantAcademicAgeAtSubmission"]<-"AcademicAge" # Applicant Academic age at submission. Starting on 2016-10-01
  colnames(apps)[colnames(apps)=="ResponsibleApplicantAgeAtSubmission"]<-"Age"   # Applicant age at submission
  colnames(apps)[colnames(apps)=="IsHasPreviousProjectRequested"] <- "PreviousRequest"   # Rename previous project Request
  colnames(apps)[colnames(apps)=="ResponsibleApplicantProfessorshipType"]<- "Professorship" # Simplify name
  colnames(apps)[colnames(apps)=="NationalityIsoCode"]<- "Nationality" # Simplify name
  
  ### Convert categorical variables to factor in apps
  
  apps$IsApproved<-as.factor(apps$IsApproved) # For Approvals
  apps$Gender<-as.factor(apps$Gender) # For Gender
  apps$Division<-as.factor(apps$Division) # For Division
  apps$IsContinuation<-as.factor(apps$IsContinuation) # Continuation
  apps$InstType<-as.factor(apps$InstType) # Institution   
  apps$Professorship<-as.factor(apps$Professorship) # Professorship
  apps$PreviousRequest<-as.factor(apps$PreviousRequest) # PreviousRequest
  
  
  ### Set `NA` to zero in granted when approved = 0
  
  id<-(is.na(apps$AmountGranted)&apps$IsApproved==0)
  apps$AmountGranted[id] <- 0
  
  
  ### Simplify Date in apps data
  
  apps$CallEndDate<-ymd(apps$CallEndDate)
  colnames(apps)[colnames(apps)=="CallEndDate"]<- "Year"
  
  
  ### Simplyfy Main Disipline 2
  apps<-separate(apps, MainDisciplineLevel2, c("MD Code", "MD Name"),
                 sep=" ",extra = "merge", fill = "right") 
  apps$`MD Code`<-as.factor(apps$`MD Code`) 
  apps$`MD Name`<-as.factor(apps$`MD Name`) 
  
  
  # Drop unnecesary variables   
  drops <- c("MainDiscipline","CallTitle", "MD Code")
  apps <- apps[ , !(names(apps) %in% drops)]
  
  # Removed NAs for gender since we are interested in it
  id.g <- which(is.na(apps$Gender)) 
  apps <- apps[-id.g,]
  
  # Reorder data set
  apps<- apps[,c("ProjectID","Gender","Age","Nationality","AcademicAge","Professorship", "IsApproved","Year", "AmountRequested" ,
                 "AmountGranted", "GradeFinal","Division","MD Name","InstType",
                 "PreviousRequest","IsContinuation")]
  
  return(apps)
  
}
                       

# EXTERNAL REVIEWERS -------------------------------------------------------


d.reviews <- function(data){
  
  external_reviews<-data  #make a copy
  
  ### Fix obvious data errors: QuestionRating
  
  id <- which(external_reviews$QuestionRating=="-outstanding")
  external_reviews$QuestionRating[id]<-"outstanding"
  
  ### Following Flabio recomendation. Ignore the "0" and "not considered", what do you think?
  
  id.o<-which(external_reviews$QuestionRating=="0")
  external_reviews<-external_reviews[-id.o,]
  id.o2<-which(external_reviews$OverallGrade=="not considered")
  external_reviews<-external_reviews[-id.o2,]
  
  
  ### Convert categorical variables to factors
  
  external_reviews$Gender<-as.factor(external_reviews$Gender) # Gender
  external_reviews$OverallGrade<-as.factor(external_reviews$OverallGrade) #OverallGrade
  external_reviews$QuestionRating<-as.factor(external_reviews$QuestionRating) #QuestionRating
  external_reviews$SourcePerson<-as.factor(external_reviews$SourcePerson) #Source Person -- Who suggested the reviewer?
 
  
  ### Simplify a few variable names in reviewers data set
  
  colnames(external_reviews)[colnames(external_reviews)=="Gender"]<-"ReviewerGender"
  colnames(external_reviews)[colnames(external_reviews)=="Country"] <- "ReviewerCountry" # specify country as ReviewerCountry
  
  
  ### Using Emailending to set Country
  
  IsoCountry <- ISO_3166_1[,c("Alpha_2","Name")]  #UK Is not in this data Set
  IsoCountry <- rbind(IsoCountry, c(Alpha_2="UK", Name="Great Britain and Northern Ireland"))
  
  external_reviews$EmailEnding<-toupper(external_reviews$EmailEnding)  
  for (i in IsoCountry$Alpha_2){ 
    id <- which(str_detect(external_reviews$EmailEnding,i)&is.na(external_reviews$ReviewerCountry))
    id_iso <- which(str_detect(IsoCountry$Alpha_2,i)==1)
    name <- IsoCountry[id_iso,2]
    external_reviews$ReviewerCountry[id] <- name
  }   
  
  
  ###  Spread the Question Variable
  
  external_reviews<-spread(external_reviews,Question,QuestionRating)
  
  mynames<-c("OverallGrade", "SourcePerson","ReviewerGender","ReviewerCountry",
             "EmailEnding","ProjectID","ReviewerID","ReviewID","ApplicantTrack","BroaderImpact",
             "ScientificRelevance","Suitability")
  colnames(external_reviews)<-mynames
  
  ###  Rearrenge columns
  
  external_reviews<-external_reviews[,c("ProjectID","SourcePerson","ReviewerID","ReviewID","ReviewerGender","ReviewerCountry",
                            "EmailEnding","ApplicantTrack","BroaderImpact",
                            "ScientificRelevance","Suitability","OverallGrade")]
  
  
  ### Remove variables we don't care about
  
  useless_variables <- colnames(external_reviews) %in% c("BroaderImpact")
  reduced_data <- external_reviews[,!useless_variables]
  
  
  return(reduced_data)
}


# INTERNAL REFEREES -------------------------------------------------------

d.referee<-function(data){
  
  #Make a Copy
  internal_reviews<-data
  
  # Question, Evaluation criterion. I will separete this into columns, so that we have one
  # raw per reviewer per project ID  
  
  internal_reviews<-spread(internal_reviews,Question,QuestionRating)
  
  # Simplify variable names
  
  mynames<-c("Ranking", "RefereeRole","RefereeGender","ProjectID","RefereeID",
             "RecommendationID", "ApplicantTrack", "ProjectAssesment")
  
  names(internal_reviews)<-mynames
  
  
  # Turning variables to factors
  
  internal_reviews$Ranking<-as.factor( internal_reviews$Ranking)#Ranking
  internal_reviews$RefereeRole<-as.factor(internal_reviews$RefereeRole)#Referee Role
  internal_reviews$RefereeGender<-as.factor(internal_reviews$RefereeGender)#RefereeGender
  internal_reviews$ApplicantTrack<-as.factor((internal_reviews$ApplicantTrack)) #ApplicantTrack
  internal_reviews$ProjectAssesment<-as.factor(internal_reviews$ProjectAssesment)#Project assesmet
  
  #Rearrenging columns
  internal_reviews<-internal_reviews[,c("ProjectID","Ranking",
                                      "RefereeID" ,"RefereeRole","RefereeGender","RecommendationID",
                                      "ApplicantTrack","ProjectAssesment")]
  
  return(internal_reviews)
  }

apps2016 <- d.apps(applications)