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
library(plyr)
library(grid)
library(vcd)
library(corrplot)
library(ROCR)
library(car)



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
  apps$Gender<-relevel(apps$Gender, ref="m")
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
  drops <- c("MainDiscipline","CallTitle", "MD Code", "Professorship", "AcademicAge")
  apps <- apps[ , !(names(apps) %in% drops)]
  
  # Removed NAs for gender since we are interested in it
  id.g <- which(is.na(apps$Gender)) 
  apps <- apps[-id.g,]
  
  # Reorder data set
  apps<- apps[,c("ProjectID","Gender","Age","Nationality", "IsApproved","Year", "AmountRequested" ,
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
  d.levels<-c("poor","average","good","very good","excellent","outstanding")
  
  external_reviews$Gender<-factor(external_reviews$Gender) # Gender
  external_reviews$OverallGrade<-factor(external_reviews$OverallGrade, levels = d.levels) #OverallGrade
  external_reviews$QuestionRating<-factor(external_reviews$QuestionRating, levels = d.levels) #QuestionRating
  external_reviews$SourcePerson<-factor(external_reviews$SourcePerson) #Source Person -- Who suggested the reviewer?
 
  
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
             "RecommendationID", "ApplicantTrack", "ProjectAssessment")
  
  names(internal_reviews)<-mynames
  
  # Delete observations with no grades
  
  id<-which(is.na(internal_reviews$Ranking)&is.na(internal_reviews$ApplicantTrack)
            &is.na(internal_reviews$ProjectAssessment))
  internal_reviews<-internal_reviews[-id,]
  
  # Turning variables to factors
  d.levels<-c("poor","average","good","very good","excellent","outstanding")
  
  internal_reviews$Ranking<-factor( internal_reviews$Ranking)#Ranking
  internal_reviews$Ranking <- factor(internal_reviews$Ranking, levels=rev(levels(internal_reviews$Ranking)))
  internal_reviews$RefereeRole<-factor(internal_reviews$RefereeRole)#Referee Role
  internal_reviews$RefereeGender<-factor(internal_reviews$RefereeGender)#RefereeGender
  internal_reviews$ApplicantTrack<-factor(internal_reviews$ApplicantTrack,levels = d.levels) #ApplicantTrack
  internal_reviews$ProjectAssessment<-factor(internal_reviews$ProjectAssessment,levels = d.levels)#Project assesmet
  
  #Rearrenging columns
  internal_reviews<-internal_reviews[,c("ProjectID","Ranking",
                                      "RefereeID" ,"RefereeRole","RefereeGender","RecommendationID",
                                      "ApplicantTrack","ProjectAssessment")]
  
  ### Remove variables we don't care about
  
  useless_variables <- colnames(internal_reviews) %in% c("RefereeRole")
  reduced_data <- internal_reviews[,!useless_variables]
  
  return(reduced_data)
  }


apps <- d.apps(applications)
external_reviews <- d.reviews(reviews)
internal_reviews <- d.referee(referee_grades)


# SELECT THE YEAR and COMPLETE OBSERVATIONS ----------------------------------------------

select.f <- function(data1,data2) {
  rows.1 <- match(data2$ProjectID, data1$ProjectID) # get apps rows corresponding to reviews
  rows.2 <- which(is.na(rows.1)==FALSE)  # get rows number of reviews that are in apps2016
  data2 <- data2[rows.2,] # select rows 
  return(data2)
}

selection.function <- function(apps,reviews,referee,year){
  apps2016<-subset(apps, year(apps$Year)==year)
  Internal2016<-merge(referee, apps2016[,c("ProjectID","Gender","Year")],
                      by="ProjectID")
  External2016<-merge(reviews, apps2016[,c("ProjectID","Gender","Year")],
                      by="ProjectID")
  complete.apps <- select.f(External2016, apps2016)
  complete.Internal <- select.f(External2016, Internal2016)
  complete.External <- External2016
  return(list(final.apps=complete.apps, final.reviews=complete.External, 
              final.referees=complete.Internal))
}

test <- selection.function(apps,external_reviews,internal_reviews,2016)

# To access one single dataset:
# f.apps <- test$final.apps
# f.reviews <- test$final.reviews
# f.referees <- test$final.referees
  


calculate_percent_female <- function(data, ReviewerTypeGender = "RefereeGender") {
  ### Function to calculate the percent of females evaluating a given proposal.
  ### Returns a matrix with projectID and % female reviewers.
  ### Works with both internal & external reviewer data sets, but requires
  ### the column name of ReviewerGender / RefereeGender to be specified 
  ### since they aren't the same.
  
  
  # Get vector of unique Project IDs
  projID <- unique(data$ProjectID)
  
  # Create empty matrix to store % female
  percent_female_matrix <- matrix(0, nrow=length(unique(data$ProjectID)), ncol=2)
  
  # Initialize j
  j <- 1
  
  for (i in projID) {
    
    # Count number of reviewers per project ID
    number_reviewers <- dim(data[which(data$ProjectID == i),])[1] 
    
    # Input the project id into the % female matrix
    percent_female_matrix[j,1] <- i 
    
    # Get a vector of just the genders
    reviewer_genders_per_projectID <- data[which(data$ProjectID == i),ReviewerTypeGender] 
    
    # Count number of females and divide by number of reviewers
    percent_female <- sum(reviewer_genders_per_projectID=="f") / number_reviewers 
    
    # Put percent female into the matrix
    percent_female_matrix[j,2] <- percent_female 
    
    # Increment j
    j <- j+1
  }
  percent_female_matrix <- as.data.frame(percent_female_matrix)
  colnames(percent_female_matrix) <- c("ProjectID", "PercentFemale")
  return(percent_female_matrix)
}



# FUNCTIONS TO AVERAGE GRADES ----------------------------------------------

### Average of referee

calculate_average_referee <- function(data) {


  # Revalue factors on a 1:6 scale and set the variable as numeric for ApplicantTrack, 
  # ProjectAssessment and Ranking
  
    #data$ApplicantTrack <- revalue(data$ApplicantTrack, c("poor"= 1, "average"= 2, "good" = 3, "very good" = 4, "excellent" = 5, "outstanding" = 6))
    data$ApplicantTrack <- as.numeric(data$ApplicantTrack)
    
    #data$ProjectAssessment <- revalue(data$ProjectAssessment, c("poor"= 1, "average"= 2, "good" = 3, "very good" = 4, "excellent" = 5, "outstanding" = 6))
    data$ProjectAssessment <- as.numeric(data$ProjectAssessment)
    
    #data$Ranking <- revalue(data$Ranking, c("D"= 1, "C"= 2, "BC" = 3, "B" = 4, "AB" = 5, "A" = 6))
    data$Ranking <- as.numeric(data$Ranking)
    
  # Create a vector of unique IDs
      projID <- unique(data$ProjectID)
  
  # Create matrix to store the data
      average_review_matrix <- matrix(0, nrow=length(unique(data$ProjectID)), ncol=4)
      
      j <- 1
      for (i in projID) {
        
        # Count number of reviewers per project ID
        number_reviewers <- dim(data[which(data$ProjectID == i),])[1] 
        
        # Input the project id into the matrix
        average_review_matrix[j,1] <- i 
        
        # Get a vector of just the vote for each variable
        reviewer_vote_per_projectID_ApplicantTrack <- data[which(data$ProjectID == i),]$ApplicantTrack
        reviewer_vote_per_projectID_ProjectAssessment <- data[which(data$ProjectID == i),]$ProjectAssessment
        reviewer_vote_per_projectID_Ranking <- data[which(data$ProjectID == i),]$Ranking
        
        # Average grade
        average_ApplicantTrack <- round(mean(reviewer_vote_per_projectID_ApplicantTrack, na.rm = TRUE),0)
        average_ProjectAssessment <-round( mean(reviewer_vote_per_projectID_ProjectAssessment, na.rm = TRUE),0)
        average_Ranking <- round(mean(reviewer_vote_per_projectID_Ranking, na.rm = TRUE),0)
        
        # Put average into the matrix and turn into factors
        average_review_matrix[j,2] <- average_ApplicantTrack
        average_review_matrix[j,3] <- average_ProjectAssessment
        average_review_matrix[j,4] <- average_Ranking
        
        # Increment j
        j <- j+1
      }
      
      
      
      colnames(average_review_matrix)<- c("ProjectID", "ApplicantTrack", "ProjectAssessment", "Ranking")
      return(as.data.frame(average_review_matrix))
    }




### Average of Reviewers

calculate_average_reviewers<- function(data) {
  
  
  # Revalue factors on a 1:6 scale and set the variable as numeric for ApplicantTrack, 
  # ProjectAssessment and Ranking
  
  data$ApplicantTrack <- revalue(data$ApplicantTrack, c("poor"= 1, "average"= 2, "good" = 3, "very good" = 4, "excellent" = 5, "outstanding" = 6))
  data$ApplicantTrack <- as.numeric(data$ApplicantTrack)
  
  data$ScientificRelevance <- revalue(data$ScientificRelevance, c("poor"= 1, "average"= 2, "good" = 3, "very good" = 4, "excellent" = 5, "outstanding" = 6))
  data$ScientificRelevance <- as.numeric(data$ScientificRelevance)
  
  data$Suitability <- revalue(data$Suitability, c("poor"= 1, "average"= 2, "good" = 3, "very good" = 4, "excellent" = 5, "outstanding" = 6))
  data$Suitability <- as.numeric(data$Suitability)
  
  data$OverallGrade <- revalue(data$OverallGrade, c("poor"= 1, "average"= 2, "good" = 3, "very good" = 4, "excellent" = 5, "outstanding" = 6))
  data$OverallGrade <- as.numeric(data$OverallGrade)
  
  # Create a vector of unique IDs
  projID <- unique(data$ProjectID)
  
  # Create matrix to store the data
  average_review_matrix <- matrix(0, nrow=length(unique(data$ProjectID)), ncol=5)
  
  j <- 1
  for (i in projID) {
    
    # Count number of reviewers per project ID
    number_reviewers <- dim(data[which(data$ProjectID == i),])[1] 
    
    # Input the project id into the matrix
    average_review_matrix[j,1] <- i 
    
    # Get a vector of just the vote for each variable
    reviewer_vote_per_projectID_ApplicantTrack <- data[which(data$ProjectID == i),]$ApplicantTrack
    reviewer_vote_per_projectID_ScientificRelevance <- data[which(data$ProjectID == i),]$ScientificRelevance
    reviewer_vote_per_projectID_Suitability <- data[which(data$ProjectID == i),]$Suitability
    reviewer_vote_per_projectID_OverallGrade <- data[which(data$ProjectID == i),]$OverallGrade
    
    # Average grade
    average_ApplicantTrack <- round(mean(reviewer_vote_per_projectID_ApplicantTrack, na.rm = TRUE),0)
    average_ScientificRelevance <-round( mean(reviewer_vote_per_projectID_ScientificRelevance, na.rm = TRUE),0)
    average_Suitability <- round(mean(reviewer_vote_per_projectID_Suitability, na.rm = TRUE),0)
    average_OverallGrade <- round(mean(reviewer_vote_per_projectID_OverallGrade, na.rm = TRUE),0)
    
    # Put average grade into the matrix
    average_review_matrix[j,2] <- average_ApplicantTrack
    average_review_matrix[j,3] <- average_ScientificRelevance
    average_review_matrix[j,4] <- average_Suitability
    average_review_matrix[j,5] <- average_OverallGrade
    
    # Increment j
    j <- j+1
  }
  
  colnames(average_review_matrix)<- c("ProjectID", "ApplicantTrack", "ScientificRelevance","Suitability", "OverallGrade")
  return(as.data.frame(average_review_matrix))
}
