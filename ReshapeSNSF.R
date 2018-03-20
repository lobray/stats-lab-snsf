setwd("/home/leslie/Desktop/stats-lab-snsf")

merge_snsf_data_2016 <- function(applications, reviews, referee_grades) {
  
  ## Rename data
  apps <- applications # make copy of applications
  aa_reviews <- reviews # make copy
  bb_referee_grades <- referee_grades # make copy
  
  ## First, clean up reviews data, and prepare it to be merged wtih referee data
  
  #Here I change some variable Types
  #Gender
  aa_reviews$Gender<-as.factor(aa_reviews$Gender) 
  #OverallGrade -- The external reviewer's overall assessment of the application
  aa_reviews$OverallGrade<-as.factor(aa_reviews$OverallGrade)
  # QuestionRating: Correction of some ovious mistake and factoring
  id<- which(aa_reviews$QuestionRating=="-outstanding")
  aa_reviews$QuestionRating[id]<-"outstanding"
  aa_reviews$QuestionRating<-as.factor(aa_reviews$QuestionRating)
  #Source Person -- Who suggested the reviewer?
  aa_reviews$SourcePerson<-as.factor(aa_reviews$SourcePerson)
  
  ## Clean up reviews data, and prepare it to be merged wtih referee data
  aa_proj_id <- which(duplicated(aa_reviews$ProjectID)) # identify unique project id
  unique_aa_reviews <- aa_reviews[-aa_proj_id,] # make new data frame with just unique project id
  unique_aa_reviews$Question <- rep("OverallGrade", dim(unique_aa_reviews)[1]) # input the overall grade as a normal question
  unique_aa_reviews$QuestionRating <- unique_aa_reviews$OverallGrade # input rating from overall grade into the questionrating column
  unique_aa_reviews <- unique_aa_reviews[,-c(3)] # remove "overallgrade" column
  aa_reviews <- aa_reviews[,-3] # remove "overallgrade" column from original data
  aa_reviews <- rbind(aa_reviews, unique_aa_reviews) # rejoin data
  colnames(aa_reviews)[4] <- "ReviewerGender" # specify gender as reviewer gender
  colnames(aa_reviews)[5] <- "ReviewerCountry" # specify country as reviewer country
  aa_reviews$ReviewerType <- rep("External", dim(aa_reviews)[1]) # add in reviewer type
  aa_reviews$RefereeRole <- NA
  # aa_reviews$RefereeRole <- "External Reviewer" # add in columns from referee - LESLIE commented thi out because I think this variable refers to something different
  aa_reviews <- aa_reviews[,c("ProjectID", "Question", "QuestionRating", "ReviewerGender", "ReviewerID", "ReviewerType", "ReviewID", "RefereeRole", "SourcePerson",
                              "ReviewerCountry", "EmailEnding")] #a align order with referees
  
  # A few lines to try to get Reviewers Country from EmailEnding
  library(ISOcodes)
  library(stringr)
  IsoCountry<-ISO_3166_1[,c("Alpha_2","Name")]  #Uk Is not in this data Set
  IsoCountry<-rbind(IsoCountry, c(Alpha_2="UK", Name="Great Britain and Northern Ireland"))
  
  #using Emailending to set Country
  aa_reviews$EmailEnding<-toupper(aa_reviews$EmailEnding)  
  for (i in IsoCountry$Alpha_2){
    id<-which(str_detect(aa_reviews$EmailEnding,i)&is.na(aa_reviews$ReviewerCountry))
    id_iso<-which(str_detect(IsoCountry$Alpha_2,i)==1)
    name<-IsoCountry[id_iso,2]
    aa_reviews$ReviewerCountry[id]<-name
  }   #There are some endings (.com, .edu, .. That I left with NA's)
  
  ## Next, clean up referee data, and prepare it to be merged with reviews data
  bb_proj_id <- which(duplicated(bb_referee_grades$ProjectID)) # identify unique project id
  unique_bb_referee_grades <- bb_referee_grades[-bb_proj_id,] # make new data frame with unique project id
  unique_bb_referee_grades$Question <- rep("OverallComparativeRanking", dim(unique_bb_referee_grades)[1]) # input overall grade as a normal question
  unique_bb_referee_grades$QuestionRating <- unique_bb_referee_grades$OverallComparativeRanking # input rating from overall comparative grade into the questionrating column
  bb_referee_grades <- rbind(bb_referee_grades, unique_bb_referee_grades) # rejoin data
  bb_referee_grades <- bb_referee_grades[,-3] # remove overall comparative ranking column
  colnames(bb_referee_grades)[4] <- "ReviewerGender" # update gender to specify reviewer gender
  bb_referee_grades$ReviewerType <- rep("Internal", dim(bb_referee_grades)[1]) # add in reviewer type
  colnames(bb_referee_grades)[6] <- "ReviewerID" # rename "RefereeID" to "ReviewerID" to align with external reviewer nomenclature
  colnames(bb_referee_grades)[7] <- "ReviewID" # rename "RefereeRecommendationID" to "ReviewerID" to align with external reviewer naming
  bb_referee_grades$SourcePerson <- "Not Applicable" # add in columns from reviews
  bb_referee_grades$ReviewerCountry <- "Not Applicable" # add in columns from reviews
  bb_referee_grades$EmailEnding <- "Not Applicable" # add in columns from reviews
  bb_referee_grades <- bb_referee_grades[,c("ProjectID", "Question", "QuestionRating", "ReviewerGender", "ReviewerID", "ReviewerType", "ReviewID", "RefereeRole", "SourcePerson",
                                            "ReviewerCountry", "EmailEnding")] # align column ordering with reviews
  
  ## Now merge reviews and referee data
  full_review_profile <- rbind(aa_reviews, bb_referee_grades) # combine two review sources
  
  
  ## Some variable change in the applications
  #Set `NA` to zero in granted when approved = 0
  id<-(is.na(apps$AmountGranted)&apps$IsApproved==0)
  apps$AmountGranted[id]<-0
  #For Approvals
  apps$IsApproved<-as.factor(apps$IsApproved)
  #For Gender
  apps$Gender<-as.factor(apps$Gender)
  #Date
  library(lubridate)
  apps$CallEndDate<-ymd(apps$CallEndDate)
  colnames(apps)[colnames(apps)=="CallEndDate"]<- "Year"
  #For Division
  apps$Division<-as.factor(apps$Division)
  #Applicant Academic age at submission. Starting on 2016-10-01 to be accounted for
  colnames(apps)[colnames(apps)=="ResponsibleApplicantAcademicAgeAtSubmission"]<-"AcademicAge"
  #Applicant age at submission
  colnames(apps)[colnames(apps)=="ResponsibleApplicantAgeAtSubmission"]<-"Age"
  #Professorship
  colnames(apps)[colnames(apps)=="ResponsibleApplicantProfessorshipType"]<- "Professorship"
  apps$Professorship<-as.factor(apps$Professorship)
  #Continuation
  apps$IsContinuation<-as.factor(apps$IsContinuation)
  #Previous project Request
  colnames(apps)[colnames(apps)=="IsHasPreviousProjectRequested"]<-"PreviousRequest"
  apps$PreviousRequest<-as.factor(apps$PreviousRequest)
  #Institution  
  apps$InstType<-as.factor(apps$InstType)
  
  ## Merge with application information, and restrict to just 2016 data
  testing_merge <- merge(full_review_profile, apps, by="ProjectID") # merges everything into one!
  merged_2016<- subset(testing_merge,year(testing_merge$Year)==2016)
  
 
 return(merged_2016)
}

combined <- merge_snsf_data_2016(applications = applications, reviews = reviews, referee_grades = referee_grades)
