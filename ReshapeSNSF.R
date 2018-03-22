setwd("/home/leslie/Desktop/stats-lab-snsf")

merge_snsf_data_2016 <- function(applications, reviews, referee_grades) {
  
  library(ISOcodes)
  library(stringr)
  library(lubridate)
  
  ### Make a copy of data & rename it
  
  apps <- applications # make copy of applications
  external_reviews <- reviews # make copy
  internal_reviews <- referee_grades # make copy
  
  
  ### Convert categorical variables to factors
  
  external_reviews$Gender<-as.factor(external_reviews$Gender) # Gender
  external_reviews$OverallGrade<-as.factor(external_reviews$OverallGrade) #OverallGrade
  external_reviews$QuestionRating<-as.factor(external_reviews$QuestionRating) #QuestionRating
  external_reviews$SourcePerson<-as.factor(external_reviews$SourcePerson) #Source Person -- Who suggested the reviewer?
  
  
  ### Fix obvious data errors: QuestionRating
  
  id <- which(external_reviews$QuestionRating=="-outstanding")
  external_reviews$QuestionRating[id]<-"outstanding"
  
  
  ### Clean up external data, and prepare it to be merged internal data
  
  aa_proj_id <- which(duplicated(external_reviews$ProjectID)) # identify duplicate project id
  unique_external_reviews <- external_reviews[-aa_proj_id,] # make new data frame removing project id
  unique_external_reviews$Question <- rep("OverallGrade", dim(unique_external_reviews)[1]) # input OverallGrade as a Question
  unique_external_reviews$QuestionRating <- unique_external_reviews$OverallGrade # input OverallGrade rating into QuestionRating
  unique_external_reviews <- unique_external_reviews[,-c(3)] # remove "OverallGrade" column
  external_reviews <- external_reviews[,-3] # remove "OverallGrade" column from original data
  external_reviews <- rbind(external_reviews, unique_external_reviews) # rejoin data
  colnames(external_reviews)[4] <- "ReviewerGender" # Rename gender as ReviewerGender
  colnames(external_reviews)[5] <- "ReviewerCountry" # specify country as ReviewerCountry
  external_reviews$ReviewerType <- rep("External", dim(external_reviews)[1]) # add in reviewer type
  external_reviews$RefereeRole <- NA
  # external_reviews$RefereeRole <- "External Reviewer" # add in columns from referee - LESLIE commented thi out because I think this variable refers to something different
  external_reviews <- external_reviews[,c("ProjectID", "Question", "QuestionRating", "ReviewerGender", "ReviewerID", "ReviewerType", "ReviewID", "RefereeRole", "SourcePerson",
                              "ReviewerCountry", "EmailEnding")] # align order with referees
  

  ### A few lines to try to get Reviewers Country from EmailEnding
  
  IsoCountry <- ISO_3166_1[,c("Alpha_2","Name")]  #UK Is not in this data Set
  IsoCountry <- rbind(IsoCountry, c(Alpha_2="UK", Name="Great Britain and Northern Ireland"))
  
  
  ### Using Emailending to set Country
  
  external_reviews$EmailEnding<-toupper(external_reviews$EmailEnding)  
  for (i in IsoCountry$Alpha_2){ #There are some endings (.com, .edu, .. That I left with NA's)
    id <- which(str_detect(external_reviews$EmailEnding,i)&is.na(external_reviews$ReviewerCountry))
    id_iso <- which(str_detect(IsoCountry$Alpha_2,i)==1)
    name <- IsoCountry[id_iso,2]
    external_reviews$ReviewerCountry[id] <- name
  }   
  
  
  ### Clean internal reviewer data, and prepare it to be merged with external reviews data
  
  bb_proj_id <- which(duplicated(internal_reviews$ProjectID)) # identify duplicate project ids
  unique_internal_reviews <- internal_reviews[-bb_proj_id,] # make new data frame with unique project id
  unique_internal_reviews$Question <- rep("OverallComparativeRanking", dim(unique_internal_reviews)[1]) # input OverallRanking into Question
  unique_internal_reviews$QuestionRating <- unique_internal_reviews$OverallComparativeRanking # input overall comparative grade into questionrating column
  internal_reviews <- rbind(internal_reviews, unique_internal_reviews) # rejoin data
  internal_reviews <- internal_reviews[,-3] # remove overall comparative ranking column
  colnames(internal_reviews)[4] <- "ReviewerGender" # update gender to specify reviewer gender
  internal_reviews$ReviewerType <- rep("Internal", dim(internal_reviews)[1]) # add in reviewer type
  colnames(internal_reviews)[6] <- "ReviewerID" # rename "RefereeID" to "ReviewerID" to align with external reviewer data
  colnames(internal_reviews)[7] <- "ReviewID" # rename "RefereeRecommendationID" to "ReviewerID" to align with external reviewer naming
  internal_reviews$SourcePerson <- "Not Applicable" # add in columns from reviews
  internal_reviews$ReviewerCountry <- "Not Applicable" # add in columns from reviews
  internal_reviews$EmailEnding <- "Not Applicable" # add in columns from reviews
  internal_reviews <- internal_reviews[,c("ProjectID", "Question", "QuestionRating", "ReviewerGender", "ReviewerID", "ReviewerType", "ReviewID", "RefereeRole", "SourcePerson",
                                            "ReviewerCountry", "EmailEnding")] # align column ordering with reviews
  
  
  ## Merge internal and external reviews
  
  full_review_profile <- rbind(external_reviews, internal_reviews) # combine two review sources
  print(length(unique(full_review_profile$ProjectID)))
  
  
  ### Simplify a few variable names in apps data set
  
  colnames(apps)[colnames(apps)=="ResponsibleApplicantAcademicAgeAtSubmission"]<-"AcademicAge" # Applicant Academic age at submission. Starting on 2016-10-01
  colnames(apps)[colnames(apps)=="ResponsibleApplicantAgeAtSubmission"]<-"Age"   # Applicant age at submission
  colnames(apps)[colnames(apps)=="IsHasPreviousProjectRequested"] <- "PreviousRequest"   # Rename previous project Request
  colnames(apps)[colnames(apps)=="ResponsibleApplicantProfessorshipType"]<- "Professorship" # Simplify name
  
  
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
  
  
  ### Merge with application information, and restrict to just 2016 data
  testing_merge <- merge(full_review_profile, apps, by="ProjectID") # merges everything into one!
  merged_2016<- subset(testing_merge,year(testing_merge$Year)==2016)
  

 return(merged_2016)
  
}

combined <- merge_snsf_data_2016(applications = applications, reviews = reviews, referee_grades = referee_grades)
length(unique(combined$ProjectID))

      