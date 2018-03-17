library(ISOcodes)
library(stringr)
library(tidyr)

## Function to clean the applications dataset, correcting types and selecting columns

cleaning_apps <- function(applications) {
  apps <- applications  # Rename data
  ## Check all variable types
  apps$IsApproved <- factor(apps$IsApproved)
  apps$GradeFinal <- factor(apps$GradeFinal)
  apps$Division <- factor(apps$Division)
  apps$MainDisciplineLevel2 <- factor(apps$MainDisciplineLevel2)
  # I decided to remove MainDiscipline and keep MainDisciplineLevel2 because
  # the first has 140 levels, the latter only 21 (maybe could be useful)
  # I also removed CallTitle because we are more interested in the date I think
  drops <- c("MainDiscipline","CallTitle")
  apps <- apps[ , !(names(apps) %in% drops)]
  
  # I added a new column with the year to make it easier to select obs from 2016
  apps$CallEndDate <- as.Date(apps$CallEndDate, format="%Y-%m-%d") # extract year
  apps$Year <- format(as.Date(apps$CallEndDate, format="%Y-%m-%d"),"%Y") 
  
  apps$ResponsibleApplicantProfessorshipType <- factor(apps$ResponsibleApplicantProfessorshipType)
  apps$Gender <- factor(apps$Gender)
  apps$IsHasPreviousProjectRequested <- factor(apps$IsHasPreviousProjectRequested)
  apps$InstType <- factor(apps$InstType)
  apps$IsContinuation <- factor(apps$IsContinuation)
  
  # Removed NAs for gender since we are interested in it
  id.g <- which(is.na(apps$Gender)) 
  apps <- apps[-id.g,]
  
  return(apps)
}

## Function to select only observations from 2016 

select.2016 <- function(data) {
  id.2016 <- which(data$Year==2016)
  apps.2016 <- data[id.2016,]
  return(apps.2016)
}


## Function to clean reviews dataset

cleaning_reviews <- function(reviews) {
  aa_reviews <- reviews
  # Correct some misspelled grades
  id<- which(aa_reviews$QuestionRating=="-outstanding") 
  aa_reviews$QuestionRating[id]<-"outstanding"
  ## Check all variable types
  aa_reviews$QuestionRating <- factor(aa_reviews$QuestionRating)
  aa_reviews$OverallGrade <- factor(aa_reviews$OverallGrade)
  aa_reviews$SourcePerson <- factor(aa_reviews$SourcePerson)
  aa_reviews$Gender <- factor(aa_reviews$Gender)
  colnames(aa_reviews)[5] <- "ReviewerGender"
  ## We combine info from COUNTRY and EMAIL to avoid NA
  IsoCountry<-ISO_3166_1[,c("Alpha_2","Name")]  #Add UK which is not in dataset
  IsoCountry<-rbind(IsoCountry, c(Alpha_2="UK", Name="Great Britain and Northern Ireland"))
  aa_reviews$EmailEnding<-toupper(aa_reviews$EmailEnding)
  for (i in 1:nrow(aa_reviews)) {
    countries <- IsoCountry$Alpha_2
    if (is.na(aa_reviews$Country[i]) & (aa_reviews$EmailEnding[i] %in% countries)) {
      id.c <- which(IsoCountry$Alpha_2==aa_reviews$EmailEnding[i])
      aa_reviews$Country[i] <- IsoCountry[id.c,2]
    }}
  sum(is.na(aa_reviews$Country)) # Now we have 949 NAs for Country instead of 6111
  # Down here I checked for how many reviewers we don't know the country -> 267 (before 1658))
  # test <- aa_reviews[c(is.na(aa_reviews$Country)),]
  # length(unique(test$ReviewerID))
  return(aa_reviews)
}

## Function to clean referees dataset

cleaning_referees <- function(referee_grades) {
  bb_referee_grades <- referee_grades  # Rename data
  bb_referee_grades$QuestionRating <- factor(bb_referee_grades$QuestionRating)
  bb_referee_grades$OverallComparativeRanking <- factor(bb_referee_grades$OverallComparativeRanking)
  bb_referee_grades$RefereeRole <- factor(bb_referee_grades$RefereeRole)
  bb_referee_grades$RefereeGender <- factor(bb_referee_grades$RefereeGender)
  return(bb_referee_grades)
}

## Function to put QuestionRating as different columns for reviews and referees

f.spread <- function(data) {
  data <- spread(data, Question, QuestionRating)
  return(data)
}

## Function to select only observations that are in all 3 datasets
selection.f <- function(data1,data2) {
  rows.1 <- match(data2$ProjectID, data1$ProjectID) # get apps rows corresponding to reviews
  rows.2 <- which(is.na(rows.1)==FALSE)  # get rows number of reviews that are in apps2016
  data2 <- data2[rows.2,] # select rows 
  return(data2)
}

## Function to check the proportion of NAs per variable
count.na <- function(v) {
  count <- sum(is.na(v))/length(v)
  return(count)
}

## Code to run

c.apps <- cleaning_apps(applications)
c.apps2016 <- select.2016(c.apps)
c.reviews <- cleaning_reviews(reviews)
c.referee <- cleaning_referees(referee_grades)
cc.reviews <- f.spread(c.reviews)
cc.referee <- f.spread(c.referee)
cc.reviews_2 <- selection.f(c.apps2016,cc.reviews)
cc.referee_2 <- selection.f(c.apps2016,cc.referee)
cc.referee_2 <- selection.f(cc.reviews_2,cc.referee_2)
c.apps2016_2 <- selection.f(cc.referee_2,c.apps2016)

## Quick check that now we have the same number of ProjectID in every dataset
cbind(length(unique(cc.referee_2$ProjectID)), length(unique(cc.reviews_2$ProjectID)), 
              length(unique(c.apps2016_2$ProjectID)))

## Check proportion of NAs we have
count.na(c.apps2016_2$ResponsibleApplicantAcademicAgeAtSubmission) # 58%
count.na(c.apps2016_2$ResponsibleApplicantProfessorshipType) # 55%
count.na(cc.reviews_2$`Broader impact (forms part of the assessment of scientific relevance, originality and topicality)`) # 100%
# count.na(cc.reviews_2$Country) # 3%

## Remove variables with more than 50% of NAs
drops <- c("ResponsibleApplicantAcademicAgeAtSubmission","ResponsibleApplicantProfessorshipType")
c.apps2016_2 <- c.apps2016_2[ , !(names(c.apps2016_2) %in% drops)]
drops2 <- c("Broader impact (forms part of the assessment of scientific relevance, originality and topicality)")
cc.reviews_2 <- cc.reviews_2[ , !(names(cc.reviews_2) %in% drops2)]

## Function to put the applicant gender in the other two datasets
gender.f <- function(application,data){
  rows <- match(data$ProjectID, application$ProjectID) # rows on applications dataset
  gender <- vector()
  for (i in 1:nrow(data)) {
    gender[i] <- application$Gender[rows[i]]
  }
  data$ApplicantGender <- gender
  data$ApplicantGender <- factor(data$ApplicantGender)
  levels(data$ApplicantGender) <- c("f","m")
  return(data)
}

## Run the previous functions
g.review <- gender.f(c.apps2016_2,cc.reviews_2)
g.referee <- gender.f(c.apps2016_2, cc.referee_2)

## Function to add a variable to see if gender of applicant and reviewer are the same
same.gender <- function(data){
  s <- vector()
  for (i in 1:nrow(data)) {
    if (data$ApplicantGender[i] == data$ReviewerGender[i]) {s[i] <- 1}
    else {s[i] <- 0}
  }
  data$SameGender <- s
  # If SameGender=0: the gender is different
  # If SameGender=1: the gender is the same
  return(data)
}

## Function to add a variable to see if gender of applicant and referee are the same
same.gender2 <- function(data){
  s <- vector()
  for (i in 1:nrow(data)) {
    if (data$ApplicantGender[i] == data$RefereeGender[i]) {s[i] <- 1}
    else {s[i] <- 0}
  }
  data$SameGender <- s
  # If SameGender=0: the gender is different
  # If SameGender=1: the gender is the same
  return(data)
}

## Run the previous two functions
gg.review <- same.gender(g.review)
gg.referee <- same.gender2(g.referee)
