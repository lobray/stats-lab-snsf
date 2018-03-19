library(ISOcodes)
library(stringr)
library(tidyr)

## Function to clean the applications dataset, correcting types and selecting columns

cleaning_apps <- function(applications) {
  apps <- applications  # Rename data
  # I decided to remove MainDiscipline and keep MainDisciplineLevel2 because
  # the first has 140 levels, the latter only 21 (maybe could be useful)
  # I also removed CallTitle because we are more interested in the date I think
  drops <- c("MainDiscipline","CallTitle")
  apps <- apps[ , !(names(apps) %in% drops)]
  
  # I added a new column with the year to make it easier to select obs from 2016
  apps$CallEndDate <- as.Date(apps$CallEndDate, format="%Y-%m-%d") # extract year
  apps$Year <- format(as.Date(apps$CallEndDate, format="%Y-%m-%d"),"%Y") 
  
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
  id<- which(aa_reviews$QuestionRating=="-outstanding") # Correct some misspelled grades
  aa_reviews$QuestionRating[id]<-"outstanding"
  colnames(aa_reviews)[5] <- "ReviewerGender"  # Change name of the gender column
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
  # sum(is.na(aa_reviews$Country)) # Now we have 949 NAs for Country instead of 6111
  return(aa_reviews)
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

## Code to run

c.apps <- cleaning_apps(applications)
c.apps2016 <- select.2016(c.apps)
c.reviews <- cleaning_reviews(reviews)
cc.reviews <- f.spread(c.reviews)
cc.referee <- f.spread(referee_grades)
cc.reviews_2 <- selection.f(c.apps2016,cc.reviews)
cc.referee_2 <- selection.f(c.apps2016,cc.referee)
cc.referee_2 <- selection.f(cc.reviews_2,cc.referee_2)
c.apps2016_2 <- selection.f(cc.referee_2,c.apps2016)

## Quick check that now we have the same number of ProjectID in every dataset
cbind(length(unique(cc.referee_2$ProjectID)), length(unique(cc.reviews_2$ProjectID)), 
              length(unique(c.apps2016_2$ProjectID)))


## Function to check the proportion of NAs per variable and delete columns with more than 50% NAs
count.na <- function(data) {
  count <- vector()
  r <- rep(F,ncol(data))
  for (i in 1:ncol(data)){
    count[i] <- sum(is.na(data[,i]))/length(data[,i])
    if (count[i]>0.5) {r[i] <- T}
  }
  data <- data[,c(which(r==F))]
  return(data)
}

c.apps2016_2 <- count.na(c.apps2016_2)   # AcademicAge and ProfessorshipType are removed
cc.reviews_2 <- count.na(cc.reviews_2)   # BroaderImpact is removed


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

g.review <- gender.f(c.apps2016_2,cc.reviews_2)
g.referee <- gender.f(c.apps2016_2, cc.referee_2)

## Functions to add a variable to see if gender of applicant and reviewer / referee are the same
same.gender <- function(data){
  s <- vector()
  for (i in 1:nrow(data)) {
    if (data$ApplicantGender[i] == data$ReviewerGender[i]) {s[i] <- 1}
    else {s[i] <- 0}
  }
  data$SameGender <- factor(s)
  # If SameGender=0: the gender is different
  # If SameGender=1: the gender is the same
  return(data)
}

same.gender2 <- function(data){
  s <- vector()
  for (i in 1:nrow(data)) {
    if (data$ApplicantGender[i] == data$RefereeGender[i]) {s[i] <- 1}
    else {s[i] <- 0}
  }
  data$SameGender <- factor(s)
  # If SameGender=0: the gender is different
  # If SameGender=1: the gender is the same
  return(data)
}

gg.review <- same.gender(g.review)
gg.referee <- same.gender2(g.referee)

## Function to transform the variable type

var.app <- function(data){
  id.B <- which(data$GradeFinal=='B-' | data$GradeFinal=='B+')
  data$GradeFinal[id.B] <- 'B'
  id.BC <- which(data$GradeFinal=='BC-' | data$GradeFinal=='BC+' | data$GradeFinal=='BC ')
  data$GradeFinal[id.BC] <- 'BC'
  data$GradeFinal <- factor(data$GradeFinal, levels=c('D','C','BC','B','AB','A'), ordered = T)
  data$IsApproved <- factor(data$IsApproved)
  data$Division <- factor(data$Division)
  data$MainDisciplineLevel2 <- factor(data$MainDisciplineLevel2)
  data$Gender <- factor(data$Gender)
  data$IsHasPreviousProjectRequested <- factor(data$IsHasPreviousProjectRequested)
  data$InstType <- factor(data$InstType)
  data$IsContinuation <- factor(data$IsContinuation)
  return(data)
}

d.apps2016 <- var.app(c.apps2016_2)

var.rev <- function(data){
  data$OverallGrade <- factor(data$OverallGrade, levels=c("not considered","poor", "average", 
                            "good", "very good","excellent","outstanding"), ordered=TRUE)
  data$SourcePerson <- factor(data$SourcePerson)
  data$ReviewerGender <- factor(data$ReviewerGender)
  colnames(data)[9:11] <- c("TrackRecord","Relevance","Feasibility")
  data$TrackRecord <- factor(data$TrackRecord, levels=c("0","poor","average","good","very good",
                            "excellent","outstanding"),ordered = T)
  data$Relevance <- factor(data$Relevance, levels=c("0","poor","average","good","very good",
                            "excellent","outstanding"),ordered=T)
  data$Feasibility <- factor(data$Feasibility, levels=c("0","poor","average","good","very good",
                            "excellent","outstanding"),ordered=T)
  return(data)
}

d.review <- var.rev(gg.review)

var.ref <- function(data){
  data$OverallComparativeRanking <- factor(data$OverallComparativeRanking, 
              levels=c("D","C", "BC", "B", "AB","A"), ordered=TRUE)
  data$RefereeRole <- factor(data$RefereeRole)
  data$RefereeGender <- factor(data$RefereeGender)
  colnames(data)[7:8] <- c("TrackRecord","Project")
  data$TrackRecord <- factor(data$TrackRecord, levels=c("0","poor","average","good","very good",
                                                        "excellent","outstanding"),ordered = T)
  data$Project <- factor(data$Project, levels=c("0","poor","average","good","very good",
                                                    "excellent","outstanding"),ordered=T)
  return(data)
}

d.referee <- var.ref(gg.referee)

########################################################################################

## GRAPHICAL ANALYSIS WITH MOSAICPLOT

library(vcd)

## APPLICATIONS:
cotabplot(~ Gender+IsApproved, data=d.apps2016, shade=T)   
cotabplot(~ Gender + Division, data= d.apps2016, shade=T)  
# Different ratio M/F per division: less women in Div2, more women in Div1
cotabplot(~ Division + IsApproved, data=d.apps2016, shade=T) 
# Different acceptance rate per division: less approved in Div1, more in Div2
cotabplot(~ IsContinuation + IsApproved, data=d.apps2016, shade=T)  
# If it's continuation is more likely to be approved
cotabplot(~ Gender+IsApproved | Division, data=d.apps2016, shade=T) 
# No difference M/F acceptance rate in each division

# cotabplot(~ MainDisciplineLevel2+IsApproved, data=d.apps2016, shade=T) 
# Too many levels to visualize it
cotabplot(~ IsHasPreviousProjectRequested+IsApproved, data=c.apps2016_2, shade=T) 
# Having Previous Project is not significant
cotabplot(~ IsApproved+InstType, data=d.apps2016, shade=T) 
# Applicants from ETH are more likely to be approved


## REVIEWERS
cotabplot(~OverallGrade+ApplicantGender, data=d.review, shade=T)
# Woman applicant less likely to be rated as outstanding
cotabplot(~OverallGrade+ApplicantGender|ReviewerGender, data=d.review, shade=T) 
# P-value not significant for woman reviewer but it is for male reviewer
cotabplot(~OverallGrade+SameGender, data=d.review, shade=T)
# If applicant and referee have same gender, more likely to get Outstanding
cotabplot(~OverallGrade+SameGender|ReviewerGender, data=d.review, shade=T)
# If applicant and referee don't have same gender, less likely for women to get Outstanding 

cotabplot(~ApplicantGender+TrackRecord, data=d.review, shade=T) 
# Considering TrackRecord, less likely for women to get outstanding
cotabplot(~ApplicantGender+Relevance,data=d.review, shade=T) 
cotabplot(~ApplicantGender+Feasibility,data=d.review, shade=T) 
# These two test are less significant than the previous


cotabplot(~ApplicantGender+TrackRecord|ReviewerGender, data=d.review, shade=T) 
# If reviewer is woman no difference, if it's man significant difference judging TrackRecord
cotabplot(~ApplicantGender+Relevance|ReviewerGender,data=d.review, shade=T) 
# P-value still significant but much higher
cotabplot(~ApplicantGender+Feasibility|ReviewerGender,data=d.review, shade=T)
# P-value significant for men

cotabplot(~TrackRecord+SameGender, data=d.review, shade=T)
cotabplot(~Relevance+SameGender,data=d.review, shade=T)
cotabplot(~Feasibility+SameGender,data=d.review, shade=T)


## REFEREE

cotabplot(~OverallComparativeRanking+ApplicantGender, data=d.referee, shade=T) 
# P-value significant but not big residuals Significant
cotabplot(~OverallComparativeRanking+ApplicantGender|RefereeGender, data=d.referee, shade=T)
cotabplot(~TrackRecord+ApplicantGender, data=d.referee, shade=T) 
cotabplot(~Project+ApplicantGender, data=d.referee, shade=T)
cotabplot(~TrackRecord+ApplicantGender|RefereeGender, data=d.referee, shade=T) 
cotabplot(~Project+ApplicantGender|RefereeGender, data=d.referee, shade=T)

cotabplot(~TrackRecord+SameGender, data=d.referee, shade=T) # Not significant
cotabplot(~Project+SameGender,data=d.referee, shade=T) # Not significant
