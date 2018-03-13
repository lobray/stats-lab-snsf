####################
## MERGE datasets ##
####################

merge_snsf_2016 <- function(applications, reviews, referee_grades) {
  
  ## Rename data
  apps <- applications # make copy 
  aa_reviews <- reviews 
  bb_referee_grades <- referee_grades
  
  ## Clean up reviews data and prepare it to be merged wtih referee data
  aa_proj_id <- unique(aa_reviews$ProjectID) # identify unique project id
  unique_aa_reviews <- aa_reviews[aa_proj_id,] # make new data frame with just unique project id
  unique_aa_reviews$Question <- rep("OverallGrade", dim(unique_aa_reviews)[1]) # input the overall grade as a normal question
  unique_aa_reviews$QuestionRating <- unique_aa_reviews$OverallGrade # input rating from overall grade into the questionrating column
  unique_aa_reviews <- unique_aa_reviews[,-c(3)] # remove "overallgrade" column
  aa_reviews <- aa_reviews[,-3] # remove "overallgrade" column from original data
  aa_reviews <- rbind(aa_reviews, unique_aa_reviews) # rejoin data
  colnames(aa_reviews)[4] <- "ReviewerGender" # specify gender as reviewer gender
  colnames(aa_reviews)[5] <- "ReviewerCountry" # specify country as reviewer country
  aa_reviews$ReviewerType <- rep("External", dim(aa_reviews)[1]) # add in reviewer type
  aa_reviews$RefereeRole <- NA # add in columns from referee
  aa_reviews <- aa_reviews[,c("ProjectID", "Question", "QuestionRating", "ReviewerGender", "ReviewerID", "ReviewerType", "ReviewID", "RefereeRole", "SourcePerson",
                              "ReviewerCountry", "EmailEnding")] #a align order with referees
  
  ## Next, clean up referee data and prepare it to be merged with reviews data
  bb_proj_id <- unique(bb_referee_grades$ProjectID) # identify unique project id
  unique_bb_referee_grades <- bb_referee_grades[bb_proj_id,] # make new data frame with unique project id
  unique_bb_referee_grades$Question <- rep("OverallComparativeRanking", dim(unique_bb_referee_grades)[1]) # input overall grade as a normal question
  unique_bb_referee_grades$QuestionRating <- unique_bb_referee_grades$OverallComparativeRanking # input rating from overall comparative grade into the questionrating column
  bb_referee_grades <- rbind(bb_referee_grades, unique_bb_referee_grades) # rejoin data
  bb_referee_grades <- bb_referee_grades[,-3] # remove overall comparative ranking column
  colnames(bb_referee_grades)[4] <- "ReviewerGender" # update gender to specify reviewer gender
  bb_referee_grades$ReviewerType <- rep("Internal", dim(bb_referee_grades)[1]) # add in reviewer type
  colnames(bb_referee_grades)[6] <- "ReviewerID" # rename "RefereeID" to "ReviewerID" to align with external reviewer nomenclature
  colnames(bb_referee_grades)[7] <- "ReviewID" # rename "RefereeRecommendationID" to "ReviewerID" to align with external reviewer naming
  bb_referee_grades$SourcePerson <- NA # add in columns from reviews
  bb_referee_grades$ReviewerCountry <- NA # add in columns from reviews
  bb_referee_grades$EmailEnding <- NA # add in columns from reviews
  bb_referee_grades <- bb_referee_grades[,c("ProjectID", "Question", "QuestionRating", "ReviewerGender", "ReviewerID", "ReviewerType", "ReviewID", "RefereeRole", "SourcePerson",
                                            "ReviewerCountry", "EmailEnding")] # align column ordering with reviews
  
  ## Now merge reviews and referee data
  full_review_profile <- rbind(aa_reviews, bb_referee_grades) # combine two review sources
  ## Merge with application information, and restrict to just 2016 data
  testing_merge <- merge(full_review_profile, apps, by="ProjectID")
  
  ## Select data from 2016
  c2016_1 <- which(testing_merge$CallEndDate=="2016-10-01")
  c2016_2 <- which(testing_merge$CallEndDate=="2016-04-01")
  apps_2016 <- testing_merge[c(c2016_1,c2016_2),]
  
  return(apps_2016)
}

#################################
## Defining cleaning functions ##
#################################

library(ISOcodes)
library(stringr)

cleaning_apps <- function(applications) {
  apps <- applications  # Rename data
  ## Check all variable types
  apps$IsApproved <- factor(apps$IsApproved)
  apps$GradeFinal <- factor(apps$GradeFinal)
  apps$Division <- factor(apps$Division)
  # I don't change MainDiscipline and MainDisciplineLevel because probably we will not use them
  # Same for CallTitle and CallEndDate
  apps$ResponsibleApplicantProfessorshipType <- factor(apps$ResponsibleApplicantProfessorshipType)
  apps$Gender <- factor(apps$Gender)
  apps$IsHasPreviousProjectRequested <- factor(apps$IsHasPreviousProjectRequested)
  apps$InstType <- factor(apps$InstType)
  apps$IsContinuation <- factor(apps$IsContinuation)
  return(apps)
}

cleaning_reviews <- function(reviews) {
  aa_reviews <- reviews  # Rename data
  # Correct some misspelled grades
  id<- which(aa_reviews$QuestionRating=="-outstanding") 
  aa_reviews$QuestionRating[id]<-"outstanding"
  ## Check all variable types
  aa_reviews$QuestionRating <- factor(aa_reviews$QuestionRating)
  aa_reviews$OverallGrade <- factor(aa_reviews$OverallGrade)
  aa_reviews$SourcePerson <- factor(aa_reviews$SourcePerson)
  aa_reviews$Gender <- factor(aa_reviews$Gender)
  ## Here we combine info from COUNTRY and EMAIL to avoid NA
  ## to get Country from email ending
  IsoCountry<-ISO_3166_1[,c("Alpha_2","Name")]  #Uk Is not in this data Set
  IsoCountry<-rbind(IsoCountry, c(Alpha_2="UK", Name="Great Britain and Northern Ireland"))
  aa_reviews$EmailEnding<-toupper(aa_reviews$EmailEnding)
    for (i in 1:nrow(aa_reviews)) {
    countries <- IsoCountry$Alpha_2
    if (is.na(aa_reviews$Country[i]) & (aa_reviews$EmailEnding[i] %in% countries)) {
      id.c <- which(IsoCountry$Alpha_2==aa_reviews$EmailEnding[i])
      aa_reviews$Country[i] <- IsoCountry[id.c,2]
    }}
  ## Now we have 949 NAs for Country instead of 6111
  ## (I checked for how many reviewers we don't know the country -> 267 (before 1658))
  test <- aa_reviews[c(is.na(aa_reviews$Country)),]
  n <- unique(test$ReviewerID)
  return(aa_reviews)
}
  
cleaning_referees <- function(referee_grades) {
    bb_referee_grades <- referee_grades  # Rename data
    # Check variable types
  bb_referee_grades$QuestionRating <- factor(bb_referee_grades$QuestionRating)
  bb_referee_grades$OverallComparativeRanking <- factor(bb_referee_grades$OverallComparativeRanking)
  bb_referee_grades$RefereeRole <- factor(bb_referee_grades$RefereeRole)
  bb_referee_grades$RefereeGender <- factor(bb_referee_grades$RefereeGender)
    return(bb_referee_grades)
}



################################
## Run the cleaning functions ##
################################

c.apps <- cleaning_apps(applications)
c.reviews <- cleaning_reviews(reviews)
c.referee <- cleaning_referees(referee_grades)


#######################################
## Columns selection in APPLICATIONS ##
#######################################

# MainDiscipline has 140 levels and MainDisciplineLevel2 has just 21 
c.apps <- c.apps[,-6] # I remove MainDiscipline and keep MainDisciplineLevel2
c.apps <- c.apps[,-7] # I remove CallTitle
c.apps$CallEndDate <- as.Date(c.apps$CallEndDate, format="%Y-%m-%d") # extract year from CallENdDate
c.apps$Year <- format(as.Date(c.apps$CallEndDate, format="%Y-%m-%d"),"%Y") # put it in a new column

## Check for how many observations in 2016 we have ApplicantAcademicAgeAtSubmission:
id <- which(!is.na(c.apps$ResponsibleApplicantAcademicAgeAtSubmission))
y <- c.apps$Year[id]
table(y) #695 in 2016, so for now I keep it

## Check for how many observations in 2016 we have ApplicantProfessorshipType:
id2 <- which(!is.na(c.apps$ResponsibleApplicantProfessorshipType))
y2 <- c.apps$Year[id2]
table(y2) #761 in 2016, so for now I keep it

## Remove NAs for gender since we are interested in it
id.g <- which(is.na(c.apps$Gender)) 
c.apps <- c.apps[-id.g,]

#############
## REVIEWS ##
#############

is.na <- which(is.na(c.reviews$QuestionRating))  # Check if there are NAs in QuestionRating
table(c.reviews$Question[is.na])    ## We have 1860 NAs for BroaderImpact in QuestionRating

sum(is.na(c.reviews$Country))  ## We have 949 NAs for ReviewerCountry
sum(is.na(c.reviews$EmailEnding)) ## We have 118 NAs for EmailEnding

sum(is.na(c.reviews$Country) & !is.na(c.reviews$EmailEnding))
id.country <- which(is.na(c.reviews$Country) & !is.na(c.reviews$EmailEnding))
table(c.reviews$EmailEnding[id.country]) # Country missing refers to Email like EDU,GOV,COM..

##############
## REFEREES ##
##############

is.na <- (is.na(c.referee$QuestionRating)) # Check if there are NAs in QuestionRating: 46
table(c.referee$Question[is.na]) # 22 NAs for Track Record and 24 NAs for Project

sum(is.na(c.referee$OverallComparativeRanking)) # 52 NAs for overall grades

########################################
## Check how many application in 2016 ##
########################################
id.2016 <- which(c.apps$Year==2016)
apps.2016 <- c.apps[id.2016,]

#############################################################################
## Check if for every application in 2016 we have the corresponding grades ##
#############################################################################

projects.id2016 <- unique(apps.2016$ProjectID)
projects.id.review <- unique(c.reviews$ProjectID)
m <- match(projects.id2016,projects.id.review)
sum(is.na(m)) 
# We have 119 Projects_ID in applications for which we don't know the reviewers scores
# (Some were approved and some were not)

## I used Leslie's function after having removed applications which ProjectID is missing in reviews 
id.na <- projects.id2016[which(is.na(m))]
rows.na <- match(id.na,c.apps$ProjectID)
t <- c.apps[-rows.na,]
my_merged2016 <- merge_snsf_data_2016(t,reviews,referee_grades)
n <- unique(my_merged2016$ProjectID)
length(n)
# I got a dataset with 1623 observations

# I compare it with the number of observations in 2016 in the original dataset
t <- c.apps[-rows.na,]
t.2016 <- which(t$Year==2016)
nrow(t.2016) # In the original dataset we have 1623 observations in 2016

# Now we are not missing any observation
# I suppose the problem was with ID which were not in both datasets
