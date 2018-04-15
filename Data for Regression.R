########################################################
### Functions to return the data for the regressions ###
########################################################


source("Cleaning Functions.R")




### Get the data from the Cleaning Functions


test <- selection.function(apps,external_reviews,internal_reviews,2016)
final.apps <- test$final.apps
final.external <- test$final.reviews
final.internal <- test$final.referees

### Logistic Regression for External Criteria

prepare_data_external_log_regression <- function(apps, internal, external) {
  
  # Calculate % female reviewers
  external_reviews_gender <- calculate_percent_female(final.external, "ReviewerGender")
  
  # Select applications data we want to use
  external_regression_data <- final.apps[,c("IsApproved", "Age", "Gender", "Division", "ProjectID")]
  
  # add in grades & Interaction
  average_ratings <- calculate_average_reviewers(final.external)
  
  # Merge applications data with external % females & average reviews
  external_regression_data <- merge(external_reviews_gender, external_regression_data, by="ProjectID")
  external_regression_data <- merge(average_ratings, external_regression_data, by="ProjectID")
  
  # changing variables to factors
  external_regression_data$ApplicantTrack<-as.factor(external_regression_data$ApplicantTrack)
  external_regression_data$ScientificRelevance<-as.factor(external_regression_data$ScientificRelevance)
  external_regression_data$Suitability<-as.factor(external_regression_data$Suitability)
  external_regression_data$OverallGrade<-as.factor(external_regression_data$OverallGrade)
  
  # Create regression object, and return it 
  #external_log_regression <- glm(external_regression_data$IsApproved ~ .-(ProjectID), data=external_regression_data, family="binomial")
  return(external_regression_data)
}


### Logistic Regression data for Internal Criteria


prepare_data_internal_log_regression <- function(apps, internal, external) {

  # Calculate % female reviewers
  internal_reviews_gender <- calculate_percent_female(final.internal, "RefereeGender")
  
  # Extract columns from applications data
  internal_regression_data <- final.apps[,c("IsApproved", "ProjectID", "Gender", "Division", "Age")]
  
  # add later: ranking, track, project assessment
  average_internal_ratings <- calculate_average_referee(internal)
  
  # Merge with % female reviewers & referee data
  internal_regression_data <- merge(internal_regression_data, internal_reviews_gender, by = "ProjectID")
  internal_regression_data <- merge(internal_regression_data, average_internal_ratings, by = "ProjectID")
  
  # changing variables to factors:
  internal_regression_data$Ranking <- factor(internal_regression_data$Ranking)
  internal_regression_data$ProjectAssessment <- factor(internal_regression_data$ProjectAssessment)
  internal_regression_data$ApplicantTrack <- factor(internal_regression_data$ApplicantTrack)
  
  # Create logistic regression & return object
  #internal_log_regression <- glm(internal_regression_data$IsApproved ~ .-(ProjectID), family="binomial", data = internal_regression_data)
  return(internal_regression_data)
}

### Logistic Regression for Final Board Deicision


prepare_data_board_log_regression <- function(apps, internal, external) {
  
  # Extract columns from applications data
  board_regression_data <- final.apps[,c("IsApproved", "ProjectID", "Gender", "Division", "Age", "AmountRequested","IsContinuation")]
  
  # Calculate average ratings for internal and external reviews
  average_internal_ratings <- calculate_average_referee(internal)[,c(1,4)]
  average_ratings <- calculate_average_reviewers(external)[,c(1,5)]
  
  # Merge with external reviews & referee data
  board_regression_data <- merge(board_regression_data, average_internal_ratings, by = "ProjectID")
  board_regression_data <- merge(board_regression_data, average_ratings, by = "ProjectID")
  
  # Create logistic regression & return object
  #board_log_regression <- glm(board_regression_data$IsApproved ~ .-(ProjectID), family="binomial", data = board_regression_data)
  return(board_regression_data)
}




