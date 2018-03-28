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
  average_ratings <- calculate_average_reviewers(external)
  
  # Merge applications data with external % females & average reviews
  external_regression_data <- merge(external_reviews_gender, external_regression_data, by="ProjectID")
  external_regression_data <- merge(average_ratings, external_regression_data, by="ProjectID")
  
  # Create regression object, and return it 
  external_log_regression <- glm(external_regression_data$IsApproved ~ .-(ProjectID), data=external_regression_data, family="binomial")
  
}

external_log_regression <- prepare_data_external_log_regression(final.apps, final.internal, final.external)
summary(external_log_regression)


  
### Logistic Regression for Internal Criteria


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
  
  # Create logistic regression & return object
  internal_log_regression <- glm(internal_regression_data$IsApproved ~ .-(ProjectID), family="binomial", data = internal_regression_data)
  
}

internal_log_regression <- prepare_data_external_log_regression(applications, final.internal, final.external)
summary(internal_log_regression)


