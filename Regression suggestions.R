# First, I would suggest to set the reference level for the gender variable as M
# in order to easily interpret the results.
# If you all agree, I would add these lines to the cleaning functions

final.external$Gender <- relevel(final.external$Gender, ref="m")
final.internal$Gender <- relevel(final.internal$Gender, ref="m")
final.apps$Gender <- relevel(final.apps$Gender, ref="m")

# Second, I noticed that all our regression outputs have a huge Residual Deviance, 
# much bigger than the degrees of freedom. Since we already saw that in different divisions
# the male/female ratio is different, I suggest not to include the variable Division in the model
# but to subset the dataset by Division and then run 3 different logistic regressions.
# I add the argument d = division to the existing function and a few lines to subset

prepare_data_external_log_regression <- function(apps, internal, external, d) {
  
  # Calculate % female reviewers
  external_reviews_gender <- calculate_percent_female(final.external, "ReviewerGender")
  
  # Select applications data we want to use
  external_regression_data <- final.apps[,c("IsApproved", "Age", "Gender", "Division", "ProjectID")]
  
  # add in grades & Interaction
  average_ratings <- calculate_average_reviewers(external)
  
  # Merge applications data with external % females & average reviews
  external_regression_data <- merge(external_reviews_gender, external_regression_data, by="ProjectID")
  external_regression_data <- merge(average_ratings, external_regression_data, by="ProjectID")
  
  external_div <- subset(external_regression_data, Division==d)
  drop <- which(colnames(external_div)=="Division")
  external_div <- external_div[,-drop]
  # Create regression object, and return it 
  external_log_regression <- glm(external_div$IsApproved ~ .-(ProjectID), data=external_div, family="binomial")
  
}

external_log_regression_div1 <- prepare_data_external_log_regression(final.apps, final.internal, final.external, "Div 1")
summary(external_log_regression_div1)


# In this way, for external reviews we obtain around 700 Residual Deviance on 500 df, 
# instead of the 2176 on 1613 df that we had before.
# I know that it's still high but better than before...


# Finally, I tried including an interaction between the Gender and the Percentage of Women

log_with_interactions <- function(apps, internal, external) {
  
  # Calculate % female reviewers
  external_reviews_gender <- calculate_percent_female(final.external, "ReviewerGender")
  
  # Select applications data we want to use
  external_regression_data <- final.apps[,c("IsApproved", "Age", "Gender", "Division", "ProjectID")]
  
  # add in grades & Interaction
  average_ratings <- calculate_average_reviewers(external)
  
  # Merge applications data with external % females & average reviews
  external_regression_data <- merge(external_reviews_gender, external_regression_data, by="ProjectID")
  external_regression_data <- merge(average_ratings, external_regression_data, by="ProjectID")
  
  external_div1 <- subset(external_regression_data, Division=="Div 1")
  drop <- which(colnames(external_div1)=="Division")
  external_div1 <- external_div1[,-drop]
  
  # Create regression object, and return it 
  external_log_regression <- glm(external_div1$IsApproved ~ ApplicantTrack + ScientificRelevance +
                                   Suitability + OverallGrade + PercentFemale + Age + Gender +
                                   Gender:PercentFemale, data=external_div1, family="binomial")
  
}

fit <- log_with_interactions(final.apps, final.internal, final.external)
summary(fit)

# I was also wondering if it makes sense to include both the OverallGrade and the Criteria
# in the same model, because I suppose that they are somehow correlated... 
# Maybe we can discuss it tomorrow...