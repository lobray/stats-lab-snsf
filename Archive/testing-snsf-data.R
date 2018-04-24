


### Function to validate things we observe from the external reviews data

test_external_data <- function(applications, ex_reviews) {
  
  ### Make copy & rename data
  apps <- applications # make copy of applications
  external_reviews <- ex_reviews # make copy
  
  ### select just the year and ProjectID from apps
  apps_prodID_year <- apps[,c("ProjectID", "CallEndDate")]
  
  external_plus_year <- merge(external_reviews, apps_prodID_year, by="ProjectID") # add year into external data set
  external_2016 <- external_plus_year[external_plus_year[,"CallEndDate"] %in% c("2016-04-01", "2016-10-01"), ]  # subset just 2016 data
}

### Function to validate things we observe from the internal reviews data

test_internal_data <- function(applications, int_reviews) {
  
  ### Make copy & rename data
  apps <- applications # make copy of applications
  internal_reviews <- int_reviews # make copy
  
  ### select just the year and ProjectID from apps
  apps_prodID_year <- apps[,c("ProjectID", "CallEndDate")]
  
  internal_plus_year <- merge(internal_reviews, apps_prodID_year, by="ProjectID") # add year into internal data set
  external_2016 <- internal_plus_year[internal_plus_year[,"CallEndDate"] %in% c("2016-04-01", "2016-10-01"), ]  # subset just 2016 data
}

sum(unique(ab$ReviewerID) %in% (unique(bc$RefereeID)))
