setwd("/home/leslie/Desktop/stats-lab-snsf")

merge_snsf_data_2016 <- function(applications, reviews, referee_grades) {

  ## Rename data
  apps <- applications # make copy of applications
  aa_reviews <- reviews # make copy
  bb_referee_grades <- referee_grades # make copy

  ## First, clean up reviews data, and prepare it to be merged wtih referee data
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


  ## Next, clean up referee data, and prepare it to be merged with reviews data
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
  testing_merge <- merge(full_review_profile, apps, by="ProjectID") # merges everything into one!
  merged_2016a <- testing_merge[testing_merge[,"CallEndDate"] ==  "2016-04-01",] # Just called in April 2016
  merged_2016b <- testing_merge[testing_merge[,"CallEndDate"] ==  "2016-10-01",] # just from October 2016
  merged_2016 <- rbind(merged_2016a, merged_2016b) # all apps from 2016

  return(merged_2016)
}
