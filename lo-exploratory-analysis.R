
### Assumes data set is called Combined

### Look at proportion of men and women receiving the different "OverallGrade" scores


compare_ratings_by_gender <- function(data, question) { # e.g. "OverallGrade"
  target_question <- combined$Question==question
  ratings_by_gender <- table(data[target_question,3], data[target_question,"Gender"]) # Table of Ratings by gender 
  proportional_ratings_by_gender <- prop.table(ratings_by_gender, 2) # Table showing proportions of female & male
  return(list(ratings_by_gender, proportional_ratings_by_gender))
}


ranking <- compare_ratings_by_gender(data = combined, question="OverallComparativeRanking")
overall_grade <- compare_ratings_by_gender(data = combined, question="OverallGrade")
track_record <- compare_ratings_by_gender(data = combined, question="Applicants' scientific track record and expertise")
proposal <- compare_ratings_by_gender(data = combined, question=unique(combined$Question)[3])
suitability <- compare_ratings_by_gender(data = combined, question=unique(combined$Question)[4])
int_proposal <- compare_ratings_by_gender(data = combined, question=unique(combined$Question)[5])





