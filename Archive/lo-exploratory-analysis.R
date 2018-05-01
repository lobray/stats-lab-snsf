
### Assumes data set is called Combined & also have reduced_combined

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


### Boxplot of amount requested & amount granted by gender

boxplot(reduced_combined$AmountRequested~reduced_combined$Gender)
boxplot(reduced_combined$AmountGranted~reduced_combined$Gender)

### Boxplot of amount requested & amount granted by divison & gender

boxplot(reduced_combined[,"AmountRequested"]~reduced_combined$Gender+reduced_combined$Division)
boxplot(reduced_combined[reduced_combined$IsApproved==1,"AmountGranted"]~
          reduced_combined[reduced_combined$IsApproved==1, "Gender"]+reduced_combined[reduced_combined$IsApproved==1,"Division"])

### Look at distribution of OverallGrade given: interesting finding, 20% overall grades are outstanding, 38% are excellent

overall_grade_barplot <- function(reduced_combined) {
  ex_rating <- reduced_combined[reduced_combined$QuestionRating 
                                %in% c("poor", "average", "good", "very good", "excellent", "outstanding"),]
  ex_rating <- ex_rating[ex_rating$Question == "OverallGrade",]
  tmp_table <- table(subset(reduced_combined, Question=="OverallGrade", select="QuestionRating"))
  tmp_prop_table <- prop.table(tmp_table)
  tmp_prop_table <- tmp_prop_table[tmp_prop_table != 0]
  print(tmp_prop_table)
  barplot(tmp_prop_table[c(7,5,1,3,6,2,4)])
}



