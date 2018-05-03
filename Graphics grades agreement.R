######################################################
## Agreement between internal criteria and Ranking  ##
######################################################

setwd("~/StatLab")
load("snsf_data.RData")
source("Cleaning Functions.R")
source("Data for Regression.R")
library(ggplot2)

rm(applications,reviews,referee_grades, test)

internal_regression_data<-prepare_data_internal_log_regression(final.apps,final.internal)

## Applicant Track
ggplot(internal_regression_data, aes(x = ApplicantTrack, y = Ranking, col=Gender)) +
  geom_jitter(alpha = .5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Agreement between Applicant Track and Ranking")

  # Clear positive correlation

## Project Assessment
ggplot(internal_regression_data, aes(x = ProjectAssessment, y = Ranking, col=Gender)) +
  geom_jitter(alpha = .5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Agreement between Project Assessment and Ranking")

  # Almost perfect correlation



######################################################
## Agreement between external criteria and Ranking  ##
######################################################

external_regression_data<-prepare_data_external_log_regression(final.apps,final.external)

## Applicant Track
ggplot(external_regression_data, aes(x = ApplicantTrack, y = OverallGrade, col=Gender)) +
  geom_jitter(alpha = .5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Agreement between Applicant Track and OverallGrade")

# Clear positive correlation, only a few grades below 4

## Suitability
ggplot(external_regression_data, aes(x = Suitability, y = OverallGrade, col=Gender)) +
  geom_jitter(alpha = .5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Agreement between Suitability of methods and OverallGrade")

# Almost perfect correlation

## Scientific Relevance
ggplot(external_regression_data, aes(x = ScientificRelevance, y = OverallGrade, col=Gender)) +
  geom_jitter(alpha = .5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Agreement between Scientific Relevance and OverallGrade")

# Again almost perfect correlation


## ProposalCombined
ggplot(external_regression_data, aes(x = ProposalCombined, y = OverallGrade, col=Gender)) +
  geom_jitter(alpha = .5) +  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Agreement between ProposalCombined and OverallGrade")

# Almost perfect correlation