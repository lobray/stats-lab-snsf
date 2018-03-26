########################################################################################
##########   Merging Functions                                                  ########
########################################################################################


setwd("~/MSc Statistics/StatsLab")
load("SNFS Data/snsf_data.RData")
source("Cleaning Functions.R")

#Select the year
  apps<-d.apps(applications)
  apps2016<-subset(apps, year(apps$Year)==2016)


# Step 1 - The external review

  external_reviews <- d.reviews(reviews)
  step1<-merge(apps2016,external_reviews, by="ProjectID")
  length(unique(step1$ProjectID))
  
# Step 2 - The internal review
  internal_reviews<-d.referee(referee_grades)
  step2<-merge(apps2016,internal_reviews, by="ProjectID")
  length(unique(step2$ProjectID))
  
# All the reviews together with the applications
  allreviews<-merge(external_reviews,internal_reviews, by="ProjectID")
  all2016<-merge(apps2016,allreviews, by="ProjectID")
  length(unique(all2016$ProjectID))
  