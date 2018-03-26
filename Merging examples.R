########################################################################################
##########   Merging Functions                                                  ########
########################################################################################


setwd("~/MSc Statistics/StatsLab")
load("SNFS Data/snsf_data.RData")
source("Cleaning Functions.R")

# Clear and Rearrenge Data sets
  apps<-d.apps(applications)
  external_reviews <- d.reviews(reviews)
  internal_reviews<-d.referee(referee_grades)

# Select the year
  apps2016<-subset(apps, year(apps$Year)==2016)

      ## Merging apps with referees to add Gender, Year to internal reviews
      Internal2016<-merge(internal_reviews, 
                          app2016[,c("ProjectID","Gender","Year")],
                          by="ProjectID")
      
      length(unique(Internal2016$ProjectID)) #1634
      
      ## Merging apps with reviews to add Gender, Year to internal reviews
      External2016<-merge(external_reviews, 
                          app2016[,c("ProjectID","Gender","Year")],
                          by="ProjectID")
      
      length(unique(External2016$ProjectID)) #1623


# Step 1 - The external review

  step1<-merge(apps2016,external_reviews[c(", by="ProjectID")
  length(unique(step1$ProjectID))
  
# Step 2 - The internal review
  
  step2<-merge(apps2016,internal_reviews, by="ProjectID")
  length(unique(step2$ProjectID))
  
# All the reviews together with the applications
  allreviews<-merge(external_reviews,internal_reviews, by="ProjectID")
  all2016<-merge(apps2016,allreviews, by="ProjectID")
  length(unique(all2016$ProjectID))
  
