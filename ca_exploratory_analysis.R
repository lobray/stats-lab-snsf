####################################################
### StatLab - Data review                     ######
####################################################

setwd("~/MSc Statistics/StatsLab")
load("SNFS Data/snsf_data.RData")
source("Analysis/Cleaning Functions.R")


# Cleanning  and Exploring Applications --------------------------------------------------


apps<-d.apps(applications)    
summary(apps)

  # Are there any Not approved with Granted >0
    table(apps$Approved, useNA = "always")
    table(apps$Approved,apps$Granted==0)
    which(applications$IsApproved==0 & applications$AmountGranted>0)
    # No there is non
    
  #For Approvals
    frequencies<-table(apps$Gender,apps$Approved)
    margin.table(frequencies,1)
    prop.table(frequencies,1)

    #For Nationality
    length(levels(apps$Nationality)) #74
    nationality<-as.matrix(table(apps$Nationality))
    summary(nationality)
    
  #Date
    sum(year(apps$Date)==2016)  #1742 applications in 2016
    sum(year(apps$Date)==2017)  #729 applications in 2017
    
    
  #For Division
    table(apps$Division)
    frequencies<-table(apps$Gender,apps$Division)
    margin.table(frequencies,1)
    prop.table(frequencies,1)
  
  #Main Discipline Level 2
    table(apps$`MD Code`)
    length(levels(apps$`MD Code`))  #21 Main disciplines
    max(table(apps$`MD Code`)) #20500 - Enginiering science
    
  #Applicant Academic age at submission. Starting on 2016-10-01 to be accounted for
  # How many NA's
    sum(is.na(apps$AcademicAge)&year(apps$Date)==2016)  #1047
    
    
  #Professorship
    length(levels(apps$Professorship)) #8  Starting on 2016-10-01 to be accounted for
    #[1] "Assistant professor with tenure track"    "Assistant professor without tenure track"
    #[3] "Associate professor"                      "Full professor"                          
    #[5] "Honorary professor or Titular professor"  "None"                                    
    #[7] "Professor at UAS / UTE"                   "Visiting professor"        
    
  #Continuation
    sum(apps$Continue==0) #14953 new applications
    
  
  #Previous project Request
    sum(apps$PreviousRequest==0) #3743 
    
  #Institution  
    levels(apps$InstType) #"ETH"     "Other"   "UAS/UTE" "Uni" 
    prop.table(table(approved=apps$Approved,Institution=apps$InstType),2)
    #67% of the proposals from ETH get approved
    #UAS/UTE has a the higher percentage of rejection, and is also the institution
    #with the higher percentage of womans
    prop.table(table(approved=apps$Gender,Institution=apps$InstType),2)
    
  #verify if Project ID are duplicated
    sum(duplicated(apps$ProjectID))   #0 -> there are no duplicates
    
  #which percentaje are approved per gender 2016
    app2016<- subset(apps,year(apps$Date)==2016)
    (gender_cuota<- prop.table(table(app2016$Gender,app2016$Approved),1))
    
  #which percentaje are approved per gender 2017
    app2017<- subset(apps,year(apps$Date)==2017)
    (gender_cuota<- prop.table(table(app2017$Gender,app2017$Approved),1))
    
# Cleaning and Exploring  Reviews --------------------------------------------------------

    external_reviews<-d.reviews(reviews)  #make a copy
    
    levels(external_reviews$OverallGrade)
    table(external_reviews$OverallGrade)
    
    #how do different gender reviewers grade
    prop.table(table(external_reviews$ReviewerGender,external_reviews$OverallGrade),1)
      
    #Source Person -- Who suggested the reviewer?
      table(external_reviews$SourcePerson)
      
    # Which Nationalities we do not know
      id<-which(is.na(external_reviews$ReviewerCountry))    #To see which ones
      idd<-external_reviews[id,c("ReviewerCountry", "EmailEnding")]
      table(idd$EmailEnding)
      
      #DDU EDU  EU GOV ORG  SU 
      #  1 181   6   9  21   1 

      # Which entries correspond to 2016 applications
      ID<-unique(external_reviews$ProjectID) #projects evaluated
      IDin2016<- match(ID,app2016$ProjectID)
      IDin2016<-IDin2016[-which(is.na(IDin2016))] # 1623 applications
      
     
      # Which entries correspond to 2017
      IDin2017<-match(ID,app2017$ProjectID)
      IDin2017<-IDin2017[-which(is.na(IDin2017))] #727 applications

# Cleaning referee grades -------------------------------------------------

    internal_reviews<-d.referee(referee_grades)
    #Summary tables
      #Ranking
      table(internal_reviews$Ranking)
      
      #Referee Role
      table(internal_reviews$RefereeRole)
      
      #RefereeGender vs. Ranking
      #Woman grades are stricter than males. i.e. woman are more strict
      prop.table(table(internal_reviews$RefereeGender,internal_reviews$Ranking),1)
      
      #ApplicantTrack
      table(internal_reviews$ApplicantTrack)
     
      #Project assesmet
      table(internal_reviews$ProjectAssesment)
      
      # Which entries correspond to 2016 applications
      ID<-unique(internal_reviews$ProjectID) #projects evaluated
      IDin2016<- match(ID,app2016$ProjectID)
      IDin2016<-IDin2016[-which(is.na(IDin2016))] # 1623 applications
      
      
      # Which entries correspond to 2017
      IDin2017<-match(ID,app2017$ProjectID)
      IDin2017<-IDin2017[-which(is.na(IDin2017))] #727 applications
  
# Merging data --------------------------------------------------------
   ### 2016 --------------------------------------------------------------------
   
      app2016<-subset(apps,year(apps$Year)==2016)
      
      ## Merging apps with referees
      Internal2016<-merge(internal_reviews, 
                          app2016[,c("ProjectID","Gender","Year")],
                          by="ProjectID")
      
      length(unique(Internal2016$ProjectID)) #1634
      
      ## Merging apps with reviews
      External2016<-merge(external_reviews, 
                          app2016[,c("ProjectID","Gender","Year")],
                          by="ProjectID")
      
      length(unique(External2016$ProjectID)) #1623
      

   ### 2017 --------------------------------------------------------------------
      
      app2017<-subset(apps,year(apps$Year)==2017)
      length(unique(app2017$ProjectID)) #729
      
      ## Merging apps with referees
      Internal2017<-merge(internal_reviews, 
                          app2017[,c("ProjectID","Gender","Year")],
                          by="ProjectID")
      
      length(unique(Internal2017$ProjectID)) #729
      
      ## Merging apps with reviews
      External2017<-merge(external_reviews, 
                          app2017[,c("ProjectID","Gender","Year")],
                          by="ProjectID")
      
      length(unique(External2017$ProjectID)) #727

    

        