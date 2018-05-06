##############################################################
#######   Graphs to show perfect separation
#############################################################




data<-prepare_data_external_log_regression(final.apps,final.external)


# visualization of grades to check for perfect separation

#ApplicantTrack

r.tab<-table(data$ApplicantTrack, data$IsApproved)
r.tab.dataframe <- as.data.frame(r.tab)    
colnames(r.tab.dataframe) <- c("ApplicantTrack", "IsApproved", "Freq")


ggplot(r.tab.dataframe,aes(x=ApplicantTrack,y=Freq,fill=factor(IsApproved)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="ApplicantTrack",
                      breaks=c(1, 2),
                      labels=c( "Approved","Not Approved"))+
  ylab("Frequency") +xlab("")+
  ggtitle("External Step Grade distribution")+
  theme(legend.position="bottom")


#ProposalCombined

r.tab<-table(data$ProposalCombined, data$IsApproved)
r.tab.dataframe <- as.data.frame(r.tab)    
colnames(r.tab.dataframe) <- c("ProposalCombined", "IsApproved", "Freq")


ggplot(r.tab.dataframe,aes(x=ProposalCombined,y=Freq,fill=factor(IsApproved)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="ProposalCombined",
                      breaks=c(1, 2),
                      labels=c( "Approved","Not Approved"))+
  ylab("Frequency") +xlab("")+
  ggtitle("External Step Grade distribution")+
  theme(legend.position="bottom")


# Internal graphs----
data<-prepare_data_internal_log_regression(final.apps,final.internal)

#ApplicantTrack

r.tab<-table(data$ApplicantTrack, data$IsApproved)
r.tab.dataframe <- as.data.frame(r.tab)    
colnames(r.tab.dataframe) <- c("ApplicantTrack", "IsApproved", "Freq")


ggplot(r.tab.dataframe,aes(x=ApplicantTrack,y=Freq,fill=factor(IsApproved)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="ApplicantTrack",
                      breaks=c(1, 2),
                      labels=c( "Approved","Not Approved"))+
  ylab("Frequency") +xlab("")+
  ggtitle("Internal Step Grade distribution")+
  theme(legend.position="bottom")


#ProjectAssessment

r.tab<-table(data$ProjectAssessment, data$IsApproved)
r.tab.dataframe <- as.data.frame(r.tab)    
colnames(r.tab.dataframe) <- c("ProjectAssessment", "IsApproved", "Freq")


ggplot(r.tab.dataframe,aes(x=ProjectAssessment,y=Freq,fill=factor(IsApproved)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="ProjectAssessment",
                      breaks=c(1, 2),
                      labels=c( "Approved","Not Approved"))+
  ylab("Frequency") +xlab("")+
  ggtitle("Internal Step Grade distribution")+
  theme(legend.position="bottom")

