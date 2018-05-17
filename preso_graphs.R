
library(biostatUZH)
library(psy)
library(psych)
library(ggplot2)
library(gridExtra)
library(coin)
library(ggmosaic)


########################################################################################################################
# Function to show mirrored bar plots of external criteria versus approved / not approved step
########################################################################################################################
plot_mirror_barplot <- function(dataset, variable1, variable2="IsApproved", plot_title="Plot Title", title_size=8) {
  table_data_frame <- as.data.frame(table(dataset[,variable1], dataset[,"IsApproved"]))
  colnames(table_data_frame) <- c(variable1, variable2, "Frequency")
  levels(table_data_frame[,variable1]) <- list("Outstanding"="6", "Excellent"="5", 
                                               "Very Good"="4", "Good"="3", "Average"="2", "Poor"="1")
  table_data_frame$Frequency[table_data_frame[,"IsApproved"]==0] <- -table_data_frame$Frequency[table_data_frame[,"IsApproved"]==0]
  
  ggplot(table_data_frame, aes_string(x=variable1, y="Frequency", fill=variable2)) + 
    geom_bar(stat="identity", position="identity") + 
    scale_y_continuous(breaks=seq(-100,100,by=50),labels=abs(seq(-100,100,by=50))) +
    coord_flip() +
    ggtitle(plot_title) + 
    theme(plot.title = element_text(size = title_size)) 
    # scale_fill_manual(values = c("firebrick","darkseagreen4"))
    
}

# Data to use

external_regression_data <- prepare_data_external_log_regression(apps=final.apps, external=final.external)
ex.f <- external_regression_data[external_regression_data$Gender=="f",]
ex.m <- external_regression_data[external_regression_data$Gender=="m",]
ex.div1 <- external_regression_data[external_regression_data$Division=="Div 1",] # particularly bad for Div 1
ex.div2 <- external_regression_data[external_regression_data$Division=="Div 2",]
ex.div3 <- external_regression_data[external_regression_data$Division=="Div 3",]


#### internal datasets to use 
internal_regression_data<-prepare_data_internal_log_regression(apps=final.apps, internal=final.internal)
in.f <- internal_regression_data[internal_regression_data$Gender=="f",]
in.m <- internal_regression_data[internal_regression_data$Gender=="m",]
in.div1 <- internal_regression_data[internal_regression_data$Division=="Div 1",] # particularly bad for Div 1
in.div2 <- internal_regression_data[internal_regression_data$Division=="Div 2",]
in.div3 <- internal_regression_data[internal_regression_data$Division=="Div 3",]

#### board data
board_data <- prepare_data_board_log_regression(apps=final.apps, internal = final.internal, external = final.external)




# Distribution of grades: ranking vs. final grade

int_ranking <- as.data.frame(prop.table(table(internal_regression_data$Ranking)))
int_ranking$Freq <- as.numeric(int_ranking$Freq)
colnames(int_ranking)[1] <- "Ranking"
p1 <- ggplot(int_ranking, aes(x=Ranking, y= Freq)) +
  geom_histogram(stat="identity") +
  ggtitle("Internal Ranking Frequency Distribution") + 
  geom_text(aes(label=round(Freq, 2),vjust=1.5))

jpeg("/home/leslie/Desktop/StatsLabs/stats-lab-snsf/Graphs/RankingDist.jpg")
p1
dev.off()

board_final_grade <- as.data.frame(prop.table(table(rev(board_data$GradeFinal))))
board_final_grade$Freq <- as.numeric(board_final_grade$Freq)
colnames(board_final_grade)[1] <- "BoardRanking"
p2 <- ggplot(board_final_grade, aes(x=BoardRanking, y= Freq)) +
  geom_histogram(stat="identity") +
  ggtitle("Board Ranking Frequency Distribution") + 
  geom_text(aes(label=round(Freq, 2),vjust=1.5))

jpeg("/home/leslie/Desktop/StatsLabs/stats-lab-snsf/Graphs/BoardRankingDist.jpg")
p2
dev.off()

# Distribution of Overall Grades

ex_OG <- as.data.frame(prop.table(table(external_regression_data$OverallGrade)))
ex_OG$Freq <- as.numeric(ex_OG$Freq)
colnames(ex_OG)[1] <- "OverallGrade"
p3 <- ggplot(ex_OG, aes(x=OverallGrade, y= Freq)) +
  geom_histogram(stat="identity") +
  ggtitle("External OverallGrade Frequency Distribution") + 
  geom_text(aes(label=round(Freq, 2),vjust=1.5))

jpeg("/home/leslie/Desktop/StatsLabs/stats-lab-snsf/Graphs/OverallGradeDist.jpg")
p3
dev.off()


# OverallGrade vs IsApproved
p4 <- plot_mirror_barplot(dataset=external_regression_data, 
                          variable1 = "OverallGrade", plot_title = "OverallGrade vs. Approved; All Data")

p5 <- plot_mirror_barplot(dataset=internal_regression_data, 
                          variable1 = "Ranking", plot_title = "Ranking vs. Approved; All Data")

g <- arrangeGrob(p4, p5, nrow=1) #generates g
ggsave(file="/home/leslie/Desktop/StatsLabs/stats-lab-snsf/Graphs/OverallGradevsRankingIsApproved.jpg", g)
pdf("/home/leslie/Desktop/StatsLabs/stats-lab-snsf/Graphs/OverallGradevsRankingIsApproved.pdf")
grid.arrange(p4, p5, ncol = 2)
dev.off()

# mosaic plots
library(vcd)
cotabplot(~ Gender+IsApproved, data=final.apps, shade=T)


tab <- xtabs(IsApproved ~ Gender, data = final.apps)
tab
tab <- structable(Freq ~ Gender + IsApproved, data = tab)
tab <- as.data.frame(table(final.apps$IsApproved, final.apps$Gender))
colnames(tab) <- c("IsApproved", "Gender", "Freq")
tab <- xtabs(Freq ~ Gender + IsApproved, data=tab)
mosaic(tab)

cotabplot(~ Gender+IsApproved, data=final.apps[final.apps[,"Division"]=="Div 1",], shade=T)
cotabplot(~ Gender+IsApproved, data=final.apps[final.apps[,"Division"]=="Div 2",], shade=T)
cotabplot(~ Gender+IsApproved, data=final.apps[final.apps[,"Division"]=="Div 3",], shade=T)
par(mfrow=c(1,3))
cotabplot( ~ IsApproved + Gender | Division, data=final.apps, panel= cotab_mosaic, shade=T)


ProposalCombined <- external_regression_data$ProposalCombined
ex_proposal <- as.data.frame(prop.table(table(ProposalCombined)))
ex_proposal$Freq <- as.numeric(ex_proposal$Freq)
colnames(ex_proposal)[1] <- "ProposalScore"
p8 <- ggplot(ex_proposal, aes(x=ProposalScore, y= Freq)) +
  geom_histogram(stat="identity") +
  ggtitle("External Proposal Frequency Distribution") +
  geom_text(aes(label=round(Freq, 2),vjust=1.5)) 
  scale_y_continuous(limits = c(0, 0.45))

in_proposal <- as.data.frame(prop.table(table(internal_regression_data$ProjectAssessment)))
in_proposal$Freq <- as.numeric(in_proposal$Freq)
colnames(in_proposal)[1] <- "ProposalScore"
p9  <- ggplot(in_proposal, aes(x=ProposalScore, y= Freq)) +
  geom_histogram(stat="identity") +
  ggtitle("Internal Proposal Frequency Distribution") +
  geom_text(aes(label=round(Freq, 2),vjust=1.5)) +
  scale_y_continuous(limits = c(0, 0.41))
grid.arrange(p8, p9, ncol=2)

jpeg("/home/leslie/Desktop/StatsLabs/stats-lab-snsf/Graphs/ExProposalDist.jpg")
p8
dev.off()

# How males and females allocate grades

tmp.external.data <- merge(final.apps[,c("Gender", "ProjectID")], external_reviews, by="ProjectID")

# just females
tmp.external.data.f <- tmp.external.data[tmp.external.data[,"Gender"]=="f",]

# just males
tmp.external.data.m <- tmp.external.data[tmp.external.data[,"Gender"]=="m",]

r.tab.2<-prop.table(table(tmp.external.data$OverallGrade, tmp.external.data$ReviewerGender),2)
r.tab.dataframe.2 <- as.data.frame(r.tab.2)    
colnames(r.tab.dataframe.2) <- c("OverallGrade", "ReviewerGender", "Freq")

# How men and women allocate overallgrades

p10 <- ggplot(r.tab.dataframe.2,aes(x=OverallGrade,y=Freq,fill=factor(ReviewerGender)))+
  geom_bar(stat="identity",position="dodge")+
  xlab("OverallGrade")+ylab("Proportion of OverallGrades allocated by Male & Female Reviewers") +
  ggtitle("Difference in how Female & Male Allocate OverallGrades") +
  scale_fill_manual(values=c("orchid4", "darkorange1")) + 
  scale_y_continuous(limits = c(0, 0.40))

# Internal Rankings


tmp.internal.data <- merge(final.apps[,c("Gender", "ProjectID")], final.internal, by="ProjectID")

# just females
tmp.internal.data.f <- tmp.internal.data[tmp.internal.data[,"Gender.x"]=="f",]

# just males
tmp.internal.data.m <- tmp.internal.data[tmp.internal.data[,"Gender.x"]=="m",]

r.tab.2<-prop.table(table(tmp.internal.data$Ranking, tmp.internal.data$RefereeGender),2)
r.tab.dataframe.2 <- as.data.frame(r.tab.2)    
colnames(r.tab.dataframe.2) <- c("Ranking", "RefereeGender", "Freq")


p11 <- ggplot(r.tab.dataframe.2,aes(x=Ranking,y=Freq,fill=factor(RefereeGender)))+
  geom_bar(stat="identity",position="dodge")+
  xlab("Ranking")+ylab("Proportion of OverallGrades allocated by Male & Female Reviewers") +
  ggtitle("Difference in how Female & Male Allocate OverallGrades") +
  scale_fill_manual(values=c("orchid4", "darkorange1")) + 
  scale_y_continuous(limits = c(0, 0.4))



grid.arrange(p10, p11, ncol=2)

568+692 = 1260
194+169 = 363

363/1623

# approval rate``
692/1260
169/363

# male female of approved
692/(692+169)
169/(692+169)



prop.table(table(final.apps$Division, final.apps$IsApproved),1)

    