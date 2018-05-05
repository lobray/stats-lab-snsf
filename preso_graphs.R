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



