
library(readr)
#install.packages("percentiles")
library(percentiles)
library(dplyr)
library(fmsb)
library(ggplot2)


radar_data <- read.csv("men.csv")
radar_data <- radar_data %>% mutate(
                                    Rebound_avg=general_rebounds/Game_played,
                                    Assists_avg =offensive_assists/Game_played,
                                    Turnover_avg = offensive_total_turnovers/Game_played,
                                    FGA_avg=offensive_field_goals_attempted/Game_played,
                                    '3FGA_avg' = offensive_three_point_field_goals_attempted/Game_played,
                                    Possession_avg = offensive_estimated_possessions/Game_played)

radar_data <- radar_data %>% mutate(block_rank = rank(-defensive_blocks,ties.method="random"),
                                    Rebound=rank(-Rebound_avg,ties.method="random"),
                                    Assists =rank(-Assists_avg,ties.method="random"),
                                    Turnover = rank(Turnover_avg,ties.method="random"),
                                    FGA=rank(-FGA_avg,ties.method="random"),
                                    '3FGA' = rank(-'3FGA_avg',ties.method="random"),
                                    Possession = rank(-Possession_avg,ties.method="random"))
                                  
final_data <- radar_data %>% select(team_display_name,Rebound,Assists,Turnover,FGA,'3FGA',Possession) 
final_data<-as.data.frame(final_data)
for (i in 1:65){
  par(family = "Times New Roman",font=1)
  team_data <-final_data[i,]
  chart_data <- rbind(rep(1,6),rep(65,6),team_data[,-1])
  radarchart(chart_data,cglty = 1,cglcol = "gray",plwd = 1:5,caxislabels=c("best","","","","worst"),calcex=0.8,pfcol=rgb(0.2,0.5,0.5,0.5),pcol=rgb(0.2,0.5,0.5,0.9),axistype = 2,axislabcol="gray",cglwd=1.0,vlcex=0.8)
  legend(x=1.4,y=1.2,legend = paste(i), bty = "n",pch = 10, text.col = "black")
  # png(filename=paste(team_data$team_display_name,"_radarchart.png"))
  # dev.off()
}
#


