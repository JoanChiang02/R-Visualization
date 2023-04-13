
diving <- read.csv("Diving2000_mod.csv")
diving_distinct <- diving %>% distinct(Round, Diver, DiveNo, .keep_all = TRUE)

diving_distinct$Round <- factor(diving_distinct$Round, levels = c("Final", "Semi", "Prelim"))
color_list <- c("navy","slateblue2","deepskyblue")

plot_ly(diving_distinct, x = ~Event, y = ~Difficulty, z = ~AvgScore, color = ~Round,size=28,colors = color_list, hoverinfo = 'text',text=~paste("Diver:",Diver,"\nCountry:",Country,"\nRank:",Rank,"\n DiveNo:",DiveNo,"\nAvgScore:",round(AvgScore,2),"\nDifficulty:",Difficulty)) %>% layout(title="2000 Sydney Olympic Diving Result")
#,caption="2000 Olympic Diving @JoanChiang Data:Emerson & Meredith (2011)"
