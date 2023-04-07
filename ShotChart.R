
#Package
if(!require(ncaahoopR)) install.packages("ncaahoopR")
library(ncaahoopR)
library(tidyverse)
library(ggplot2)
library(ggimage)

# Type the team and player

team_list=c("Alabama","Texas A&M-CC","Maryland","West Virginia","San Diego State","Charleston","UVA","Furman","Creighton","NC State","Baylor","UCSB","Missouri","Utah State","Arizona","Princeton","Houston","N Kentucky","Iowa","Auburn","Miami","Drake","Indiana","Kent State","Iowa State","Miss St","Pitt","Xavier","Kennesaw State",
            "Texas A&M","Penn State","Texas","Colgate","Purdue","Texas Southern","Fair Dickinson","Memphis","FAU","Duke","Oral Roberts","Tennessee","Louisiana","Kentucky","Providence","Kansas State","Montana State","Michigan State","USC","Marquette","Vermont","Kansas","Howard","Arkansas","Illinois","Saint Mary's","VCU","UConn","Iona","TCU","Arizona State","Nevada","Gonzaga","Grand Canyon","Northwestern","Boise State","UCLA","UNC Asheville")
Player_list=c("Brandon Miller","Isaac Mushila","Jahmir Young","Erik Stevenson","Matt Bradley","Ryan Larson","Kihei Clark","Mike Bothwell","Ryan Kalkbrenner","Jarkel Joiner","Adam Flagler","Ajay Mitchell","Kobe Brown","Steven Ashworth","Azuolas Tubelis","Tosan Evbuomwan","Marcus Sasser","Marques Warrick","Kris Murray","Johni Broome","Jordan Miller","Tucker DeVries","Trayce Jackson-Davis","Sincere Carry","Jaren Holmes","Tolu Smith","Jamarius Burton","Souley Boum","Chris Youngblood",
              "Wade Taylor IV","Jalen Pickett","Marcus Carr","Keegan Records","Zach Edey","John Walker III","Grant Singleton","Kendric Davis","Johnell Davis","Kyle Filipowski","Max Abmas","Santiago Vescovi","Terence Lewis II","Oscar Tshiebwe","Ed Croswell","Markquis Nowell","RaeQuan Battle","Joey Hauser","Boogie Ellis","Tyler Kolek","Dylan Penn","Jalen Wilson","Elijah Hawkins","Ricky Council IV","Terrence Shannon Jr.","Logan Johnson","Adrian Baldwin Jr.","Adama Sanogo","Walter Clayton Jr.","Mike Miles Jr.","Desmond Cambridge Jr.","Jarod Lucas","Drew Timme","Rayshon Harrison","Boo Buie","Tyson Degenhart","Jaime Jaquez Jr.","Drew Pember")
test<-data.frame(team_list,Player_list)
seed_list=seq(1:67)

#id data
id<-data.frame(ncaahoopR::id)
color <-data.frame(ncaahoopR::ncaa_colors)

#half court data
court<-data.frame(ncaahoopR::court)

court<-court %>% filter(descri=="3pts bas gauche" | 
                          descri ==	"3pts bas droit" |
                          descri ==	"cercle 3pts" |
                          descri ==	"cercle LF haut" |
                          descri == "anneau" |
                          descri ==	"LF bas gauche"|
                          descri ==	"LF bas droit" |
                          descri ==	"LF tireur" |
                          descri ==	"planche" |
                          descri ==	"ligne de fond")


#Function

team_col <-function(Team){
  print(Team)
  color<-data.frame(ncaahoopR::ncaa_colors)
  team_col<-color %>% filter(espn_name==Team)
  return(team_col)
}
shotdata <- function(Team,Player){
  if(Player != "none"){
    pbp<-get_pbp(Team,extra_parse = TRUE)
    shotdata<-pbp %>% select('game_id','shot_x','shot_y','shot_team','shot_outcome','shooter','three_pt') %>% drop_na(shot_y)%>%filter(shot_y<1000)
    shotdata<-shotdata %>% mutate(adjust_x=(50-shot_x),adjust_y=(100-shot_y))
    shotdata <- shotdata %>% mutate (x=case_when(shot_y>50~adjust_x,shot_y<50~shot_x),y=case_when(shot_y>50~adjust_y,shot_y<50~shot_y))
    shotdata <- shotdata %>% mutate (star_x=case_when(shooter==Player~x),star_y=case_when(shooter==Player~y))
    return(shotdata)
  }
}


for(i in 1:1){
  Team<-team_list[i]
  Player<-Player_list[i]
  team_color <- team_col(Team)
  teamdata <- shotdata(Team, Player)
  logo<-ggplot(team_color,aes(x=0,y=0,image=logo_url))+geom_image(size = 0.5, by="height")+theme_void()
  teamshotchart <- ggplot(teamdata,aes(x=x,y=y))+geom_point(alpha=0.2,color="red")+stat_density_2d(aes(fill=after_stat(nlevel)),geom="polygon",bins=20,alpha=0.5)+scale_fill_gradientn(colours=c("skyblue","yellow","red"),trans="log10")+theme_void()+geom_path(data=court,aes(x=x,y=y,group=group),color="black")+xlim(0,50)+ylim(0,47)
  starshotshot <- ggplot(teamdata,aes(x=star_x,y=star_y))+geom_point(alpha=0.2,color="blue")+stat_density_2d(aes(fill=after_stat(nlevel)),geom="polygon",bins=20,alpha=0.5)+scale_fill_gradientn(colours=c("skyblue","yellow","red"),trans="log10")+theme_void()+geom_path(data=court,aes(x=x,y=y,group=group),color="black")+xlim(0,50)+ylim(0,47)
  ggsave(plot=logo,filename=paste(Team,"_logo.png"),width=4,height=3,device="png",path="shotchartplot")
  ggsave(plot=teamshotchart,filename=paste(Team,"_shotchart.png"),width=4,height=3,device="png",dpi=900,path="shotchartplot")
  ggsave(plot=starshotshot,filename=paste(Team,Player,"_shotchart.png"),width=4,height=3,device="png",path="shotchartplot")
}



