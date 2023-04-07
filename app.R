#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if(!require(ncaahoopR)) install.packages("ncaahoopR")
if(!require(patchwork)) install.packages("patchwork")
if(!require(ggpubr)) install.packages("ggpubr")
library(shiny)
library(ggpubr)
library(gridExtra)
library(grid)
library(patchwork)
library(ncaahoopR)
library(tidyverse)
library(ggplot2)
library(ggimage)



round_list=c("Round of 64","Round of 32","Sweet 16","Elite 8","Final Four","Championship")
team_list=c("Alabama","Texas A&M-CC","Maryland","West Virginia","San Diego State","Charleston","UVA","Furman","Creighton","NC State","Baylor","UCSB","Missouri","Utah State","Arizona","Princeton","Houston","N Kentucky","Iowa","Auburn","Miami","Drake","Indiana","Kent State","Iowa State","Miss St","Pitt","Xavier","Kennesaw State",
            "Texas A&M","Penn State","Texas","Colgate","Purdue","Fair Dickinson","Memphis","FAU","Duke","Oral Roberts","Tennessee","Louisiana","Kentucky","Providence","Kansas State","Montana State","Michigan State","USC","Marquette","Vermont","Kansas","Howard","Arkansas","Illinois","Saint Mary's","VCU","UConn","Iona","TCU","Arizona State","Gonzaga","Grand Canyon","Northwestern","Boise State","UCLA","UNC Asheville")
player_list=c("Brandon Miller","Isaac Mushila","Jahmir Young","Erik Stevenson","Matt Bradley","Ryan Larson","Kihei Clark","Mike Bothwell","Ryan Kalkbrenner","Jarkel Joiner","Adam Flagler","Ajay Mitchell","Kobe Brown","Steven Ashworth","Azuolas Tubelis","Tosan Evbuomwan","Marcus Sasser","Marques Warrick","Kris Murray","Johni Broome","Jordan Miller","Tucker DeVries","Trayce Jackson-Davis","Sincere Carry","Jaren Holmes","Tolu Smith","Jamarius Burton","Souley Boum","Chris Youngblood",
              "Wade Taylor IV","Jalen Pickett","Marcus Carr","Keegan Records","Zach Edey","Grant Singleton","Kendric Davis","Johnell Davis","Kyle Filipowski","Max Abmas","Santiago Vescovi","Terence Lewis II","Oscar Tshiebwe","Ed Croswell","Markquis Nowell","RaeQuan Battle","Joey Hauser","Boogie Ellis","Tyler Kolek","Dylan Penn","Jalen Wilson","Elijah Hawkins","Ricky Council IV","Terrence Shannon Jr.","Logan Johnson","Adrian Baldwin Jr.","Adama Sanogo","Walter Clayton Jr.","Mike Miles Jr.","Desmond Cambridge Jr.","Drew Timme","Rayshon Harrison","Boo Buie","Tyson Degenhart","Jaime Jaquez Jr.","Drew Pember")
seed_list=seq(1:68)


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

vegas_odd <-function(number){
  bar <-data.frame(x=c(number,10-number),y=c("Vegas","Vegas"),group=c(1,2))
  return(bar)
}

team_col <-function(Team,Player){
  if(Player!="Player to Watch"){
  color<-data.frame(ncaahoopR::ncaa_colors)
  team_col<-color %>% filter(espn_name==Team)
  return(team_col)
  }
}

off_data<-function(Team,Player){
  if(Player!="Player to Watch"){
  seasonscore <- season_boxscore(Team,"2022-23",'raw')
  schedule <- get_schedule(Team)
  off<-seasonscore %>% select ('FGM','FGA','3PTM','3PTA','FTM','FTA') %>% summarize_all(sum)
  score <- schedule %>% select ('team_score','opp_score') %>% summarize_all(mean,na.rm=TRUE)
  off_cleanD <- data.frame(type=c('FG(%)','3PT(%)','FT(%)','Points','Opp Points'),value=c(100*(off$FGM/off$FGA),100*(off$'3PTM'/off$'3PTA'),100*(off$FTM/off$FTA),score$team_score,score$opp_score),order=c(1,2,3,4,5))
  return(off_cleanD)
  }
}

shotdata <- function(Team,Player){
  if(Player!="Player to Watch"){
    pbp<-get_pbp(Team,extra_parse = TRUE)
    shotdata<-pbp %>% select('game_id','shot_x','shot_y','shot_team','shot_outcome','shooter','three_pt') %>% drop_na(shot_y)%>%filter(shot_y<1000)
    shotdata<-shotdata %>% mutate(adjust_x=(50-shot_x),adjust_y=(100-shot_y))
    shotdata <- shotdata %>% mutate (x=case_when(shot_y>50~adjust_x,shot_y<50~shot_x),y=case_when(shot_y>50~adjust_y,shot_y<50~shot_y))
    shotdata <- shotdata %>% mutate (star_x=case_when(shooter==Player~x),star_y=case_when(shooter==Player~y))
    return(shotdata)
  }
}

star_data <- function(Team,Player){
  if(Player !="Player to Watch"){
  print("runstar")
  roster<-get_roster(Team)
  star<-roster %>% filter(name==Player)
  return(star)
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("March Madness Combination (Select by Yourself)"),
    h4("Be Patient! The Loading is around 2~3 Minutes"),
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(4,
            selectInput("Round","Round",choices=c("First Round",round_list)),
            selectInput("HT","Home",choices=c("",team_list)),
            selectInput("AT","Away",choices=c("",team_list)),
            selectInput("HTSeed","HTseed",choice=c(1,seed_list)),
            selectInput("ATSeed","ATseed",choice=c(2,seed_list)),
            uiOutput("htplayer"),
            uiOutput("atplayer"),
            #textOutput("note")
            #numericInput("Odd", "Odds", 5,min=1, max=10, 0.1)
        ),

        # Show a plot of the generated distribution
        column(8,
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #output$note <- renderText({"Be Patient! The Loading is around 1~2 Minutes"})
  
  output$htplayer <- renderUI({
    team<-input$HT  
    htposi<-which(team_list==team)
    sp<-player_list[htposi]
    mvp <- c("Player to Watch",sp)
    selectizeInput("htplayer","Home Team Player",choices = mvp)
  })
  output$atplayer <- renderUI({
    atteam<-input$AT  
    atposi<-which(team_list==atteam)
    atsp<-player_list[atposi]
    atmvp <- c("Player to Watch",atsp)
    selectizeInput("atplayer","Away Team Player",choices = atmvp)
  })
  
    htteam_color <- reactive({team_col(input$HT,input$htplayer)})
    atteam_color <- reactive({team_col(input$AT,input$atplayer)})
    htoff_data <- reactive({off_data(input$HT,input$htplayer)})
    atoff_data <- reactive({off_data(input$AT,input$atplayer)})
    htstarimage <- reactive({star_data(input$HT,input$htplayer)})
    atstarimage <- reactive({star_data(input$AT,input$atplayer)})
    htshotdata <- reactive({shotdata(input$HT,input$htplayer)})
    atshotdata <- reactive({shotdata(input$AT,input$atplayer)})
    #vegasodd <- reactive({vegas_odd(input$Odd)})
    
     
    output$distPlot <- renderPlot({
        htdata<-htshotdata()
        atdata<-atshotdata()

        # Information
        rd<-grobTree(rectGrob(gp=gpar(fill="lightblue",col="lightblue")),textGrob(input$Round, gp=gpar(col="Black",fontface="bold.italic",fontfamily="Arial",fontsize=32)))
        ht<-textGrob(input$HT,gp=gpar(col="Black",fontface="bold",fontfamily="Arial",fontsize=32))
        at<-textGrob(input$AT,gp=gpar(col="Black",fontface="bold",fontfamily="Arial",fontsize=32))
        seed1<-grobTree(rectGrob(gp=gpar(fill="#FFA500",col="#FFA500")),textGrob(input$HTSeed,gp=gpar(col="Black",fontface="italic",fontfamily="Arial",fontsize=24)))
        seed2<-grobTree(rectGrob(gp=gpar(fill="#FF8C00",col="#FF8C00")),textGrob(input$ATSeed,gp=gpar(col="Black",fontface="italic",fontfamily="Arial",fontsize=24)))
        seed<-grid.arrange(seed1,seed2,ncol=2)

        #Logo (Image)
        htlogo<-ggplot(htteam_color(),aes(x=0,y=0,image=logo_url))+geom_image(size = 0.5, by="height")+theme_void()
        atlogo<-ggplot(atteam_color(),aes(x=0,y=0,image=logo_url))+geom_image(size = 0.5, by="height")+theme_void()

        #Off chart
        htoffchart <- ggplot(htoff_data(),aes(y=reorder(type,-order),x=value))+geom_col(width=0.5,fill="navajowhite2")+geom_text(aes(label = round(value,1)),  color = "black",size=4,hjust=1)+theme_void()+theme(axis.text.y = element_text(size=8))
        atoffchart <- ggplot(atoff_data(),aes(y=reorder(type,-order),x=value))+geom_col(width=0.5,fill="navajowhite2")+geom_text(aes(label = round(value,1)),  color = "black",size=4,hjust=1)+theme_void()+theme(axis.text.y = element_text(size=8))

        #Shot Chart
        htshotchart <- ggplot(htdata,aes(x=x,y=y))+geom_point(alpha=0.2,color="red")+theme_void()+geom_path(data=court,aes(x=x,y=y,group=group),color="black")+xlim(0,50)+ylim(0,47)
        atshotchart <- ggplot(atdata,aes(x=x,y=y))+geom_point(alpha=0.2,color="red")+theme_void()+geom_path(data=court,aes(x=x,y=y,group=group),color="black")+xlim(0,50)+ylim(0,47)

        #Radar Chart (Image)
        #radar <- textGrob("radar")

        #Team Combine
        teamlayout <-rbind(c(1,1,1,2,2,2,2),c(1,1,1,2,2,2,2))
        htcombine <- grid.arrange(htshotchart,htoffchart,layout_matrix=teamlayout)
        atcombine <-grid.arrange(atshotchart,atoffchart,layout_matrix=teamlayout)

        #Star Player (Image + shot data)
        #star <-textGrob("star")
        htstar<-ggplot(htstarimage(),aes(x=0,y=0,image=player_image))+geom_image(size = 1, by="height")+theme_void()
        atstar<-ggplot(atstarimage(),aes(x=0,y=0,image=player_image))+geom_image(size = 1, by="height")+theme_void()
        htstarshot <- ggplot(htdata,aes(x=star_x,y=star_y))+geom_point(alpha=0.1,color="blue")+theme_void()+geom_path(data=court,aes(x=x,y=y,group=group),color="black")+xlim(0,50)+ylim(0,47)
        atstarshot <- ggplot(atdata,aes(x=star_x,y=star_y))+geom_point(alpha=0.1,color="blue")+theme_void()+geom_path(data=court,aes(x=x,y=y,group=group),color="black")+xlim(0,50)+ylim(0,47)

        # Star Combine
        laystar <- rbind(c(NA,2,2),c(1,2,2),c(1,2,2))
        htstarcombine <-grid.arrange(htstar,htstarshot,layout_matrix=laystar)
        atstarcombine <-grid.arrange(atstar,atstarshot,layout_matrix=laystar)


        #Vegas Odd (Win Probability)
        #vegas<-ggplot(vegasodd(),aes(x=x,y=y ,fill=group))+geom_bar(stat = "identity")+theme_void()+theme(legend.position = "none")
        #Vegas_text <-textGrob("Win%:",gp=gpar(col="Black",fontfamily="Arial",fontsize=18))

        #Layout
        lay <- rbind(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                     c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                     c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                     c(2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,5,5,5,5,5,5,5,5,5,5,6,6,6),
                     c(2,2,2,3,3,3,3,3,3,3,3,3,3,NA,NA,5,5,5,5,5,5,5,5,5,5,6,6,6),
                     c(2,2,2,3,3,3,3,3,3,3,3,3,3,NA,NA,5,5,5,5,5,5,5,5,5,5,6,6,6),
                     c(7,7,7,7,7,7,7,7,7,7,7,7,7,NA,NA,8,8,8,8,8,8,8,8,8,8,8,8,8),
                     c(7,7,7,7,7,7,7,7,7,7,7,7,7,NA,NA,8,8,8,8,8,8,8,8,8,8,8,8,8),
                     c(7,7,7,7,7,7,7,7,7,7,7,7,7,NA,NA,8,8,8,8,8,8,8,8,8,8,8,8,8),
                     c(7,7,7,7,7,7,7,7,7,7,7,7,7,NA,NA,8,8,8,8,8,8,8,8,8,8,8,8,8),
                     c(7,7,7,7,7,7,7,7,7,7,7,7,7,NA,NA,8,8,8,8,8,8,8,8,8,8,8,8,8),
                     c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
                     c(9,9,9,9,9,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,10,10,10,10,10,10,NA,NA,NA,NA,NA,NA,NA),
                     c(NA,NA,11,11,11,11,11,11,11,11,11,11,11,NA,NA,NA,NA,12,12,12,12,12,12,12,12,12,12,12),
                     c(NA,NA,11,11,11,11,11,11,11,11,11,11,11,NA,NA,NA,NA,12,12,12,12,12,12,12,12,12,12,12),
                     c(NA,NA,11,11,11,11,11,11,11,11,11,11,11,NA,NA,NA,NA,12,12,12,12,12,12,12,12,12,12,12),
                     c(NA,NA,11,11,11,11,11,11,11,11,11,11,11,NA,NA,NA,NA,12,12,12,12,12,12,12,12,12,12,12),
                     c(NA,NA,11,11,11,11,11,11,11,11,11,11,11,NA,NA,NA,NA,12,12,12,12,12,12,12,12,12,12,12)
                       )

        #test <- text_grob("test")
        startext <- textGrob("Player To Watch:",gp=gpar(col="Black",fontfamily="Arial",fontsize=18))
        #chart <- text_grob("chart")

        grid.arrange(rd,htlogo,ht,seed,at,atlogo,htcombine,atcombine,startext,startext,htstarcombine,atstarcombine,layout_matrix=lay)
        # #grid.arrange(rd,htlogo,ht,seed,at,atlogo,atcombine,chart,startext,startext,atstarcombine,chart,Vegas_text,vegas,layout_matrix=lay)
        #grid.arrange(htstar,htstarshot)
      })
}
#c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
#c(NA,NA,NA,NA,NA,NA,NA,13,13,NA,14,14,14,14,14,14,14,14,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
# Run the application 
shinyApp(ui = ui, server = server)
