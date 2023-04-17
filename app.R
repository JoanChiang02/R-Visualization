#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(readr)
library(ggplot2)
library(dplyr)


diving <- read.csv("Diving2000_mod.csv")
diving_distinct <- diving %>% distinct(Round, Diver, DiveNo, .keep_all = TRUE)
diver_list <- unique(diving_distinct$Diver)
diving_distinct$Round[diving_distinct$Round == "Final" & diving_distinct$Rank == 1] <- "Gold"
diving_distinct <- diving_distinct %>% mutate(size=case_when(Round=="Gold"~ 10,.default =5))





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h1("2000 Sydney Olympic Diving Result", align = "center"),windowTitle = "Sydney Olympic Diving"),

    
    fluidRow(align = 'center',
    selectInput("Diver","Diver:",choices=c("",diver_list))),
    plotlyOutput("distPlot",height=700)
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
      
      diving_distinct$Round[diving_distinct$Diver == input$Diver ] <- "Diver"
      diving_distinct$Round <- factor(diving_distinct$Round, levels = c("Gold","Final", "Semi", "Prelim","Diver"))
      color_list <- c("gold","navyblue","slateblue3","steelblue1","red")#
      
      plot_ly(diving_distinct, x = ~Event, y = ~Difficulty, z = ~AvgScore,size=~size,marker=list(sizeref=0.08),hoverinfo = 'text',text=~paste("Diver:",Diver,"\nCountry:",Country,"\nRank:",Rank,"\nDiveNo:",DiveNo,"\nAvgScore:",round(AvgScore,2),"\nDifficulty:",Difficulty))%>%add_markers(color = ~Round,colors = color_list) %>% layout(title="2000 Sydney Olympic Diving Result")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
