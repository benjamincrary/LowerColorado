---
output: html_document
runtime: shiny
---
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library()
library(plotly)
library(tidyverse) 

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Primary Water Users, Lower Colorado River Basin"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("scaleby",
                     "Scale nodes by:",
                    c("End of Month Use", "End of Year Forecast", "Approved Annual Diversion", "Forecasted Annual Excess"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("NetworkPlot", width="400px", height="800px")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  uframe <- read_csv("C:/Users/bcrary/Desktop/Projects/CRB/TestViz/NetworkXY/Users.csv")
  nframe <- read_csv("C:/Users/bcrary/Desktop/Projects/CRB/TestViz/NetworkXY/Nodes.csv")
  riv <- read_csv("C:/Users/bcrary/Desktop/Projects/CRB/TestViz/NetworkXY/River.csv")
  cons <- read_csv("C:/Users/bcrary/Desktop/Projects/CRB/TestViz/NetworkXY/connections.csv")
  uframe$MoYear <- paste(uframe$Month, " - ", uframe$Year, sep="") 
  nframe$MoYear <- paste(nframe$Month, " - ", nframe$Year, sep="")
  
  #uframe$scale <- reactive({ uframe$scale <- if(input$scaleby == "End of Month Use") {
  #  uframe$endOfMonUse
  #} else if(input$scaleby == "End of Year Forecast") {
  #  uframe$eoyForecast 
  #} else if (input$scaleby == "Approved Annual Diversion") {
  #  uframe$approvedAnnualVol
  #} else if (input$scaleby == "Forecasted Annual Excess") {
  #  uframe$forecastedExcess
  #}
  #})

  options <- c("End of Month Use", "End of Year Forecast", "Approved Annual Diversion", "Forecasted Annual Excess")
  vars <- c("endofMonUse", "eoyForecast", "approvedAnnualVol", "forecastedExcess")
  lookup <- data.frame(options, vars)
  scaled <- reactive({ lookup[options==input$scaleby,2] })
  
  output$NetworkPlot <- renderPlotly({

    nframe %>% plot_ly(x=~dx, y=~dy, size=~LiveStorage,text=~Reservoir, hoverinfo="text") %>%
      
    
    
   })
} 


#geom_segment(data=uframe, aes(x=dx, y=dy, xend=Diversionx, yend=Diversiony, group=User), colour="grey85", alpha=0.1, size=0.2)+
#
# Run the application 
shinyApp(ui = ui, server = server)

