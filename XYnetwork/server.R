#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$NetPlot <- renderPlotly({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # generate data frame
    node <- c("Powell", "Meade","SoNev","Tribe1","Tribe2","SoCal","Tribe3","Tribe4","Havasu", "HavasuCity","Blythe", "Tribe5","Yuma","YumaCity", "YumaCounty")
    dx <- c(1,1,2,2,2,2,2,2,1,2,2,2,1,2,2)
    dy <- c(12, 9, 12.25, 12, 11.75, 9.25, 9, 8.75, 6, 6.25, 6, 5.75, 3, 3.25, 2.75)
    size1 <- c(3, 3, 4,3,2, 4,3,2, 3, 1, 2,1, 3, 1, 3)
    size2 <- c(3, 3, 2,3,1, 4,3,2, 3, 2, 3,2, 3, 1, 2)
    size3 <- c(3, 3, 3,1,1, 4,3,2, 3, 3, 4,1, 3, 2, 2)
    frame <- data.frame(node, dx, dy, size1, size2, size3)
    
    
    
    
    # draw the histogram with the specified number of bins
    p <- plot_ly(data=frame, x=~dx, y=~dy, size=~size1, color=~node)
    
  })
  
})
