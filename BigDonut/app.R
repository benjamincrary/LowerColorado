#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Week:",
                     min = 1,
                     max = 50,
                     value = 50)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  reservoir <- c("Havasu", "Mead", "Mohave", "Powell")
  full <- c(95,50,90,30)
  lab <- c("95%", "50%", "90%", "30%")
  lakesize <- c(100, 1000, 100, 700)
  ymin <- c(0,0,0,0)
  xmin <- c(70, 70, 70, 70)
  xmax <- c(100, 100, 100, 100)
  
  dat <- data.frame(reservoir, lab, full, lakesize, ymin, xmax, frame)

  
  donut <- ggplot(dat, aes(fill=full, ymax=full, ymin=ymin, xmax=xmax, xmin=xmin)) + 
    geom_rect() +
    scale_fill_gradient2(low="#FF8c00", midpoint=50, mid="#FFfa00", high="#00ff83", limits=c(0,100), guide=FALSE) +
    coord_polar(theta="y") +
    xlim(c(0,100)) + 
    ylim(c(0,100)) + 
    theme_minimal() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text=element_blank(),
          strip.text.x=element_text(size=10, colour="grey60", family="Calibri")) +
    annotate("text", x=0,y=0, label=lab, fontface=2, color="grey20", family="Calibri", size=14) +
    xlab("") + 
    ylab("") +
    ggtitle("Percent Full") +
    facet_grid(~reservoir, switch="both")
  donut


  output$distPlot <- renderPlot({
      donut
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

