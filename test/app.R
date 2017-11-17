#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(rmarkdown)
library(tidyverse)
library(ggplot2)
library(plotly)
library(leaflet)
library(rgdal)
library(rgeos)
library(sf)
library(raster)
library(rvest)
library(stringr)
library(lubridate)
library(gridExtra)
library(tabulizer)
library(data.table)
library(zoo)
cat("

    <style>
    .shiny-frame {
    height: 1000px;
    width: 775px;
    overflow-x: hidden;
    }
    </style>
    
    ")

###Call functions
rm(list=ls())
library(shiny)
monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

###initial read of uframe for ui
uframe <- read_csv("Users.csv", col_types=cols())
uframe$MoYear <- as.POSIXct(paste(uframe$Month, "-", 1, "-", uframe$Year, sep=""), format="%m-%d-%Y")


ui <- fluidPage(
  
  fluidRow(
    shiny::column(6, selectInput("scaleby","Scale Users by:",c("Diversion", "Consumptive Use"))
    )
  ),
  fluidRow(
    column(6, div(plotlyOutput("NetworkPlot", width="370px", height="500px"))
    ) 
  ),  
  fluidRow(shiny::column(12, align="center", sliderInput("slider", "", min = min(uframe$MoYear),max =max(uframe$MoYear),value=max(uframe$MoYear),timeFormat="%b %Y", animate = TRUE)
  )
  )
)


server <- shinyServer(function(input, output, session){
  
  #Query this year's use monthly use data   
  url <- "https://www.usbr.gov/lc/region/g4000/hourly/use.pdf"      
  
  ### Load pages 1-5. Direct coercion into data frame creates no concerns with these tables
  tab1_5 <- extract_tables(url,pages=c(1,2,3,4,5), method="data.frame") 
  
  ### Load page 6.  No tab delimiter between user and source on page 6, coerced table does not populate correctly. conditionally populate source, "X", based on last nchar of user name
  tab6 <- extract_tables(url, pages=6, method="data.frame")
  tab6[[1]]$X <- ""
  tab6[[1]]$X <- ifelse(substrRight(tab6[[1]]$WATER.USER, 9)=="Diversion","Diversion", tab6[[1]]$X)
  tab6[[1]]$X <- ifelse(substrRight(tab6[[1]]$WATER.USER, 16)=="Measured Returns", "Measured Returns", tab6[[1]]$X)
  tab6[[1]]$X <- ifelse(substrRight(tab6[[1]]$WATER.USER, 18)=="Unmeasured Returns", "Unmeasured Returns", tab6[[1]]$X)
  tab6[[1]]$X <- ifelse(substrRight(tab6[[1]]$WATER.USER, 15)=="Consumptive Use", "Consumptive Use", tab6[[1]]$X)
  
  ### Load pages 7-8. Direct coercion into data frame creates no concerns with these tables 
  tab7_8 <- extract_tables(url, pages=c(7,8), method="data.frame")
  
  
  ### Load page 9. Coercion for this page is fine if isolated from other tables/pages using extract_tables function
  tab9 <- extract_tables(url, pages=9, method="data.frame")
  tab9 <- list(tab9[[1]]) ## removes extraneous second table on this page
  
  ### merge tables into single dataframe
  tab <- rbindlist(c(tab1_5, tab6, tab7_8, tab9))
  
  
  #####4. Do a little tidying
  
  tab2 <- tab %>% 
    mutate(temp=lag(tab$X), temp2=lag(WATER.USER)) %>% 
    mutate(User=na.locf(ifelse(temp=="",temp2,NA), na.rm=FALSE)) %>% 
    select(-c(temp, temp2)) %>%
    filter(X!="") %>%
    gather("month", "value", 3:14) %>%
    mutate(parameter=X, year=2017) %>%
    select(-c(TOTAL, WATER.USER,X)) %>%
    filter(parameter %in% c("Diversion", "Consumptive Use"))
  
  
  keeplist <- c("Bard Unit", 
                "Bullhead City",
                "Central Arizona Project",
                "Chemehuevi Indian Tribe",
                "Cibola Valley I.D.D.", 
                "City of Yuma",
                "Coachella Valley Water District", 
                "Cocopah Indian Reservation",
                "Colorado River Indian Reservation",
                "Fort Mojave Indian Reservation",
                "Imperial Irrigation District", 
                "Lake Havasu City",
                "Metropolitan Water District of Southern California", 
                "Mohave Valley I.D.D.",
                "North Gila Valley Irrigation District", 
                "Palo Verdo Irrigation District", 
                "Robert B. Griffith Water Project", 
                "Unit B Irrigation and Drainage District",
                "Wellton-Mohawk I.D.D.",
                "Yuma County Water Users' Association",
                "Yuma Irrigation District", 
                "Yuma Mesa I.D.D."
  )
  tab3 <- tab2 %>%
    filter(User %in% keeplist) %>%
    mutate(User= str_replace_all(User, c("Yuma County Water Users' Association"="Yuma County WUA", "Unit B Irrigation and Drainage District" = "Unit 'B' I.D.D.", "Chemehuevi Indian Tribe"="Chemehuevi Indian Reservation"))) %>%
    mutate(value = as.numeric(value))
  
  ######## maybe find/replace with alias name here?
  
  ucache <- read_csv("Users_2012-2016.csv")
  #ucache <- read_csv("C:/Users/bcrary/Desktop/Projects/CRB/TestViz/Rmarkdown/Users_2012-2016.csv")
  ucache2 <-  ucache %>% 
    gather("month", "value", 4:15) %>%
    filter(parameter %in% c("Diversion", "Consumptive Use")) %>%
    group_by(User, parameter, year, month) %>%
    summarise(value=sum(value))
  
  m2 <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  
  uframe <- union(tab3, ucache2) %>%
    mutate(month=match(month, m2)) %>%
    mutate(User= str_replace_all(User, c("Metropolitan Water District of Southern California" = "Met. Water District of So. Cal."))) %>%     
    group_by(parameter) %>% 
    mutate("NormValue" = value/max(value, na.rm=TRUE))
  
  
  #uframe <- read_csv("Users.csv") %>% gather("factor", "value", 11:14)
  #uframe <- read_csv("M:/Proposals/Proposals_2017/17111_CRB_Dataviz/App/NetworkXY/Users2.csv") 
  #uframe <- read_csv("Users2.csv") %>% gather("factor", "value", 12:13) %>% group_by(factor) %>% mutate("NormValue" = value/max(value))
  nframe <- read_csv("Nodes.csv") %>% mutate("NormLiveStorage" = LiveStorage/max(LiveStorage))
  riv <- read_csv("River.csv")
  cons <- read_csv("connections.csv") 
  #cons <- read_csv("C:/Users/bcrary/Desktop/Projects/CRB/TestViz/Rmarkdown/connections.csv")
  uframe$MoYear <- as.POSIXct(paste(uframe$month, "-", 1, "-", uframe$year, sep=""), format="%m-%d-%Y")
  nframe$MoYear <- as.POSIXct(paste(nframe$Month, "-", 1, "-", nframe$Year, sep=""), format="%m-%d-%Y")
  
  
  ##Link inputs to field names
  
  inputscale <- c("Diversion", "Consumptive Use")
  field <- c("Diversion", "Consumptive Use")
  scaletab <- data.frame(inputscale, field)
  
  
  ##Set the slider result to first of month
  sliderMonth <- reactiveValues()
  observe({
    full.date <- as.POSIXct(input$slider, tz="GMT")
    sliderMonth$Month <- as.character(monthStart(full.date))
  })
  output$SliderText <- renderText({sliderMonth$Month})
  
  
  ## Create reactive data frames
  puframe <- reactive({
    df <- uframe %>% 
      filter(MoYear == sliderMonth$Month & parameter==scaletab[inputscale==input$scaleby,2])
  })
  
  pnframe <- reactive({
    df <- nframe %>% 
      filter(MoYear == sliderMonth$Month) 
  })
  
  #text=sprintf("Reservoir: %s", Reservoir, "Storage: %s"))) +
  
  output$NetworkPlot <- renderPlotly({
    
    gg <- ggplot(puframe(), aes(dx, dy)) + 
      geom_point(data=puframe(), aes(size=NormValue, text=paste0("User: ", User, "<br>",input$scaleby," (kaf): ",round(value,0)), ids=User), colour="slategray", alpha=0.5) #+
    #geom_path(data=riv, aes(Longitude, Latitude), colour="steelblue", alpha=0.8, size=1) +
    #geom_segment(data=cons, aes(x=dx, y=dy, xend=Diversionx, yend=Diversiony, linetype=Type), color="steelblue", size=0.5, alpha=0.2) +
    #geom_point(data=pnframe(), aes(dx,dy, size=NormLiveStorage, colour=LiveStoragePerc, text=paste0("Reservoir: ", Reservoir, "<br>Storage (kaf): ", round(LiveStorage,0), "<br>Percent Full: ", round(LiveStoragePerc,0)))) +
    #scale_colour_gradient2(low="#FF8c00", midpoint=50, mid="#FFfa00", high="#00ff83",guide=FALSE) +
    #scale_size(limits=c(0,1)) +
    #coord_equal() +
    #xlab("") + 
    #ylab("") + 
    #xlim(520000,2100000) +
    #theme_minimal() +
    #theme(panel.background=element_rect(colour=NULL, fill="white"), 
    #      panel.grid.major=element_blank(), 
    #      panel.grid.minor=element_blank(), 
    #      axis.text=element_blank(),
    #      legend.position="none")
    
    gg <- ggplotly(gg, tooltip=c("text")) %>% config(displayModeBar = F) 
    
  })
}) 
  
# Run the application 
shinyApp(ui = ui, server = server) 

