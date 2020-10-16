# this is a shiny web app. Save as app.r

library(shiny)
library(leaflet)
library(dplyr)

# Define UI for application that draws a map
data<- readRDS("f.rds") # loading the data. It has the timestamp, lon, lat, and the accuracy (size of circles)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mapAct", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
  sliderInput("animation", "Time:",
              min = as.POSIXct("2017-02-15 00:00:00",tz = "Europe/Budapest"),
              max = as.POSIXct("2017-02-15 23:59:59",tz = "Europe/Budapest"),
              value = as.POSIXct("2017-02-15 00:00:00",tz = "Europe/Budapest"),
              timezone = "+0200",
              animate =
                animationOptions(interval = 600, loop = TRUE))
  )
                
  )


# Define server logic required
server <- function(input, output) {
  #stuff in server
  filteredData <- reactive({
    #add rollified thing
    from<- input$animation-90
    till<- input$animation+90
    data %>% filter(time >= from & time <=  till)
  })
  
  output$mapAct<-renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      fitBounds(lng1 = 5,lat1 = 52,lng2 = 5.2,lat2 = 52.2)# set to reactive minimums
  })
  
  observe({
    leafletProxy("mapAct", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(lng = ~lon, lat = ~lat,
                 radius = ~accuracy, fillOpacity = 0.02,color = "#DF2935")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
