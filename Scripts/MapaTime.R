# this is a shiny web app. Save as app.r
#lastupdate: acvillami

library(shiny)
library(leaflet)
library(dplyr)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(ggiraph)
library(DT)
library(shinyWidgets)

# Define UI for application that draws a map

min_time <- as.POSIXct("2020-01-01 05:00:00")

secuencia <- seq(from = min_time, length.out = 100, by = "mins")
latitudes <- seq(4.64, 4.66, l = 100)
longitudes <- seq(-74.1, -74.062, l = 100)
tomados <- rbinom(100,1,0.3)



data<- data.frame(time = c(secuencia),
                  lat = latitudes,
                  lon = longitudes,
                  accuracy = 10,
                  visto = tomados)

ui <- shinyUI(
  dashboardPage(dashboardHeader(title = span("Proyecto BA (Enciso-Merchan) - Universidad de los Andes",
                                             style = "color: white; font-size: 14px"),
                                titleWidth = 400),
                dashboardSidebar(
                  tags$head(tags$style(HTML('
                               /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #2D5FCC;
                                }
                              '))))
                dashboardBody(
                  tabsetPanel(type = 'tabs',
                              tabPanel('Análisis Departamentos',
                                       column(
                                         width = 9,
                                         sliderInput("animation", "Time:",
                                                     min = as.POSIXct("2020-01-01 05:00:00"),
                                                     max = as.POSIXct("2020-01-01 06:00:00"),
                                                     value = as.POSIXct("2020-01-01 05:00:00"),
                                                     animate =
                                                       animationOptions(interval = 600, loop = TRUE))
                                       ), 
                                       column(
                                         width = 9,
                                         leafletOutput("mapAct")
                                       ),
                                       
                                       column(
                                         width = 9,
                                         style='padding-top:100px;padding-bottom:70px; padding-left:0px',
                                         DT::dataTableOutput(outputId = 'depto_data', width = "100%")
                                       )
                              )
                  )
                  
                )
  )
)









# Define server logic required
server <- function(input, output) {
  #stuff in server
  filteredData <- reactive({
    #add rollified thing
    from<- input$animation - 50
    till<- input$animation + 50
    data %>% filter(time <= till) %>% 
      arrange(desc(time))
  })
  
  
  output$depto_data <- DT::renderDataTable({
    req(filteredData())
    Lista <- filteredData()
    
    selTable <- Lista
    
    DT::datatable(data = selTable,
                  escape=FALSE, 
                  options = list(sDom  = '<"top">lrt<"bottom">ip', 
                                 lengthChange = FALSE,
                                 dom = 'l',
                                 searchable = FALSE))
  })
  
  
  output$mapAct<-renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      fitBounds(lng1 = -74.05393,lat1 = 4.65386,
                lng2 = -71.1080,lat2 = 4.7082)# set to reactive minimums
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


# tt <- as.POSIXct("2020-01-01 05:00:00")
# 
# 
# from<- tt- 50
# 
# data %>% filter(time >= tt) %>% 
#   arrange(desc(time))

