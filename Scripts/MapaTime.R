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
library(SiMRiv)
library(spatstat)
library(tibble)
library(dplyr)
library(lubridate)
#library(data.table)
# Define UI for application that draws a map
normalize <- function(x)
{
  a=max(abs(max(x)),abs(min(x)))
  return((x) /(a))
}
minmax <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}
min_time <- as.POSIXct("2020-01-01 05:00:00")

secuencia <- seq(from = min_time, length.out = 550, by = "mins")
set.seed(9)
levy.walker <- species(state.RW() + state.CRW(0.99), trans = transitionMatrix(0.01, 0.01)) 
sim.lw <- simulate(levy.walker, rpois(1,600))
# plot(sim.lw[450:550,], type = "l", asp = 1, main = "L ́evy-like walker")
# sim.lw[,1]=normalize(sim.lw[,1])
# sim.lw[,2]=normalize(sim.lw[,2])
# plot(sim.lw[], type = "l", asp = 1, main = "L ́evy-like walker")
juana=data.frame(minmax(sim.lw[1:550,1]),minmax(sim.lw[1:550,2]),"Juana",secuencia)
trabajo=data.frame(juana[which.max(sim.lw[,1])-10,1],juana[which.max(sim.lw[,1])-10,2],"trabajo",secuencia[295:400])
casa= data.frame(juana[1,1],juana[1,2],"casa",secuencia[1:40])
restaurante=data.frame(juana[500,1],juana[500,2],"restaurante",secuencia[1:40])
set.seed(14)
n=20
pp <- rpoispp(n)
plot(density(pp))
establecimientos=data.frame(pp$x[0:17],pp$y[0:17],"establecimientos",rep(secuencia[400:470],each=17))
colnames(juana)=c("x","y","label","time")
colnames(trabajo)=c("x","y","label","time")
colnames(casa)=c("x","y","label","time")
colnames(restaurante)=c("x","y","label","time")
colnames(establecimientos)=c("x","y","label","time")
X1= -74.1080
Y1= 4.7082
X2= -74.05393
Y2= 4.65386

df=rbind(juana,trabajo,casa,restaurante,establecimientos) %>% 
  mutate(x=(x*(X2-X1))+X1,
         y=(y*(Y1-Y2))+Y2)
data<- data.frame(time = df$time,
                  lat = df$y,
                  lon = df$x,
                  accuracy = 10,
                  visto = df$label)
anglerIcon <- makeIcon(
  iconUrl = "https://i.imgur.com/otS5q0K.png",
  iconWidth = 64, iconHeight = 64,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

leaflet(data = quakes[1:4,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = anglerIcon)
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
                              ')))),
                dashboardBody(
                  tabsetPanel(type = 'tabs',
                              tabPanel('An?lisis Departamentos',
                                       column(
                                         width = 9,
                                         sliderInput("animation", "Time:",
                                                     min = min(data$time),
                                                     max = max(data$time),
                                                     value = min(data$time),
                                                     animate =
                                                       animationOptions(interval = 6000, loop = TRUE))
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
      fitBounds(lng1 = -74.05393,lat1 = 4.655,
                lng2 = -74.09,lat2 = 4.66)# set to reactive minimums
  })
  
  observe({
    leafletProxy("mapAct", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(lng = ~lon, lat = ~lat,
                 radius = ~accuracy, fillOpacity = 0.02,color = "#DF2935") %>%
      addMarkers(lng =~lon,lat = ~lat, icon = anglerIcon)
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

