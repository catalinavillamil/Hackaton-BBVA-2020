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
#library(ggiraph)
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
casa= data.frame(juana[1,1],juana[1,2],"Home",secuencia[1:40])
restaurante=data.frame(juana[500,1],juana[500,2],"restaurante",secuencia[460:550])
restaurante2=data.frame(juana[500,1],juana[500,2],"establecimientos",secuencia[400:460])
set.seed(648)
n=20
pp <- rpoispp(n)
plot(pp)
establecimientos=data.frame(pp$x[0:17],pp$y[0:17],"establecimientos",rep(secuencia[400:460],each=17))
colnames(juana)=c("x","y","label","time")
colnames(trabajo)=c("x","y","label","time")
colnames(casa)=c("x","y","label","time")
colnames(restaurante)=c("x","y","label","time")
colnames(restaurante2)=c("x","y","label","time")
colnames(establecimientos)=c("x","y","label","time")
X1= -74.1080
Y1= 4.7082
X2= -74.05393
Y2= 4.65386

df=rbind(juana,trabajo,casa,restaurante,restaurante2,establecimientos) %>% 
  mutate(x=(x*(X2-X1))+X1,
         y=(y*(Y1-Y2))+Y2)
data<- data.frame(time = df$time,
                  lat = df$y,
                  lon = df$x,
                  accuracy = 10,
                  visto = df$label)
HomeIcon <- makeIcon(
  iconUrl = "https://image.flaticon.com/icons/png/512/69/69524.png",
  iconWidth = 40, iconHeight = 40,
  iconAnchorX = 0, iconAnchorY = 0,

)

RestaurantIcon <- makeIcon(
  iconUrl = "https://cdn.onlinewebfonts.com/svg/img_224457.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY = 0,

)

JuanaIcon <- makeIcon(
  iconUrl = "https://cdn.shopify.com/s/files/1/1061/1924/products/Dancer_With_Red_Dress_Emoji_large.png?v=1571606063",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 0, iconAnchorY = 0,

)

SuperIcon <- makeIcon(
  iconUrl = "https://cdn.onlinewebfonts.com/svg/img_224457.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 0, iconAnchorY = 0,

)

WorkIcon <- makeIcon(
  iconUrl = "https://static.thenounproject.com/png/990246-200.png",
  iconWidth = 40, iconHeight = 40,
  iconAnchorX = 0, iconAnchorY = 0,

)

CarIcon <- makeIcon(
  iconUrl = "https://cdn1.iconfinder.com/data/icons/ios-11-glyphs/30/car-512.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 0, iconAnchorY = 0,

)
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
                                                       animationOptions(interval = 200, loop = TRUE))
                                       ), 
                                       column(
                                         width = 12,
                                         style='padding-top:100px;padding-bottom:100px; padding-left:0px',
                                         leafletOutput("mapAct")
                                       ),
                                       
                                       column(
                                         width = 12,
                                         style='padding-top:100px;padding-bottom:70px; padding-left:0px',
                                         DT::dataTableOutput(outputId = 'depto_data', width = "150%")
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
    from<- input$animation 
    till<- input$animation
    data %>% filter(time >= from & time <=  till,visto =="Juana")
  })
  filteredData2 <- reactive({
    #add rollified thing
    from<- input$animation 
    till<- input$animation
    data %>% filter(time >= from & time <=  till, visto=="Home")
  })
  filteredData3 <- reactive({
    #add rollified thing
    from<- input$animation 
    till<- input$animation
    data %>% filter(time >= from & time <=  till,visto =="trabajo")
  })
  filteredData4 <- reactive({
    #add rollified thing
    from<- input$animation -50
    till<- input$animation +50
    data %>% filter(time >= from & time <=  till,visto =="restaurante")
  })
  filteredData5 <- reactive({
    #add rollified thing
    from<- input$animation -600
    till<- input$animation +600
    data %>% filter(time >= from & time <=  till,visto =="establecimientos")
  })
  filteredData6 <- reactive({
    #add rollified thing
    from<- input$animation -1000
    till<- input$animation
    data %>% filter(time >= from & time <=  till,visto =="Juana")
  })
  filteredData7 <- reactive({
    #add rollified thing
    from<- input$animation 
    till<- input$animation
    data %>% filter(time >= from & time <=  till,visto =="establecimientos")
  })
  filteredData8 <- reactive({
    #add rollified thing
    from<- input$animation 
    till<- input$animation
    data %>% filter(time >= from & time <=  till,visto =="restaurante")
  })
  output$mapAct<-renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron)%>%
      fitBounds(lng1 = -74.1080,lat1 = 4.65386,
                lng2 = -74.05393,lat2 = 4.7082)# set to reactive minimums
  })
  
  observe({
    req(filteredData())
    leafletProxy("mapAct", data = filteredData()) %>%
      clearMarkers() %>% 
      # addCircles(lng = ~lon, lat = ~lat,
      #            radius = ~accuracy, fillOpacity = 0.02,color = "#DF2935") %>%
      addMarkers(lng =~lon, lat = ~lat, icon = JuanaIcon)
  })
  
  observe({
    req(filteredData2())
    leafletProxy("mapAct", data = filteredData2()) %>%
      clearShapes() %>%
      # addCircles(lng = ~lon, lat = ~lat,
      #            radius = ~accuracy, fillOpacity = 0.02,color = "#DF2935") %>% 
      addMarkers(lng =~lon,lat = ~lat, icon = HomeIcon)
  })
  observe({
    req(filteredData3())
    leafletProxy("mapAct", data = filteredData3()) %>%

      # addCircles(lng = ~lon, lat = ~lat,
      #            radius = ~accuracy, fillOpacity = 0.02,color = "#DF2935") %>% 
      addMarkers(lng =~lon,lat = ~lat, icon = WorkIcon)
  })
  
  observe({
    req(filteredData4())
    leafletProxy("mapAct", data = filteredData4()) %>%

       # addCircles(lng = ~lon, lat = ~lat,
       #           radius = ~accuracy*100, fillOpacity = 0.02,color = "#2EFE64") %>% 
      addMarkers(lng =~lon,lat = ~lat, icon = RestaurantIcon)
  })
  
  observe({
    req(filteredData5())
    leafletProxy("mapAct", data = filteredData5()) %>%

       # addCircles(lng = ~lon, lat = ~lat,
       #            radius = ~accuracy*80, fillOpacity = 0.02,color = "#8258FA") %>% 
      addMarkers(lng =~lon,lat = ~lat, icon = SuperIcon)
  })
  observe({
    req(filteredData6())
    leafletProxy("mapAct", data = filteredData6()) %>%

       addCircles(lng = ~lon, lat = ~lat,
                  radius = ~accuracy, fillOpacity = 0.002,color = "#f11a3a") #%>%
      #addMarkers(lng =~lon,lat = ~lat, icon = JuanaIcon)
  })
  observe({
    req(filteredData7())
    leafletProxy("mapAct", data = filteredData7()) %>%
      addCircles(lng = ~lon, lat = ~lat,
                 radius = ~accuracy*30, fillOpacity = 0.2,color = "#fb842c") #%>%
    #addMarkers(lng =~lon,lat = ~lat, icon = JuanaIcon)
  })
  observe({
    req(filteredData8())
    leafletProxy("mapAct", data = filteredData8()) %>%
      addCircles(lng = ~lon, lat = ~lat,
                 radius = ~accuracy*40, fillOpacity = 0.2,color = "#40e138") #%>%
    #addMarkers(lng =~lon,lat = ~lat, icon = JuanaIcon)
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

# p <- 80
# from<- min(data$time) -50 + p*60
# till<- min(data$time) +50 + p*60
# 
# data %>% filter(time >= from & time <=  till)
