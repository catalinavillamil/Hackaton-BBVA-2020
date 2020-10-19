# this is a shiny web app. Save as app.r
#lastupdate: acvillami

#setwd('/home/ubuntu')

library(leaflet)
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
# library(ggpmisc)
library(shinyWidgets) 
library(shinyjs)
#library(ggimage)
library(ggthemes)
library(lubridate)
library(data.table)

# Define UI for application that draws a map

load('data_cata.Rda')
min_time <- as.POSIXct("2020-01-01 06:00:00")


secuencia <- seq(from=min_time, length.out = 10440, by = 30)
  
  
esta <- fread("Establecimientos.csv", header = T)[,1:6] %>% 
  mutate(seqq = ifelse(Producto == 'Restaurantes', 1,
                       ifelse(Producto == 'Licores', 2,
                              ifelse(Producto == 'Vestuario',3,
                                     ifelse(Producto == 'LI', 4,
                                            ifelse(Producto == 'Hipotecario',5,6)))))) %>% 
  arrange(seqq) %>% 
  mutate(Id_Esta = 1:106) %>% 
  select(-seqq, -Dias, - Horario, -Zona)

head(es)
head(esta)
base_final <- es %>% 
  left_join(esta,
            by = c('X1' = 'Id_Esta')) %>% 
  select(Id_Cliente = id,
         Hora = fecha,
         Establecimiento = Nombre,
         Producto,
         Id_Est = X1)



base_final_2 <- base_final %>% 
  arrange(Id_Cliente, Hora)

mensajes_enviados <- base_final_2 %>% 
  group_by(Id_Cliente) %>% 
  mutate(secuencia = 1:n()) %>% 
  ungroup() %>% 
  filter(secuencia == 1) %>% 
  rename(Hora_Envio = Hora,
         Producto_Enviado = Producto) %>% 
  select(Id_Cliente, Hora_Envio, Producto_Enviado)

d=data.frame(Hora=secuencia,ventana=ceiling(rep(1:length(secuencia)/30))) %>% 
  group_by(ventana) %>% 
  mutate(hora_cierra = max(Hora)) %>% 
  ungroup() %>% 
  select(-ventana)

base_final_3 <- base_final_2 %>% 
  left_join(mensajes_enviados) %>% 
  mutate(dif_horas = (Hora - Hora_Envio)/3600,
         dif_horas = as.numeric(dif_horas)) %>% 
  mutate(Mensaje_Enviado = ifelse(Hora == Hora_Envio, 1, 
                                  ifelse(Producto != Producto_Enviado & dif_horas >= 5, 1, 0))) %>% 
  group_by(Id_Cliente) %>% 
  mutate(sec_cliente = 1:n()) %>% 
  mutate(Mensaje_Enviado = ifelse(sec_cliente >= 3 , 0, Mensaje_Enviado)) %>% 
  select(-sec_cliente, -dif_horas, -Hora_Envio, -Producto_Enviado) %>% 
  left_join(d) %>% 
  mutate(Mensaje_abierto = rbinom(1,1,0.02))

summary_total <-  base_final_3 %>% 
  group_by(hora_cierra) %>% 
  summarise(n=n())

load('data_cata_est.Rda')

base_final_4 <- aux %>% 
  select(Id_Est = id, lon, lat)

base_final_3 <- base_final_3 %>% 
  left_join(base_final_4)


min_n <- min(summary_total$n)
max_n <- max(summary_total$n)
min_x <- min(summary_total$hora_cierra)
max_x <- max(summary_total$hora_cierra)
saltos <- seq(min_x, max_x, l = 20)


data_clientes <- fread('db_clientes_perfil.csv')
# names(data_clientes) <- gsub('_rta', '', names(data_clientes))

opciones_prod <- toupper(gsub('_',' ',gsub('pred','',names(data_clientes)[grepl('pred', names(data_clientes))])))
opciones_tc <- c(1,0)
min_ingresos <- min(data_clientes$ingresos)
max_ingresos <- max(data_clientes$ingresos)



categorias <- unique(esta$Categoria)
lugares <- unique(esta$Nombre)

imagen_p <- tags$a(tags$img(src="p5.png", height='81', width='256'), 
                   style = "background-size:cover; background-position:center; position:absolute;right:2em")


ui <- shinyUI(
  dashboardPage(dashboardHeader(title = span("",
                                             style = "color: white; font-size: 14px")
                                ),
                dashboardSidebar(
                ),
                dashboardBody(
              tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #072146;
                       }
                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #072146;
                       }
                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #072146;
                       }
                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #072146;
                       }
                                  /* body */
                                .content-wrapper, .right-side {
                                background-color: #072146;
                                }
                                ".irs-bar {",
           "  border-color: transparent;",
           "  background-color: transparent;",
           "}",
           ".irs-bar-edge {",
           "  border-color: transparent;",
           "  background-color: transparent;",
           "}"
           .nav-tabs {
  background-color: #006747;
    }

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: transparent;
border-color: transparent;
}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #006747;
}
                       ')
                  ),
                 
                  tabsetPanel(type = 'tabs',
                              tabPanel('Resumen de oferta',
                                       fluidRow(column(3),
                                                column(4,
                                                       style='padding-top:20px;padding-bottom:50px; padding-left:0px',
                                                       
                                                       imagen_p
                                                ),
                                                column(5)),
                                       fluidRow(
                                         style='padding-top:70px;padding-bottom:10px; padding-left:0px',
                                         valueBoxOutput("resu1"),
                                         valueBoxOutput("resu2"),
                                         valueBoxOutput("resu3")
                                       ),
                                       fluidRow(
                                         column(width = 4),
                                         column(
                                           width = 4,
                                           setSliderColor(c("black"), c(1)),
                                           sliderInput("animation", 
                                                       h3(p("Hora: Dar play ",style="color:#FFFFFF")),
                                                       min = min(base_final$Hora),
                                                       max = max(base_final$Hora),
                                                       value = min(base_final$Hora),
                                                       step = 1800, 
                                                       animate =
                                                         animationOptions(interval = 1000, loop = TRUE))
                                         )
                                       ), 
                                       fluidRow(
                                       column(width = 1),
                                       column(
                                         width = 5, 
                                         style='padding-top:0px;padding-bottom:20px; padding-left:0px',
                                         plotOutput(outputId = 'tabla_summary', width = "80%")
                                       ),
                                   
                                       column(
                                         width = 5, 
                                         style='padding-top:0px;padding-bottom:20px; padding-left:0px',
                                         plotOutput(outputId = 'ordering', width = "80%")
                                       ),
                                       column(width = 1),
                                       ),
                                       fluidRow(
                                         column(width = 1),
                                         column(
                                           width = 5,
                                           style='padding-top:20px;padding-bottom:20px; padding-left:10px',
                                           leafletOutput("mapAct", width = "80%")
                                         ),
                                         column(
                                           width = 5,
                                           style='padding-top:20px;padding-bottom:20px; padding-left:10px',
                                           div(DT::dataTableOutput(outputId = 'tabla_inicial'),
                                               style = "font-size: 70%; width: 70%" )
                                         )
                                       )
                                      
                                   
                              ), 
                              tabPanel('Campañas',
                                       fluidRow(column(3),
                                                column(4,
                                                        style='padding-top:20px;padding-bottom:20px; padding-left:0px',
                                                        
                                                        imagen_p
                                       ),
                                       column(5)),
                                       fluidRow(
                                         column(4,
                                                style='padding-top:20px;padding-bottom:20px; padding-left:0px',
                                                tags$head(tags$style(HTML(' #Reg { color:#FFFFFF  ;}'  ))),
                                                selectInput("camp", 
                                                            h3(p("Seleccione la campaña",style="color:#FFFFFF")),
                                                            choices = c('GeoMensaje', 'Mensaje'),
                                                            multiple = F,
                                                            selected = NA)
                                         ),
                                         column(4),
                                         column(4)
                                        
                                        
                                       ),
                                       fluidRow(
                                         column(width = 3,
                                                style='padding-top:20px;padding-bottom:5px; padding-left:10px',
                                                tags$head(tags$style(HTML('
                                            #prod { color:#FFFFFF  ;padding-top:0px}'))),
                                                selectInput("prod", 
                                                             h3(p("Seleccione producto",style="color:#FFFFFF")),
                                                             choices = opciones_prod,
                                                             multiple = T,
                                                             selected = NA),
                                         ),
                                         column(width = 3,
                                                style='padding-top:20px;padding-bottom:5px; padding-left:10px',
                                                tags$head(tags$style(HTML('
                                            #prod { color:#FFFFFF  ;padding-top:0px}'))),
                                                selectInput("tc", 
                                                            h3(p("Tarjeta crédito",style="color:#FFFFFF")),
                                                            choices = opciones_tc,
                                                            selected = NA),
                                         ),
                                       column(width = 3,
                                              style='padding-top:10px;padding-bottom:5px; padding-left:10px',
                                              tags$head(tags$style(HTML('
                                            #prod { color:#FFFFFF  ;padding-top:0px}'))),
                                              sliderInput("ingresos",
                                                          h3(p("Ingresos mínimos",style="color:#FFFFFF")),
                                                          min = min_ingresos, max = max_ingresos, value = max_ingresos
                                              )
                                       ),
                                       column(width = 3,
                                              style='padding-top:10px;padding-bottom:5px; padding-left:10px',
                                              tags$head(tags$style(HTML('
                                            #prod { color:#FFFFFF  ;padding-top:0px}'))),
                                              sliderInput("proba",
                                                          h3(p("Prob min de compra",style="color:#FFFFFF")),
                                                          min = 0, max = 1, value = 0
                                              )
                                       )
                                       ),
                                       fluidRow(
                                         column(5,
                                                actionButton(inputId = "Clic", label = 'Genere los clientes'))
                                       ),
                                       fluidRow(
                                         column(3,
                                                style='padding-top:20px;padding-bottom:10px; padding-left:10px',
                                                valueBoxOutput("resu4", width = "50%")),
                                         column(3,
                                                style='padding-top:20px;padding-bottom:10px; padding-left:10px',
                                                valueBoxOutput("resu5", width = "50%")),
                                         column(3,
                                                style='padding-top:20px;padding-bottom:10px; padding-left:10px',
                                                valueBoxOutput("resu6", width = "50%")),
                                         column(3,
                                                style='padding-top:20px;padding-bottom:10px; padding-left:10px',
                                                valueBoxOutput("resu7", width = "50%"))
                                       ),
                                  
                                       fluidRow(
                                         column(width = 3,
                                                        dateInput("date",
                                                                  h3(p("Fecha de envío de mensaje",style="color:#FFFFFF")),
                                                                  value = "2020-10-18")
                                         ),
                                         column(6,
                                                tags$head(tags$style(HTML('
                                            #prod { color:#FFFFFF  ;padding-top:0px}'))),
                                          textAreaInput("caption",
                                                        h3(p("Escriba el mensaje a enviar",style="color:#FFFFFF")),
                                                        "", width = "710px")),
                                         column(3,
                                                style='padding-top:40px;padding-bottom:5px; padding-left:10px',
                                                actionButton(inputId = "pp", label = 'Envíe el mensaje'))
                                         
                                       ),
                                       fluidRow(
                                         column(3,
                                                style='padding-top:40px;padding-bottom:50px; padding-left:10px',
                                                tags$head(tags$style(HTML('
                                            #prod { color:#FFFFFF  ;padding-top:0px}'))),
                                                selectInput("cat", 
                                                            h3(p("Categoría del establecimiento",
                                                                 style="color:#FFFFFF")),
                                                            choices = categorias,
                                                            selected = NA,
                                                            multiple = F)
                                         ),
                                         column(3,
                                                style='padding-top:40px;padding-bottom:50px; padding-left:10px',
                                                tags$head(tags$style(HTML('
                                            #prod { color:#FFFFFF  ;padding-top:0px}'))),
                                                selectInput("lug", 
                                                            h3(p("Nombre del establecimiento",style="color:#FFFFFF")),
                                                            choices = lugares,
                                                            selected = NA,
                                                            multiple = T)
                                         )
                                         ),
                                       fluidRow(column(12)),
                                       fluidRow()
                                        
                                       )
                              )
                  ),
                  useShinyjs()
                  
                )
  )










# Define server logic required
server <- function(input, output, session) {
  #stuff in server
  
  reactivo <- eventReactive(input$Clic,
                            {
                              productos <- input$prod
                              tc <- input$tc
                              ing <- input$ingresos
                              prob <- input$proba
                              productos <- gsub(' $','',productos)
                              
                              variables_1 <- gsub(' ','_',paste(tolower(productos), 'pred', sep ='_'))
                              
                              gg <- data_clientes %>% 
                                select(variables_1)
                              
                              proba_f <- apply(gg, 1, max)
                              
                              base_1 <- data_clientes %>% 
                                mutate(P_final = proba_f) %>% 
                                filter(tc == tc) %>% 
                                filter(ingresos >= ing) %>% 
                                select(id, variables_1, edad, ingresos, P_final, sexo) %>% 
                                filter(P_final >= prob)
                              
                              return(list(base_cl = base_1))
                              
                            })
  
  
  outVar <- reactive({
    data <- esta %>% 
    filter(Categoria == input$cat)
  
  unique(data$Nombre)
    })
  
  observe({updateSelectInput(session, 'lug', choices = outVar())}) 
  

  
  
  
  filteredData <- reactive({
    #add rollified thing
    from<- input$animation - 50
    till<- input$animation + 50
    base_final_3 %>% filter(Hora <= till) %>% 
      arrange(desc(Hora)) %>% 
      mutate(Mensaje_abierto = ifelse(Mensaje_abierto == 1, 1,
                                      rbinom(1,1,0.001)))
  })
  
  
  output$tabla_inicial <- DT::renderDataTable({
    req(filteredData())
    Lista <- filteredData() %>% 
      select(-hora_cierra) %>% 
      arrange(desc(Hora))
    
    selTable <- Lista %>% 
      select(-Hora, -lon, -lat)
    
    DT::datatable(data = selTable,
                  escape=FALSE, 
                  options = list(sDom  = '<"top">lrt<"bottom">ip', 
                                 lengthChange = FALSE,
                                 dom = 'l',
                                 searchable = FALSE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('td').css({'border': '1px solid black'});",
                                   "$('th').css({'border': '1px solid black'});",
                                   "$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});",
                                   "}")))
  })
  
  
  output$tabla_summary <- renderPlot({
    req(filteredData())
    Lista <- filteredData() %>% 
      group_by(hora_cierra) %>% 
      summarise(n=n())
    
    selTable <- Lista[-nrow(Lista),]
    
    ggplot(selTable, aes(x=hora_cierra, y=n)) +
      geom_line(color = 'white') + 
      xlab("Hora") +
      ylab("Mensajes enviados") +
      ylim(min_n, max_n) + 
      xlim(min_x, max_x) +
      scale_x_datetime(date_labels = "%H:%M:%S",limits=c(min_x, max_x)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.background=element_rect(fill = "#072146"),
            panel.background = element_rect(fill = '#072146'),
            panel.border = element_rect(colour = "#072146", fill=NA, size=1),
            plot.title = element_text(hjust = 0.5, colour = 'white'),
            axis.text.x=element_text(colour="white"),
            axis.title.x = element_text(colour = 'white'),
            axis.text.y=element_text(colour="white"),
            axis.title.y = element_text(colour = 'white')) +
      theme_hc()+ 
      ggtitle("Numero de mensajes enviados cada 30 min")
    
    
  })
  
  output$ordering <- renderPlot({
    req(filteredData())
    
    fecha_filtro <- max(filteredData()$Hora) 
    
    Lista <- filteredData() %>% 
      # filter(Hora >= fecha_filtro) %>% 
      group_by(Producto) %>% 
      summarise(n=sum(Mensaje_Enviado))
    
    selTable <- Lista %>% 
      arrange(desc(n)) 
    
    selTable <- selTable[1:5,]
    
   colors_blue <- data.frame(Producto = unique(selTable$Producto),
            colores_blue = RColorBrewer::brewer.pal(length(unique(selTable$Producto)), 'Blues'))
    
  
   selTable <- selTable %>% 
     left_join(colors_blue)
   
   ggplot(data = selTable, aes(x = reorder(Producto,n),  y = n, fill = colores_blue, color = "black")) +
     geom_col() +
     scale_fill_manual(values = selTable$colores_blue)  +
     geom_text(aes(y = 0, label = Producto), size = 5, color="white", hjust = 0) +
     scale_colour_manual(values = selTable$colores_blue)+
     # geom_image(aes(x = Producto, image = Image), y = 0,  # add geom_image layer
     #            size = 0.2, hjust = 1,
     #            inherit.aes = FALSE) +
     coord_flip(clip = "off", expand = FALSE) +
     scale_y_continuous(labels = scales::comma) +
     guides(color = FALSE, fill = FALSE) +
     # theme_classic() +
     theme(plot.title = element_text(hjust = 0.5, colour = 'white'),
           axis.ticks.y = element_blank(),
           axis.text.y  = element_blank(),
           # plot.margin = margin(1, 1, 1, 4, "cm"),
           axis.title.y=element_blank(),
           plot.background=element_rect(fill = "#072146"),
           panel.background = element_rect(fill = '#072146'),
           panel.border = element_rect(colour = "#072146", fill=NA, size=1),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x=element_text(colour="white")) +
     ylab("") +
     xlab("") +
     theme_hc()+
     ggtitle("Top 5 de productos con mensaje (Ult 3 horas)")
    
    
  })
  
  
  
  output$mapAct<-renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      fitBounds(lng1 = -74.05393,lat1 = 4.65386,
                lng2 = -74.1080,lat2 = 4.7082)# set to reactive minimums
  })
  
  observe({
    req(filteredData())
    data_p <- filteredData()
    
    data_p2 <- data_p %>% 
      group_by(Establecimiento, lon, lat) %>% 
      summarise(p = sum(Mensaje_Enviado))
    
    leafletProxy("mapAct", data = data_p2) %>%
      clearShapes() %>%
      addCircles(lng = ~lon, lat = ~lat,
                 radius = ~p, fillOpacity = 0.02,color = "#DF2935")
  })
  
  
  output$resu1 <- renderValueBox({
    req(filteredData())
    mensajes_enviados <- sum(filteredData()$Mensaje_Enviado)
    valueBox(value = tags$p(mensajes_enviados, style = "font-size: 150%;"), 
             "Mensajes enviados", icon = icon("envelope"), color = "blue")
  })
  
  output$resu2 <- renderValueBox({
    req(filteredData())
    personas <- length(unique(filteredData()$Id_Cliente))
    valueBox(value = tags$p(personas, style = "font-size: 150%;"), 
             "Clientes han recibido mensaje", icon = icon("users"), 
             color = "light-blue")
  })
  
  output$resu3 <- renderValueBox({
    req(filteredData())
    mensajes_leidos <- sum(filteredData()$Mensaje_abierto)
    mensajes_enviados <- sum(filteredData()$Mensaje_Enviado)
    valueBox(value = tags$p(paste(mensajes_leidos, "(", round(100*mensajes_leidos/mensajes_enviados,1
                                                              ), "%)", sep = ''), style = "font-size: 150%;"), 
             "Mensajes abiertos", 
             icon = icon("award"), color = "aqua")
  })
  
  output$resu4 <- renderValueBox({
    req(reactivo())
    t1 <- reactivo()$base_cl
    n1 <- nrow(t1)
    n1 <- format(round(n1, 0), nsmall=0, big.mark=",")
    valueBox(value = tags$p(n1, style = "font-size: 100%;"), 
             "Clientes", icon = icon("users"), color = "blue")
  })
  
  output$resu5 <- renderValueBox({
    req(reactivo())
    t1 <- reactivo()$base_cl
    n1 <- round(mean(t1$ingresos),2)
    n1 <- format(round(n1, 0), nsmall=0, big.mark=",")
    valueBox(value = tags$p(paste(n1, 'USD'), style = "font-size: 100%;"), 
             "Ingresos promedio", icon = icon("coins"), color = "blue")
  })
  
  output$resu6 <- renderValueBox({
    req(reactivo())
    t1 <- reactivo()$base_cl
    n1 <- round(mean(t1$edad),1)
    valueBox(value = tags$p(paste(n1, 'años'), style = "font-size: 100%;"), 
             "Edad promedio", icon = icon("user-clock"), color = "blue")
  })
  
  output$resu7 <- renderValueBox({
    req(reactivo())
    t1 <- reactivo()$base_cl
    h1 <- round(sum(t1$sexo)/nrow(t1), 2)*100
    m1 <- 100 - h1
    
    
    
    valueBox(value = tags$p(paste(h1, "% ", "-", m1 , "%", sep = ''), style = "font-size: 100%;"), 
             "Proporción de hombres-mujeres", icon = icon("restroom"), color = "blue")
  })
  
  
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  
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






# 
# Lista <- base_final_3 %>% 
#   group_by(Producto) %>% 
#   summarise(n=n())



# fecha_filtro <- max(base_final_3$Hora) - 10800
# Lista <- base_final_3 %>% 
#   filter(Hora >= fecha_filtro) %>% 
#   group_by(Producto) %>% 
#   summarise(n=sum(Mensaje_Enviado))
# 
# selTable <- Lista %>% 
#   left_join(df2)

