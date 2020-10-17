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
library(shinyjs)

# Define UI for application that draws a map

min_time <- as.POSIXct("2020-01-01 05:00:00")

secuencia <- seq(from = min_time, length.out = 1020, by = "mins")

clientes <- as.character(1:15000)

producto <- paste('Producto', 1:10, sep = '_')

establecimiento <- paste('Establecimiento', 1:150, sep ='_')

filas <- 10000

base_final <- data.frame(Hora = sample(secuencia, filas, replace = TRUE),
                         Id_Cliente = sample(clientes, filas, replace = TRUE),
                         Producto = sample(producto, filas, replace = TRUE),
                         Establecimiento = sample(establecimiento, filas, replace = TRUE))

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
  left_join(d)

summary_total <-  base_final_3 %>% 
  group_by(hora_cierra) %>% 
  summarise(n=n())


min_n <- min(summary_total$n)
max_n <- max(summary_total$n)
min_x <- min(summary_total$hora_cierra)
max_x <- max(summary_total$hora_cierra)
saltos <- seq(min_x, max_x, l = 10)


ui <- shinyUI(
  dashboardPage(dashboardHeader(title = span("Geo Ritmo",
                                             style = "color: white; font-size: 14px"),
                                titleWidth = 400),
                dashboardSidebar(),
                dashboardBody(
                  useShinyjs(),
                  tabsetPanel(type = 'tabs',
                              tabPanel('Offer Summary',
                                       fluidRow(offset = 0, 
                                         column(width = 2, offset = 0),
                                         column(
                                           width = 9, offset = 0,
                                           sliderInput("animation", "Time:",
                                                       min = min(base_final$Hora),
                                                       max = max(base_final$Hora),
                                                       value = min(base_final$Hora),
                                                       step = 1800, 
                                                       animate =
                                                         animationOptions(interval = 1500, loop = TRUE))
                                         )
                                       ), 
                                       fluidRow(offset = 0,
                                       column(
                                         width = 6, offset = 0,
                                         style='padding-top:0px;padding-bottom:10px; padding-left:0px',
                                         plotOutput(outputId = 'tabla_summary', width = "100%")
                                       ),
                                       column(
                                         width = 6, offset = 0,
                                         style='padding-top:0px;padding-bottom:10px; padding-left:0px',
                                         plotOutput(outputId = 'ordering', width = "100%")
                                       )
                                       ),
                                       fluidRow(
                                     column(
                                         width = 9,
                                         DT::dataTableOutput(outputId = 'tabla_inicial', width = "100%")
                                       )
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
    base_final_3 %>% filter(Hora <= till) %>% 
      arrange(desc(Hora))
  })
  
  
  output$tabla_inicial <- DT::renderDataTable({
    req(filteredData())
    Lista <- filteredData() %>% 
      select(-hora_cierra) %>% 
      arrange(desc(Hora))
    
    selTable <- Lista
    
    DT::datatable(data = selTable,
                  escape=FALSE, 
                  options = list(sDom  = '<"top">lrt<"bottom">ip', 
                                 lengthChange = FALSE,
                                 dom = 'l',
                                 searchable = FALSE))
  })
  
  
  output$tabla_summary <- renderPlot({
    req(filteredData())
    Lista <- filteredData() %>% 
      group_by(hora_cierra) %>% 
      summarise(n=n())
    
    selTable <- Lista[-nrow(Lista),]
    
    ggplot(selTable, aes(x=hora_cierra, y=n)) +
      geom_line() + 
      xlab("Hour") +
      ylab("Sent messages") +
      ylim(min_n, max_n) + 
      xlim(min_x, max_x) +
      scale_x_datetime(date_labels = "%H:%M:%S",limits=c(min_x, max_x)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) 
    
    
  })
  
  output$ordering <- renderPlot({
    req(filteredData())
    Lista <- filteredData() %>% 
      group_by(Producto) %>% 
      summarise(n=n())
    
    selTable <- Lista
    
    ggplot(selTable, aes(x = reorder(Producto, -n), y = n)) +
      coord_flip() +
      geom_bar(stat="identity", color='skyblue',fill='steelblue') +
      theme(axis.text.x=element_text(angle=45, hjust=1))
    
    
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




  
 
