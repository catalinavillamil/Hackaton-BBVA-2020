# this is a shiny web app. Save as app.r
#lastupdate: acvillami


library(leaflet)
library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
# library(ggpmisc)
library(shinyWidgets)
library(shinyjs)
library(ggimage)
library(ggthemes)


# Define UI for application that draws a map

min_time <- as.POSIXct("2020-01-01 06:00:00")

secuencia <- seq(from = min_time, length.out = 1800, by = 30)

clientes <- as.character(1:15000)

producto <- paste('Producto', 1:5, sep = '_')

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

df2 <- data.frame(Producto = c("Producto_1", "Producto_2", "Producto_3", "Producto_4", "Producto_5"),
                  Image = sample(c("image1.png","image2.png", "image1.png", "image1.png", "image2.png")),
                  stringsAsFactors = F)


ui <- shinyUI(
  dashboardPage(dashboardHeader(title = span("",
                                             style = "color: white; font-size: 14px")
                                ),
                dashboardSidebar(),
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
                       ')
                  ),
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
                                                         animationOptions(interval = 3000, loop = TRUE))
                                         )
                                       ), 
                                       fluidRow(offset = 0,
                                       column(width = 1),
                                       column(
                                         width = 5, offset = 0,
                                         style='padding-top:0px;padding-bottom:10px; padding-left:0px',
                                         plotOutput(outputId = 'tabla_summary', width = "80%")
                                       ),
                                   
                                       column(
                                         width = 5, offset = 0,
                                         style='padding-top:0px;padding-bottom:30px; padding-left:0px',
                                         plotOutput(outputId = 'ordering', width = "80%")
                                       )
                                       ),
                                       fluidRow(
                                         column(width = 1),
                                     column(
                                         width = 10,
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
      xlab("Hour") +
      ylab("Sent messages") +
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
      ggtitle("Sent messages every 30 min")
    
    
  })
  
  output$ordering <- renderPlot({
    req(filteredData())
    
    fecha_filtro <- max(filteredData()$Hora) - 10800
    Lista <- filteredData() %>% 
      filter(Hora >= fecha_filtro) %>% 
      group_by(Producto) %>% 
      summarise(n=sum(Mensaje_Enviado))
    
    selTable <- Lista %>% 
      arrange(desc(n)) %>% 
      left_join(df2)
    
   colors_blue <- data.frame(Producto = unique(selTable$Producto),
            colores_blue = RColorBrewer::brewer.pal(length(unique(selTable$Producto)), 'Blues'))
    
  
   selTable <- selTable %>% 
     left_join(colors_blue)
   
ggplot(data = selTable, aes(x = reorder(Producto,n),  y = n, fill = colores_blue, color = colores_blue)) +
      geom_col() +
      scale_fill_manual(values = selTable$colores_blue)  +
      geom_text(aes(y = 0, label = Producto), size = 5, color="black", hjust = -0.05) +
      geom_image(aes(x = Producto, image = Image), y = 0,  # add geom_image layer
                 size = 0.1, hjust = 1,
                 inherit.aes = FALSE) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      guides(color = FALSE, fill = FALSE) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, colour = 'white'),
            axis.ticks.y = element_blank(),
            axis.text.y  = element_blank(),
            plot.margin = margin(1, 1, 1, 4, "cm"),
            axis.title.y=element_blank(),
            plot.background=element_rect(fill = "#072146"),
            panel.background = element_rect(fill = '#072146'),
            panel.border = element_rect(colour = "#072146", fill=NA, size=1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x=element_text(colour="white")) +
      ylab("") +
  ggtitle("Top 5 of products whit message (Last 3Hours)")
    
    
  })
  
  
  
  
  
  # addClass(selector = "body", class = "sidebar-collapse")
  
  
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

