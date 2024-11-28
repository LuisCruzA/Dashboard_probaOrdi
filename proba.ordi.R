## app.R ##
##prueba git##
library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "DashBoard Col. Satelite"),
  dashboardSidebar( 
    sidebarMenu(
      menuItem("Mapas", tabName = "histogramas_mapas", icon = icon("map")),
      
      
      # Menú desplegable para escoger el tipo de mapa
      selectInput("map_option", "Elige el tipo de mapa:",
                  choices = c("Mapa de Calor", "Mapa de Puntos", "Mapa de Líneas"),
                  selected = "Mapa de Calor"),
      
      
      menuItem("Graficas", tabName = "histo", icon= icon("chart-bar")),
      
      
      
      # Menú de radio buttons para escoger el tipo de gráfico
      selectInput("graph_option", "Elige el tipo de gráfico:",
                   choices = c("Histograma", "Gráfico de Barras", "Gráfico de Dispersión"),
                   selected = "Histograma")
      
    )
  ),
  dashboardBody(
    # Pestaña de Mapas
    tabItem(tabName = "histogramas_mapas",
            box(
              title = "Opciones de Mapa",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              # texto dde salida
              textOutput("Mapa_Seleccionado")
            )
    ),
    
    # Pestaña de Gráficas
    tabItem(tabName = "histo",
            box(
              title = "Opciones de Gráficas",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              # texto de salida
              textOutput("Grafica_Seleccionada")
            )
    )
  )
)

server <- function(input, output) {
  #se muestra la opcion seleccionada para mapas
  output$Mapa_Seleccionado <- renderText({
    paste("Tipo de Mapa Seleccionado: ", input$map_option)
    
  })
  
  # Mostrar la opción seleccionada para gráficos
  output$Grafica_Seleccionada <- renderText({
    paste("Tipo de gráfico seleccionado:", input$graph_option)
  })
}

shinyApp(ui, server)
