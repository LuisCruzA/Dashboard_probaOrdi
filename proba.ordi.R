## app.R ##
##prueba git##
##librerias##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(sf)
library(leaflet)


shp_url <- "mapas/colonia_Satelite 1.shp"

# Leer el shapefile 
mapa <- st_read(shp_url)

# Ver los primeros registros del shapefile
head(mapa)

ui <- dashboardPage(
  dashboardHeader(title = "DashBoard Col. Satelite"),
  dashboardSidebar( 
    sidebarMenu(
      ##MAPAS
      menuItem("Mapas", tabName = "histogramas_mapas", icon = icon("map")),
      
      
      # Menú desplegable para escoger el tipo de mapa
      selectInput("map_option", "Elige el tipo de mapa:",
                  choices = c("Poblacion total", "poblacion masculina", "poblacion femenina"),
                  selected = "Poblacion total"),
      
      ##GRAFICAS
      menuItem("Graficas", tabName = "histo", icon= icon("chart-bar")),
      
      
      
      # Menú de radio buttons para escoger el tipo de gráfico
      selectInput("graph_option", "Elige el tipo de gráfico:",
                   choices = c("poblacion total", "poblacion masculina", "poblacion femenina"),
                   selected = "poblacion total")
      
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
              textOutput("Mapa_Seleccionado"),
              # Salida para mostrar el mapa
              leafletOutput("mapa")
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
