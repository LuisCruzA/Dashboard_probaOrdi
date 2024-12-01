## app.R ##
##prueba git##
##librerias##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(sf)
library(leaflet)
library(dplyr)
library(readxl)


shp_url <- "mapas/colonia_Satelite 1.shp"

datoscol <- "excel/gdatosdashboard-2.xlsx"

# Leer el shapefile 
mapa <- st_read(shp_url)
poblacion <- read_excel(datoscol)

# Ver los primeros registros del shapefile
head(mapa)

#unir datos del shp con los datos del excel usando cvgeo
datos <- poblacion %>% rename(ID = "CVEGEO")
colonia <- mapa %>% rename(ID = "CVEGEO")
mapa_completo <- left_join(colonia, datos, by = "ID")

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
  # Renderizar el mapa de calor
  output$mapa <- renderLeaflet({
    mapa <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    if (input$map_option == "Poblacion total") {
      mapa <- mapa %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("YlOrRd", mapa_completo$POBTOT)(POBTOT), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Población Total: ", POBTOT)) %>%
        addLegend(pal = colorNumeric("YlOrRd", mapa_completo$POBTOT), 
                  values = mapa_completo$POBTOT, 
                  opacity = 0.7, 
                  title = "Población Total",
                  position = "bottomright")
    } else if (input$map_option == "poblacion femenina") {
      mapa <- mapa %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("YlOrRd", mapa_completo$POBFEM)(POBFEM), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Población Femenina Total: ", POBFEM)) %>%
        addLegend(pal = colorNumeric("YlOrRd", mapa_completo$POBFEM), 
                  values = mapa_completo$POBFEM, 
                  opacity = 0.7, 
                  title = "Población Femenina Total",
                  position = "bottomright")
    } else if (input$map_option == "poblacion masculina") {
      mapa <- mapa %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("YlOrRd", mapa_completo$POBMAS)(POBMAS), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Población Masculina Total: ", POBMAS)) %>%
        addLegend(pal = colorNumeric("YlOrRd", mapa_completo$POBMAS), 
                  values = mapa_completo$POBMAS, 
                  opacity = 0.7, 
                  title = "Población Masculina Total",
                  position = "bottomright")
    }
    
    mapa
  })

}

shinyApp(ui, server)
