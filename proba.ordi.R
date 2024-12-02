## app.R ##
## prueba git ##
## librerías ##
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(sf)
library(leaflet)
library(dplyr)
library(readxl)
library(tidyr)

shp_url <- "mapas/colonia_Satelite 1.shp"
datoscol <- "excel/gdatosdashboard-2.xlsx"

# Leer el shapefile 
mapa <- st_read(shp_url)
poblacion <- read_excel(datoscol)

# Ver los primeros registros del shapefile
head(mapa)

# Unir datos del shapefile con los datos del Excel usando 'CVEGEO'
datos <- poblacion %>% rename(ID = "CVEGEO")
colonia <- mapa %>% rename(ID = "CVEGEO")
mapa_completo <- left_join(colonia, datos, by = "ID")

ui <- dashboardPage(
  dashboardHeader(title = "DashBoard Col. Satelite"),
  dashboardSidebar( 
    sidebarMenu(
      ## MAPAS
      menuItem("Mapas", tabName = "mapas", icon = icon("map")),
      
      # Menú desplegable para escoger el tipo de mapa
      selectInput("map_option", "Elige el tipo de mapa:",
                  choices = c("Seleccionar" = "", "Poblacion total", "Poblacion masculina", "Poblacion femenina"),
                  selected = ""),
      
      ## GRAFICAS
      menuItem("Graficas", tabName = "graficas", icon = icon("chart-bar")),
      
      # Menú de radio buttons para escoger el tipo de gráfico
      selectInput("graph_option", "Elige el tipo de gráfico:",
                  choices = c("Seleccionar" = "", "Poblacion masculina y femenina", "Servicios a salud","Religion"),
                  selected = "")
    )
  ),
  dashboardBody(
    
    # Mensaje inicial cuando no se ha hecho ninguna selección
    textOutput("mensaje_inicial"),
    
    # Contenido que se actualizará dependiendo de la selección de mapas o gráficos
    tabItems(
      tabItem(tabName = "mapas",
              # Si se elige un mapa, este se mostrará
              uiOutput("mapaUI")
      ),
      
      tabItem(tabName = "graficas",
              # Si se elige un gráfico, este se mostrará
              uiOutput("graficoUI")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Mostrar el mensaje inicial si no se ha seleccionado nada
  output$mensaje_inicial <- renderText({
    if(input$map_option == "" && input$graph_option == "") {
      return("Dashboard Colonia Satélite")
    }
  })
  
  # Renderizar el mapa dependiendo de la opción seleccionada
  output$mapaUI <- renderUI({
    # Mostrar el mapa solo si se ha seleccionado una opción válida
    req(input$map_option != "")
    
    # Mostrar el tipo de mapa seleccionado
    output$Mapa_Seleccionado <- renderText({
      paste("Tipo de Mapa Seleccionado: ", input$map_option)
    })
    
    # Renderizar el mapa de calor
    leafletOutput("mapa")
  })
  
  # Renderizar la gráfica dependiendo de la opción seleccionada
  output$graficoUI <- renderUI({
    # Mostrar el gráfico solo si se ha seleccionado una opción válida
    req(input$graph_option != "")
    
    # Mostrar el tipo de gráfico seleccionado
    output$Grafica_Seleccionada <- renderText({
      paste("Tipo de gráfico seleccionado:", input$graph_option)
    })
    
    # Mostrar el gráfico de barras
    plotOutput("grafico_barras")
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
    } else if (input$map_option == "Poblacion femenina") {
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
    } else if (input$map_option == "Poblacion masculina") {
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
  
  # Crear el gráfico de barras comparando la población masculina y femenina
  output$grafico_barras <- renderPlot({
    # Solo crear el gráfico si se seleccionó correctamente la opción
    req(input$graph_option)  # Asegura que input$graph_option no esté vacío
    if(input$graph_option == "Poblacion masculina y femenina"){
    # Crear un data frame para la gráfica de barras con pivot_longer
    df <- mapa_completo %>%
      select(ID, POBMAS, POBFEM) %>%
      pivot_longer(cols = c(POBMAS, POBFEM), 
                   names_to = "sexo", 
                   values_to = "poblacion")
    
  
    
    ggplot(df, aes(x = ID, y = poblacion, fill = sexo)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Colonia", y = "Población", title = "Comparación de Población Masculina vs Femenina") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotar etiquetas de las colonias
   
   
    
    }
    
    else if (input$graph_option == "Servicios a salud") {
      # Crear un data frame para comparar acceso al IMSS y al ISSSTE
      df <- mapa_completo %>%
        select(ID, PDER_IMSS, PDER_ISTE) %>%
        pivot_longer(cols = c(PDER_IMSS, PDER_ISTE), 
                     names_to = "servicio", 
                     values_to = "acceso")
      
      ggplot(df, aes(x = ID, y = acceso, fill = servicio)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Colonia", y = "Acceso", title = "Comparación de Acceso a IMSS vs ISSSTE") +
        scale_fill_manual(values = c("PDER_IMSS" = "lightblue", "PDER_ISTE" = "lightgreen")) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotar etiquetas de las colonias
    }
    
    else if (input$graph_option == "Religion") {
      # Crear un data frame para comparar las religiones por colonia
      df <- mapa_completo %>%
        select(ID, PCATOLICA, PRO_CRIEVA, PSIN_RELIG) %>%
        pivot_longer(cols = c(PCATOLICA, PRO_CRIEVA, PSIN_RELIG), 
                     names_to = "religion", 
                     values_to = "poblacion")
      
      # Crear un gráfico de dispersión, donde cada punto representa a una religión por colonia
      ggplot(df, aes(x = as.factor(ID), y = poblacion, color = religion)) +
        geom_jitter(size = 4, width = 0.1, height = 0) +  # `geom_jitter` dispersa los puntos
        labs(x = "Colonia", y = "Población", title = "Comparación de Religiones por Colonia") +
        scale_color_manual(values = c("PCATOLICA" = "blue", 
                                      "PRO_CRIEVA" = "green", 
                                      "PSIN_RELIG" = "red")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotar las etiquetas de las colonias
    }
    
    
    
    })
}

shinyApp(ui, server)

