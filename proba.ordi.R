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
library(DT)

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
                  choices = c("Seleccionar" = "", "Poblacion total", "Poblacion masculina", "Poblacion femenina", "Hombres por cada 100 mujeres","Religión",
                              "Servicios de Salud"),
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
  
  # Función para calcular la moda
  calculate_mode <- function(x) {
    uniq_x <- unique(x)
    uniq_x[which.max(tabulate(match(x, uniq_x)))]
  }
  
  # Renderizar el mapa dependiendo de la opción seleccionada
  output$mapaUI <- renderUI({
    # Mostrar el mapa solo si se ha seleccionado una opción válida
    req(input$map_option != "")
    
    # Calcular estadísticas para la variable seleccionada
    selected_variable <- switch(input$map_option,
                                "Poblacion total" = mapa_completo$POBTOT,
                                "Poblacion femenina" = mapa_completo$POBFEM,
                                "Poblacion masculina" = mapa_completo$POBMAS,
                                "Hombres por cada 100 mujeres" = mapa_completo$REL_H_M,
                                "Religión" = 0,
                                "Servicios de Salud" = 0)
                                
    
    # Calcular estadísticas
    moda <- calculate_mode(selected_variable)
    mediana <- median(selected_variable, na.rm = TRUE)
    media <- mean(selected_variable, na.rm = TRUE)
    maximo <- max(selected_variable, na.rm = TRUE)
    minimo <- min(selected_variable, na.rm = TRUE)
    
    # Organizar las estadísticas en un data.frame para mostrarlas en una tabla
    stats_df <- data.frame(
      Estadística = c("Moda", "Mediana", "Media", "Máximo", "Mínimo"),
      Valor = c(moda, mediana, media, maximo, minimo)
    )
    
    # Mostrar las estadísticas como una tabla interactiva
    if(input$map_option != "Religión" && input$map_option != "Servicios de Salud"){
    output$estadisticas <- DT::renderDataTable({
      datatable(stats_df, options = list(dom = 't', paging = FALSE, searching = FALSE), 
                class = "display nowrap", rownames = FALSE)
    })
    }
    
    else if(input$map_option == "Religión") {
      # Mostrar estadísticas de las tres religiones
      stats_catolica <- data.frame(
        Estadística = c("Moda", "Mediana", "Media", "Máximo", "Mínimo"),
        Valor = c(calculate_mode(mapa_completo$PCATOLICA),
                  median(mapa_completo$PCATOLICA, na.rm = TRUE),
                  mean(mapa_completo$PCATOLICA, na.rm = TRUE),
                  max(mapa_completo$PCATOLICA, na.rm = TRUE),
                  min(mapa_completo$PCATOLICA, na.rm = TRUE))
      )
      
      stats_protestante <- data.frame(
        Estadística = c("Moda", "Mediana", "Media", "Máximo", "Mínimo"),
        Valor = c(calculate_mode(mapa_completo$PRO_CRIEVA),
                  median(mapa_completo$PRO_CRIEVA, na.rm = TRUE),
                  mean(mapa_completo$PRO_CRIEVA, na.rm = TRUE),
                  max(mapa_completo$PRO_CRIEVA, na.rm = TRUE),
                  min(mapa_completo$PRO_CRIEVA, na.rm = TRUE))
      )
      stats_sin_religion <- data.frame(
        Estadística = c("Moda", "Mediana", "Media", "Máximo", "Mínimo"),
        Valor = c(calculate_mode(mapa_completo$PSIN_RELIG),
                  median(mapa_completo$PSIN_RELIG, na.rm = TRUE),
                  mean(mapa_completo$PSIN_RELIG, na.rm = TRUE),
                  max(mapa_completo$PSIN_RELIG, na.rm = TRUE),
                  min(mapa_completo$PSIN_RELIG, na.rm = TRUE))
      )
     
      
      # Renderizar las tres tablas de estadísticas
      output$estadisticas_catolica <- DT::renderDataTable({
        datatable(stats_catolica, options = list(dom = 't', paging = FALSE, searching = FALSE), 
                  class = "display nowrap", rownames = FALSE)
      })
      
      output$estadisticas_protestante <- DT::renderDataTable({
        datatable(stats_protestante, options = list(dom = 't', paging = FALSE, searching = FALSE), 
                  class = "display nowrap", rownames = FALSE)
      })
      
      output$estadisticas_sin_religion <- DT::renderDataTable({
        datatable(stats_sin_religion, options = list(dom = 't', paging = FALSE, searching = FALSE), 
                  class = "display nowrap", rownames = FALSE)
      })
      
    }
    
    else if(input$map_option == "Servicios de Salud") {
      # Mostrar estadísticas de los servicios de salud
      stats_isste <- data.frame(
        Estadística = c("Moda", "Mediana", "Media", "Máximo", "Mínimo"),
        Valor = c(calculate_mode(mapa_completo$PDER_ISTE),
                  median(mapa_completo$PDER_ISTE, na.rm = TRUE),
                  mean(mapa_completo$PDER_ISTE, na.rm = TRUE),
                  max(mapa_completo$PDER_ISTE, na.rm = TRUE),
                  min(mapa_completo$PDER_ISTE, na.rm = TRUE))
      )
      
      stats_imss <- data.frame(
        Estadística = c("Moda", "Mediana", "Media", "Máximo", "Mínimo"),
        Valor = c(calculate_mode(mapa_completo$PDER_IMSS),
                  median(mapa_completo$PDER_IMSS, na.rm = TRUE),
                  mean(mapa_completo$PDER_IMSS, na.rm = TRUE),
                  max(mapa_completo$PDER_IMSS, na.rm = TRUE),
                  min(mapa_completo$PDER_IMSS, na.rm = TRUE))
      )
      stats_sin_servicio <- data.frame(
        Estadística = c("Moda", "Mediana", "Media", "Máximo", "Mínimo"),
        Valor = c(calculate_mode(mapa_completo$PSINDER),
                  median(mapa_completo$PSINDER, na.rm = TRUE),
                  mean(mapa_completo$PSINDER, na.rm = TRUE),
                  max(mapa_completo$PSINDER, na.rm = TRUE),
                  min(mapa_completo$PSINDER, na.rm = TRUE))
      )
      
      
      # Renderizar las tres tablas de estadísticas
      output$estadisticas_isste <- DT::renderDataTable({
        datatable(stats_isste, options = list(dom = 't', paging = FALSE, searching = FALSE), 
                  class = "display nowrap", rownames = FALSE)
      })
      
      output$estadisticas_imss <- DT::renderDataTable({
        datatable(stats_imss, options = list(dom = 't', paging = FALSE, searching = FALSE), 
                  class = "display nowrap", rownames = FALSE)
      })
      
      output$estadisticas_sin_servicio <- DT::renderDataTable({
        datatable(stats_sin_servicio, options = list(dom = 't', paging = FALSE, searching = FALSE), 
                  class = "display nowrap", rownames = FALSE)
      })
      
    }
    
    # Mostrar el tipo de mapa seleccionado
    output$Mapa_Seleccionado <- renderText({
      
    })
   
    fluidRow(
      box(
        title = "Mapa Seleccionado", 
        status = "info",  # Puedes cambiar el color de fondo
        solidHeader = TRUE,  # Hace que el cuadro tenga un encabezado sólido
        width = 12,  # Puedes ajustar el tamaño del cuadro
        textOutput("Mapa_Seleccionado"),  # Mostrar el mensaje
        headerBorder = FALSE,  # Quitar el borde del encabezado
        tags$style(HTML('
        
        .box-header {
          background-color: lightblue !important;
          color: black !important;
        }
      ')), 
        
        
      
      # Cuadro con el mapa
      column(
        width = 8,  # Ancho del mapa
        box(
          title = paste("Tipo de Mapa Seleccionado: ", input$map_option), 
          status = "info", 
          solidHeader = TRUE, 
          width = NULL,  # El ancho es flexible
          leafletOutput("mapa")
        )
      ),
      
      # Cuadro con las estadísticas
      if(input$map_option != "Religión" && input$map_option != "Servicios de Salud"  ){
      box(
        title = "Estadísticas de la variable", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 4,  # Ancho del cuadro de las estadísticas
        DT::dataTableOutput("estadisticas")
      )
      }
      # Cuadro con las estadísticas específicas de religión
      else if(input$map_option == "Religión") {
        column(
          width = 12,
          box(
            title = "Estadísticas Católicas", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,  
            DT::dataTableOutput("estadisticas_catolica")
          ),
          box(
            title = "Estadísticas Protestantes", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,  
            DT::dataTableOutput("estadisticas_protestante")
          ),
          box(
            title = "Estadísticas Sin Religión", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,  
            DT::dataTableOutput("estadisticas_sin_religion")
          )
          
        )
      }
      
      else if(input$map_option == "Servicios de Salud") {
        column(
          width = 12,
          box(
            title = "Estadísticas Issste", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,  
            DT::dataTableOutput("estadisticas_isste")
          ),
          box(
            title = "Estadísticas Imss", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,  
            DT::dataTableOutput("estadisticas_imss")
          ),
          box(
            title = "Estadísticas Sin servicio", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 4,  
            DT::dataTableOutput("estadisticas_sin_servicio")
          )
          
        )
      }
      )
  
      
      
      
    
    )
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
                    popup = ~paste("Cuadra: ", ID,
                                   "<br>Población Total: ", POBTOT)) %>%
        addLegend(pal = colorNumeric("YlOrRd", mapa_completo$POBTOT), 
                  values = mapa_completo$POBTOT, 
                  opacity = 0.7, 
                  title = "Población Total",
                  position = "bottomleft")
    } else if (input$map_option == "Poblacion femenina") {
      mapa <- mapa %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("YlOrRd", mapa_completo$POBFEM)(POBFEM), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Cuadra: ", ID,
                                   "<br>Población Femenina Total: ", POBFEM)) %>%
        addLegend(pal = colorNumeric("YlOrRd", mapa_completo$POBFEM), 
                  values = mapa_completo$POBFEM, 
                  opacity = 0.7, 
                  title = "Población Femenina Total",
                  position = "bottomleft")
    } else if (input$map_option == "Poblacion masculina") {
      mapa <- mapa %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("YlOrRd", mapa_completo$POBMAS)(POBMAS), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Cuadra: ",ID,
                                   "<br>Población Masculina Total: ", POBMAS)) %>%
        addLegend(pal = colorNumeric("YlOrRd", mapa_completo$POBMAS), 
                  values = mapa_completo$POBMAS, 
                  opacity = 0.7, 
                  title = "Población Masculina Total",
                  position = "bottomleft")
    }
    
    else if (input$map_option == "Hombres por cada 100 mujeres") {
      # Mapa de hombres por cada 100 mujeres
      mapa <- mapa %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("YlOrRd", mapa_completo$REL_H_M)(REL_H_M), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Colonia: ", ID, 
                                   "<br>Hombres por cada 100 mujeres: ", REL_H_M)) %>%
        addLegend(pal = colorNumeric("YlOrRd", mapa_completo$REL_H_M), 
                  values = mapa_completo$REL_H_M, 
                  opacity = 0.7, 
                  title = "Hombres por cada 100 Mujeres",
                  position = "bottomleft")
    }
    else if (input$map_option == "Religión") {
      mapa <- mapa %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("YlOrRd", mapa_completo$PCATOLICA)(PCATOLICA), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Colonia: ", ID,
                                   "<br>Población Católica: ", PCATOLICA),
                    group = "Católica") %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("YlOrRd", mapa_completo$PRO_CRIEVA)(PRO_CRIEVA), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Colonia: ", ID,
                                   "<br>Población Protestante: ", PRO_CRIEVA),
                    group = "Protestante") %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("YlOrRd", mapa_completo$PSIN_RELIG)(PSIN_RELIG), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Colonia: ", ID,
                                   "<br>Población Sin Religión: ", PSIN_RELIG),
                    group = "Sin Religión") %>%
        addLayersControl(
          overlayGroups = c("Católica", "Protestante", "Sin Religión"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    }
    
    else if (input$map_option == "Servicios de Salud") {
      mapa <- mapa %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("BuGn", mapa_completo$PDER_ISTE)(PDER_ISTE), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Colonia: ", ID,
                                   "<br>Población con Issste: ", PDER_ISTE),
                    group = "ISSSTE") %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("PuBu", mapa_completo$PDER_IMSS)(PDER_IMSS), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Colonia: ", ID,
                                   "<br>Población con Imss: ", PDER_IMSS),
                    group = "IMSS") %>%
        addPolygons(data = mapa_completo, 
                    fillColor = ~colorNumeric("YlOrRd", mapa_completo$PSINDER)(PSINDER), 
                    color = "black", 
                    weight = 1, 
                    opacity = 1, 
                    fillOpacity = 0.7, 
                    popup = ~paste("Colonia: ", ID,
                                   "<br>Población Sin Servicio a Salud: ", PSINDER),
                    group = "Sin Servicio") %>%
        addLayersControl(
          overlayGroups = c("ISSSTE", "IMSS", "Sin Servicio"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
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

