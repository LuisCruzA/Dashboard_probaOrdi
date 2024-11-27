## app.R ##
##prueba git##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "DashBoard Col. Satelite"),
  dashboardSidebar( # Corrige el nombre a "dashboardSidebar"
    sidebarMenu(
      menuItem("Mapas", tabName = "histogramas_mapas", icon = icon("map")),
      menuItem("Histogramas", tabName = "histo", icon= icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItem(tabName = "histogramas_mapas"
      
    ),
    tabItem(tabName = "histo")
  )
)

server <- function(input, output) { }

shinyApp(ui, server)
