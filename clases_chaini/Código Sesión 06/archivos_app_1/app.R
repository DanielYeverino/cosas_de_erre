options(shiny.reactlog = TRUE)

# Librerias
library(shiny)
library(shinycssloaders)
library(reactlog)

# Cargar las funciones y los datos: 
source("global.R")

# Interfaz de usuario
ui <- fluidPage(
  titlePanel(title = "Tablero de indicadores"),
  sidebarLayout(
    sidebarPanel(
      h1("Controles"),
      selectizeInput(inputId = "selEntidad", 
                     label = "Seleccione entidad", 
                     choices = entidades_a_escoger
                     ), 
      uiOutput("selMunicipio"),
      selectizeInput(inputId = "selIndicador", 
                     label = "Seleccione indicador", 
                     choices = indicadores_disponibles
      ), 
      uiOutput("selAnio")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Barras", plotlyOutput("grafica_barras", height = "600px") %>% 
                   withSpinner()), 
        tabPanel("Líneas", plotlyOutput("grafica_lineas", height = "600px")), 
        tabPanel("Mapa", leafletOutput("mapa", height = "600px"))
      )
    )
  )  
)

# Servidor 
server <- function(input, output, session) {
  
  output$selMunicipio <- renderUI({
    selectizeInput(inputId = "selMunicipio", 
                   label = "Seleccione Municipio", 
                   choices = gen_municipios(cve_ent_sel = input$selEntidad))
  })
  
  output$selAnio <- renderUI({
    selectizeInput(inputId = "selAnio", 
                   label = "Seleccione año", 
                   choices = anio_disponible_por_indicador(no_sel = input$selIndicador))
  })
  
  metadatos_sel <- reactive({
    gen_metadatos(no_sel = input$selIndicador)
  })
  
  output$grafica_lineas <- renderPlotly({
    gen_linea(no_sel = input$selIndicador, 
              cve_mun_sel = input$selMunicipio, 
              cve_ent_sel = input$selEntidad, 
              metadatos = metadatos_sel())
  })
  
  output$grafica_barras <- renderPlotly({
    gen_barras(no_sel = input$selIndicador,
               year_sel = input$selAnio, 
               cve_ent_sel = input$selEntidad, 
               metadatos = metadatos_sel())
  })
  
  output$mapa <- leaflet::renderLeaflet({
    gen_mapa(ent_sel = input$selEntidad,
             no_sel = input$selIndicador,
             year_sel = input$selAnio, 
             metadatos = metadatos_sel())
  })
  
}

shiny::reactlog()
shinyApp(ui, server)
