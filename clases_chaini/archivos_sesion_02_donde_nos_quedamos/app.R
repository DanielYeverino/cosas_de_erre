# Librerias
library(shiny)
library(shinycssloaders)

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
      uiOutput(
        "selMunicipio"
      ),
      selectizeInput(inputId = "selIndicador", 
                     label = "Seleccione indicador", 
                     choices = indicadores_disponibles
      ),
      uiOutput(
        "selAnio"
      )
    ),
    mainPanel(
      tabsetPanel(
        
        tabPanel(
          "Barras", 
          plotlyOutput("grafica_barras", height = "600px") %>% 
            withSpinner()
        ),
        tabPanel(
          "Líneas", 
          plotOutput("grafica_lineas", height = "600px") %>% 
            withSpinner()
        ),
        tabPanel(
          "Mapa", 
          plotOutput("mapa", height = "600px") %>% 
            withSpinner()
        )
        
      )
    )
  )  
)

# Servidor 
server <- function(input, output, session) {
  
  output$selMunicipio <- renderUI({
    
    selectizeInput(
      inputId = "selMunicipio",
      label = "Seleccione municipio",
      choices = gen_municipios(cve_ent_sel = input$selEntidad)
      )
    
  })
    
    output$selAnio <- renderUI({
      
      selectizeInput(
        inputId = "selAnio",
        label = "Seleccione año",
        choices = anio_disponible_por_indicador(no_sel = input$selIndicador)
      )
    
  })
  
    output$grafica_lineas <- renderPlot({
      
      gen_linea(
        cve_ent_sel = input$selEntidad,
        cve_mun_sel = input$selMunicipio,
        no_sel = input$selIndicador
          )
      
    })
    
    output$grafica_barras <- renderPlotly({
      
      gen_barras(
        year_sel = input$selAnio, 
        no_sel = input$selIndicador, 
        cve_ent_sel = input$selEntidad
      )
      
    })
    
    
    output$mapa <- renderPlot({
      
      gen_mapa(
        ent_sel = input$selEntidad, 
        no_sel = input$selIndicador, 
        year_sel = input$selAnio
      )
      
    })
    
}

shinyApp(ui, server)

