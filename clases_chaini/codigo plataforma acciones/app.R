library(shiny)
library(DT)

source("stock_analysis_functions.R")

ui <- fluidPage(
  tags$head(
    # includeCSS("")
    tags$style(
      "
      h3 {
          color:red;
          text-align:center;
      }
      
      h1 {
          color:green;
          text-align:center;
      }
      
      p{
        color:blue;
        text-align:justify;
      }
      "
    )
  ),
  
  h1("Analisis de Acciones"),
  fluidRow(
    column(12, 
           wellPanel(style = "background-color:gray; min-height:150px;", 
                     selectizeInput(inputId = "selAccion", 
                                    label = "Seleccione acción", 
                                    choices = get_stock_list() %>% 
                                      slice(-1)), 
                     actionButton(inputId = "btnCalculo", 
                                  label = "Generar gráfica")
           )
        )
  ),
  fluidRow(
    column(6, 
           h3("Gráfica de Líneas"),
           uiOutput("accion_seleccionada"),
           p("Esta es una gráfica de líneas"),
           plotlyOutput("grafica_lineas", height = "600px"), 
           downloadButton(outputId = "descargaLineas", label = "Descargar"),
           br(), br(), br()
           ), 
    column(6, 
           h3("Gráfica de Velas"),
           p(strong("Esta es una gráfica de velas."),
             "Una gráfica de velas, también conocida como",
             em("gráfico de velas japonesas"),
             "es una herramienta visual utilizada en el análisis técnico de los mercados financieros, como el mercado de acciones, futuros, divisas, entre otros. Se utiliza para representar y analizar movimientos de precios en un período de tiempo específico. Cada 'vela' en el gráfico proporciona cuatro puntos de información importantes acerca del período de tiempo que representa: el precio de apertura, el precio de cierre, el precio más alto alcanzado (máximo) y el precio más bajo alcanzado (mínimo)."),
           plotlyOutput("grafica_velas",  height = "600px"), 
           downloadButton(outputId = "descargaVelas", label = "Descargar"), 
           br(), br(), br()
           )
  ),
  fluidRow(
    column(12,
           wellPanel(
             h3("Tabla"),
             DT::dataTableOutput(outputId = "tabla")
           )
    )
  )
)

server <- function(input, output, session) {
  
  datos_lineas <- reactive({
    input$btnCalculo
    get_stock_data(stock_symbol = 
                     isolate(get_symbol_from_user_input(input$selAccion)))
  })
  
  output$descargaLineas <- downloadHandler(
    filename = function(){
      str_c(input$selAccion, 
            Sys.Date(),
            ".csv")
    }, 
    content = function(name){
      datos_lineas() %>% 
        write.csv(name)
    }
  )

  
  output$grafica_lineas <- renderPlotly({
    plot_stock_data(data = datos_lineas())
  })
  
  datos_velas <- reactive({
    input$btnCalculo
    isolate(get_stock_candles(get_symbol_from_user_input(input$selAccion)))
  })
  
  output$descargaVelas <- downloadHandler(
    filename = function(){
      "datos_velas.csv"
    }, 
    content = function(name){
      datos_velas() %>% 
        write.csv(name)
    }
  )
  
  output$grafica_velas  <- renderPlotly({
    gen_candle_plot(datos_velas())
  })
  
  output$accion_seleccionada <- renderUI({
    input$btnCalculo
    HTML("<h2>", isolate(input$selAccion), "</h2>")
  })
  
  output$tabla <- DT::renderDT({
    DT::datatable(
      datos_lineas() %>% 
        mutate(adjusted = round(adjusted, 1)), 
      options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        pageLength = 10
      )
    )
  })
  
}

shinyApp(ui, server)
