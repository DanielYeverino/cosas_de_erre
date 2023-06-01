library(shiny)

source("stock_analysis_functions.R")

ui <- fluidPage(
  h1("Analisis de Acciones"),
  fluidRow(
    column(4, 
           wellPanel(style = "background-color:red; min-height:150px;", 
                     selectizeInput(inputId = "selAccion", 
                                    label = "Seleccione acción", 
                                    choices = get_stock_list() %>% 
                                      slice(-1))
                     )
           ), 
    column(8, 
           wellPanel(style = "background-color:green;", 
                     plotlyOutput("grafica_lineas", height = "600px"), 
                     plotlyOutput("grafica_velas",  height = "600px")
                     )
    )
  )
)

server <- function(input, output, session) {
  
  datos_lineas <- reactive({
    get_stock_data(stock_symbol = 
                     get_symbol_from_user_input(input$selAccion))
  })

  output$grafica_lineas <- renderPlotly({
    plot_stock_data(data = datos_lineas())
  })
  
  datos_velas <- reactive({
    get_stock_candles(get_symbol_from_user_input(input$selAccion))
  })
  
  output$grafica_velas  <- renderPlotly({
    gen_candle_plot(datos_velas())
  })
  
}

shinyApp(ui, server)
