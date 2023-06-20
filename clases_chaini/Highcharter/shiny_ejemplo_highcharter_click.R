library("shiny")
library("highcharter")



ui <- shinyUI(
  fluidPage(
    fluidRow(
      column(width = 8, highchartOutput("hcontainer", height = "500px")),
      column(width = 4, textOutput("text"))
    ), 
    fluidRow(
      column(12, 
             highchartOutput("treemap"), br(),
             verbatimTextOutput("texto_treemap"))
    )
  )
)

server <- function(input, output) {      
  
  a <- data.frame(b = LETTERS[1:10], b_alt = LETTERS[11:20], c = 11:20, d = 21:30, e = 31:40)
  
  output$hcontainer <- renderHighchart({      
    
    canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.series.chart.series[0].options.additionalInfo[event.point.index]]);}")
    legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")
    
    highchart() %>% 
      hc_xAxis(categories = a$b) %>% 
      hc_add_series(name = "c", additionalInfo = a$b_alt, data = a$c) %>%
      hc_add_series(name = "d", data = a$d) %>% 
      hc_add_series(name = "e", data = a$e) %>%
      hc_plotOptions(series = list(events = list(click = canvasClickFunction, legendItemClick = legendClickFunction))) %>%
      hc_chart(type = "column")
    
  })      
  
  makeReactiveBinding("outputText")
  
  observeEvent(input$canvasClicked, {
    outputText <<- paste0("You clicked on series ", input$canvasClicked[1], " and the bar you clicked was from category ", input$canvasClicked[2], ".") 
  })
  
  observeEvent(input$legendClicked, {
    outputText <<- paste0("You clicked into the legend and selected series ", input$legendClicked, ".")
  })
  
  output$text <- renderText({
    outputText      
  })
  
  # -----------
  output$treemap <- renderHighchart({
    set.seed(110)
    # canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.series.chart.series[0].options.additionalInfo[event.point.index]]);}")
    # legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")
    
    # tmFunction <- JS("function(event) {Shiny.onInputChange('tmlegendClicked', [this.name, event.point.node.childrenTotal[0]]);}")
    tmFunction <- JS("function(event) {Shiny.onInputChange('Clickeado', event.point.name);}")  
    
    
    ex <- data.frame(
      # Nivel 1: 
      Level1 = rep(paste0("Country", seq(1:5)), each = 5),
      # Nivel 2: 
      Level2 = rep(paste0("Sector", seq(1:5)), 5),
      # Valor: 
      Percentage = runif(25, 0, 1)
    )
    
    # Datos: 
    ex %>% 
      data_to_hierarchical(c(Level1, Level2), 
                           Percentage, 
                           colors = wesanderson::wes_palettes$Zissou1) %>% 
      hchart(type = "treemap",
             # hcaes(color = Level1),
             allowTraversingTree = T,
             levelIsConstant = F,
             drillUpButton = list(
               text = "← Volver"
             ),
             levels = list(
               list(level = 1, dataLabels = list(enabled = TRUE, 
                                                 format = "{point.name}<br>
                                                      {point.value}%"), borderColor = "black", borderWidth = 2),
               list(level = 2, dataLabels = list(enabled = FALSE))
             )
      ) %>%
      hc_plotOptions(series = list(events = list(click = tmFunction)))
    
  })
  
  observeEvent(input$Clickeado, {
      print("Presionaste el treemap")
      outputTextTreemap <<- str_c("Seleccionaste: ", input$Clickeado)
      output$texto_treemap <- renderText({
        outputTextTreemap
      })  
  })
  
}

shinyApp(ui, server) 