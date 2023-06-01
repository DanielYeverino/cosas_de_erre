library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Dashboard con dataset 'cars'",
    titleWidth = "250px"
    ),
  dashboardSidebar(
    width = "250px",
    sidebarMenu(
      menuItem("Gráficos", tabName = "plots", icon = icon("bar-chart-o")),
      menuItem("Mensajes", tabName = "msj", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plots",
              
              fluidRow(
                box(plotOutput("scatterPlot")),
                box(plotOutput("histPlot"))
              )
      ),
      tabItem(tabName = "msj",
              
              br(),
              h1("Popo")
      )
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    with(cars, plot(speed, dist, main = "Gráfico de Dispersión - Velocidad vs Distancia", xlab = "Velocidad", ylab = "Distancia"))
  })
  
  output$histPlot <- renderPlot({
    with(cars, hist(speed, main = "Histograma - Velocidad", xlab = "Velocidad", col = "lightblue"))
  })
  
}

shinyApp(ui = ui, server = server)