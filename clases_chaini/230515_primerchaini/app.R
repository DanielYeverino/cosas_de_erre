#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram

ui <- fluidPage( # Genera una pagina fluida (es adaptable a la ventana del usuario)

    # Application title
    titlePanel("Old Faithful Geyser Data"), 

    # Sidebar with a slider input for number of bins 
    # Funcion que permite acomodar elementos en la barra lateral
    sidebarLayout(
      
      # Crea un panel lateral
        sidebarPanel(
            sliderInput("bins", # Identificador
                        "Number of bins:", # Que dice el control (etiqueta)
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plotpopo")
        )
    )
)

# Output e input son listas

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Pa pruebas
  # input <- list()
  # input$bins <- 14
  
  # Lista output y el elemento de la lista
    output$plotpopo <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
