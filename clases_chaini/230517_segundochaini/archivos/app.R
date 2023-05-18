library(shiny)

# Cargar bases----
source("global.R")

# Interfaz
ui <- fluidPage(
  titlePanel(title = "Tablero de indicadores"),
  sidebarLayout(
    sidebarPanel(
      h1("Controles"),
      selectizeInput(
        inputId = "selEntidad",
        label = "Seleccione entidad",
        choices = entidades_a_escoger
        )
    ),
    mainPanel()
  )
)

# Servidor
server <- function(input, output, session) {
  
}

shinyApp(ui, server)