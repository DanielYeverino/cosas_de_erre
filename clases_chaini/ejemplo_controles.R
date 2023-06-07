library(shiny)

ui <- fluidPage(
  titlePanel("Mi Aplicación con Diversos Widgets"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("texto", "Ingresa un texto:", ""),
      passwordInput("password", "Ingresa una contraseña:", ""),
      textAreaInput("areaTexto", "Ingresa un párrafo:", ""),
      checkboxInput("check", "Marcar si aceptas los términos y condiciones:", FALSE),
      radioButtons("radios", "Escoge una opción:", 
                   choices = list("Opción 1" = 1, "Opción 2" = 2, "Opción 3" = 3), selected = 1),
      sliderInput("deslizador", "Escoge un valor:",
                  min = 0, max = 100, value = 50),
      dateInput("fecha", "Escoge una fecha:", Sys.Date()),
      selectInput("selector", "Escoge un elemento:",
                  choices = c("Elemento 1", "Elemento 2", "Elemento 3")),
      actionButton("boton", "Haz clic!")
    ),
    mainPanel(
      # Aquí se puede mostrar el output de las interacciones con los widgets
    )
  )
)

server <- function(input, output) {
  # Aquí va la lógica para manejar las acciones del usuario
}

shinyApp(ui = ui, server = server)