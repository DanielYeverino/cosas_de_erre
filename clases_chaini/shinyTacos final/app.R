# Librerias: ----
library(shiny)
library(shinyWidgets)
library(leaflet)
library(googlesheets4)
library(gargle)

source("global.R")

# Sección de google sheets ----
SHEET_ID = "https://docs.google.com/spreadsheets/d/1pkJaxhnONbEB80L5L7yMSkFyIjkPjFLaN_5mX5NRbPs/edit#gid=0"

# Seccion de autenticación:
options(gargle_oauth_cache = ".secrets")
gargle::gargle_oauth_cache()
list.files(".secrets/")

gs4_auth(
  cache = ".secrets",
  email = "juveroark@gmail.com"
)

save_data_gsheets <- function(data) {
  data <- data %>% as.list() %>% data.frame()
  sheet_append(SHEET_ID, data)
}

load_data_gsheets <- function() {
    read_sheet(SHEET_ID)
}               

# Interfaz de Usuario: ----
ui <- navbarPage("Tacos CDMX", id = "nav", 
  tabPanel("Mapa", 
           div(class="outer",
               tags$head(
                   # Include our custom CSS
                  includeCSS("styles.css"),
                  includeScript("gomap.js")
                ),
               
           leafletOutput("mapa_tacos", width = "100%", height = "100%"),
               
           absolutePanel(id = "controls", class = "panel panel-default", 
                         fixed = TRUE, draggable = TRUE,
                         top = 70, left = "auto",
                         right = 20, bottom = "auto",
                         width = 330, height = "auto",
                         h2("Sistema de calificación de tacos por alcaldía"),
                         p(tags$strong("Seleccione una taquería ", style = "color:red;"), 
                           "para mostar información adicional sobre el establecimiento"),
                         uiOutput("info_establecimiento"),
                         br(),
                         selectizeInput(inputId = "selAlcaldia", 
                                        label = "Seleccione alcaldía de la CDMX", 
                                        choices = opcion_alcaldias
                         ), 
                         textAreaInput(inputId = "txtArea", 
                                       label = "Introduzca su reseña", 
                                       value = "", 
                                       placeholder = "Escriba su reseña (max 1000 palabras)", 
                                       height = "84px"), 
                         sliderTextInput(inputId = "sldCalificacion", 
                                         label = "Introduzca su calificación", 
                                         choices = str_c(0:5, "⭐"), 
                                         grid = T), 
                         actionButton("btnAccion", 
                                      label = "Mandar reseña", 
                                      icon = icon("star"), 
                                      class = "btn-success")
           )
           )
          ), 
  tabPanel("Datos")
)

# Servidor ----
server <- function(input, output, session) {
  
  output$mapa_tacos <- renderLeaflet({
    gen_mapa(input$selAlcaldia)
  })
  
  
  observe({
    print(input$mapa_tacos_marker_click$id)
    click <<- input$mapa_tacos_marker_click$id
    
    output$info_establecimiento <- renderUI({
      
      if(is.null(click)){
        
        str_c() %>% 
          HTML()
        
      } else {
      
        info_establecimiento_df <- tacos_cdmx %>% 
          filter(id == click)
        
        str_c(
          "<b style = 'color:green;'>", "Nombre del establecimiento: ",  "</b>", info_establecimiento_df$nom_estab, "<br>",
          "<b>", "Nombre de actividad: ",   "</b>", info_establecimiento_df$nombre_act, "<br>",
          "<b>", "Personal ocupado: ",  "</b>", info_establecimiento_df$per_ocu, "<br>",
          "<b>", "Asentamiento: ",  "</b>", info_establecimiento_df$nomb_asent,  "<br>"
        ) %>% HTML()
      }
    })
  })
  
  datos_a_enviar_al_drive <- reactive({
    alcaldia <- input$selAlcaldia
    resenia <- input$txtArea
    calificacion <- input$sldCalificacion
    establecimiento <- tacos_cdmx %>% 
      filter(id == click) %>% 
      pull(nom_estab)
    
    datos_gs <- c(alcaldia, 
                  resenia, 
                  calificacion, 
                  establecimiento)
    
    datos_gs
  })
  
  # Sección del botón: 
  observeEvent(input$btnAccion, {
    
    if(is.null(input$txtArea) | input$txtArea == ""){
      showModal(
        modalDialog(
          title = "Reseña no enviada", 
          "Reseña no válida"
        )
      )
    } else {
      save_data_gsheets(data = datos_a_enviar_al_drive())
      
      updateTextAreaInput(session = session, 
                          inputId = "txtArea", 
                          label = "Introduzca su reseña", 
                          value = "", 
                          placeholder = "Escriba su reseña (max 1000 palabras)"
      )
      updateSliderTextInput(session = session, 
                            inputId = "sldCalificacion", 
                            label = "Introduzca su calificación",
                            choices = str_c(0:5, "⭐"), 
                            selected = "0⭐")
      
      showModal(
        modalDialog(
          title = "Reseña enviada", 
          "Te agradecmos tu colaboración... Haremos llegar la reseña a los dueños del restaurante"
        )
      )
    }
    
  })

}

shinyApp(ui, server)

