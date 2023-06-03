library(tidyverse)
library(sf)
library(leaflet)

shp <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

x_i <- c(st_coordinates(shp)[,1] %>% min(), st_coordinates(shp)[,1] %>% max()) %>% mean()
y_i <- c(st_coordinates(shp)[,2] %>% min(), st_coordinates(shp)[,2] %>% max()) %>% mean()

pal_color_area <- colorNumeric(palette = "viridis", 
                               domain = shp$AREA)

# var = "Area"
gen_map <- function(var){
  
  if(var == "Area"){
    pal_color <- colorNumeric(palette = "viridis", domain = shp$AREA)
    titulo = "Área"
    shp <- shp %>% mutate(var_interes = AREA)
  } else if(var == "Perímetro"){
    pal_color <- colorNumeric(palette = "magma", domain = shp$PERIMETER)
    titulo = "Perímetro"
    shp <- shp %>% mutate(var_interes = PERIMETER)
  }
  
  leaflet(shp) %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(fillColor = pal_color(shp$var_interes), 
                fillOpacity = 1, 
                color = "white", 
                opacity = 1, 
                weight = 1)
}


library(shiny)

ui <- fluidPage(
  titlePanel("Mapa"),
  sidebarLayout(  
    sidebarPanel(
      radioButtons(inputId = "sel_mapa",
                     label = "Seleccione Variable", 
                     choices = c("Area", "Perímetro"))
    ), 
    mainPanel(
      leafletOutput("mapa", height = "80vh")
    )
  )
)
  

server <- function(input, output, session) {
  
  output$mapa <- renderLeaflet({
    # Solo las primeras capas del mapa: 
    leaflet(shp) %>% 
      addProviderTiles("CartoDB.Positron")
  })
  
  # Detonamos el observador: 
  observe({
    # Que observe el cambio en el control del sel mapa: 
    if(input$sel_mapa == "Area"){
      pal_color <- colorNumeric(palette = "viridis", domain = shp$AREA)
      shp <- shp %>% mutate(var_interes = AREA)
      titulo = "Area"
    } else if(input$sel_mapa == "Perímetro"){
      pal_color <- colorNumeric(palette = "magma", domain = shp$PERIMETER)
      shp <- shp %>% mutate(var_interes = PERIMETER)
      titulo = "Perímetro"
    }
    
    # Para que cambie la info del mapa sin recalcularse desde cero: 
    leafletProxy("mapa", data = shp) %>%
      # setView(lng = x_i, lat = y_i, zoom = 5) %>% 
      # clearShapes() %>%
      removeControl("legend") %>% 
      addPolygons(fillColor = ~pal_color(var_interes), 
                  fillOpacity = 1, 
                  color = "white", 
                  opacity = 1, 
                  weight = 1) %>% 
      addLegend(pal = pal_color, values = shp$var_interes, title = titulo, 
                layerId = "legend")
  })
}

shinyApp(ui, server)
