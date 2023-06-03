shinyApp(
  ui = fluidPage(wellPanel(uiOutput("depth_slider_UI")),
                 leafletOutput("my_map")),
  server = function(input, output) {
    output$depth_slider_UI <- renderUI({
      sliderInput(
        "quake_depth",
        "Depth",
        min = 40,
        max = 100,
        value = 50
      )
      
    })
    
    observeEvent(input$quake_depth,
                 {
                   my_quakes <- quakes %>%
                     filter(depth <= input$quake_depth)
                   
                   pal_quakes <- colorNumeric("Purples", quakes$mag)
                   
                   leafletProxy("my_map") %>%
                     clearMarkers() %>%
                     removeControl("legend") %>% 
                     addCircleMarkers(
                       data = my_quakes,
                       stroke = TRUE,
                       fillColor = ~ pal_quakes(mag),
                       color = "#000000"
                     ) %>%
                     addLegend(pal = pal_quakes,
                               values = my_quakes$mag,
                               opacity = 1,
                               layerId = "legend")
                   
                 })
    
    output$my_map <- renderLeaflet({
      leaflet() %>%
        addTiles()
      
    })
    
  }
)