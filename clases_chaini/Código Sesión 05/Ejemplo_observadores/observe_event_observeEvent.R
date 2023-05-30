options(shiny.reactlog = TRUE)

library(shiny)

ui<-
  fluidPage(
    fluidRow(
      column(4,
             h2("Prueba del Reactive"),
             textInput("Test_R","Test_R"),
             textInput("Test_R2","Test_R2"),
             textInput("Test_R3","Test_R3"),
             tableOutput("React_Out")
      ),
      column(4,
             h2("Prueba del Observe"),
             textInput("Test","Test"),
             textInput("Test2","Test2"),
             textInput("Test3","Test3"),
             tableOutput("Observe_Out")
      ),
      column(4,
             h2("Prueba del observeEvent"),
             textInput("Test_OE","Test_OE"),
             textInput("Test_OE2","Test_OE2"),
             textInput("Test_OE3","Test_OE3"),
             tableOutput("Observe_Out_E"),
             actionButton("Go","PRUEBA")
      )
      
    ),
    fluidRow(
      column(8,
             h4("Nótese que el observe() y el reactive() trabajan muy similar en la superficie; las diferencias son notorias cuando checamos el código del servidor y como estas diferencias pueden ser explotadas para diferentes usos.")
      ))
    
  )

server<-function(input,output,session){
  
  # Crea un Entorno Reactivo. Nótese que podemos llamar a los valores fuera del ambiente llamando a la variable reactiva. 
  
  Reactive_Var <- reactive({
    c(input$Test_R, input$Test_R2, input$Test_R3) # Valor reactivo
    })
  
  output$React_Out<-renderTable({
    Reactive_Var()
  })
  
  # Crea un ambiente de observación (Observe Event). Nótese que no podemos acceder a los valores creados fuera de este entorno. A, B y C se van a actualizar con el valor que les metamos a los campos de texto de arriba.
  
  observe({ # Creamos el entorno para acceder al input
    
    # Variables creadas en el observe
    A<-input$Test
    B<-input$Test2
    C<-input$Test3
    
    # Generamos el objeto con valores generados dentro del observe
    df<-c(A,B,C)
    
    # Generamos la tabla y la mandamos a la lista output
    output$Observe_Out <- renderTable({df})
    
  })
  
  # Con observeEvent(), podemos cambiar cualquier input las veces que lo necesitemos, sin embargo, no va a correr el código hasta que el "disparador" se active. En este caso, el disparador es el botón input$Go
  
  observeEvent(input$Go, # Disparador
               {
    # Se toman los valores
    A<-input$Test_OE
    B<-input$Test_OE2
    C<-input$Test_OE3
    
    # Se genera el objeto
    df<-c(A,B,C)
    
    # Se guarda en el output
    output$Observe_Out_E<-renderTable({df})
  })
  
}

# Ejecutamos el shiny. 
shinyApp(ui, server)
