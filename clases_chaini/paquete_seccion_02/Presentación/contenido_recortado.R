<!-- --- -->
  <!-- background-image: url(Imagenes/RLogo.png) -->
  <!-- background-size: 100px -->
  <!-- background-position: 96% 3% -->
  
  <!-- ## Repaso de operadores de toma de decisiones: `if` y `else`. -->
  
  <!-- Hay veces que queremos que se ejecute un código cuando se cumplen ciertas condiciones (por ejemplo, que pase cierta cantidad de tiempo, que una variable tome un valor determinado, etc.). -->
  
  <!-- **En shiny, hay ocasiones que cierto código se debe activar cuando se cumplen ciertas condiciones, y es por eso que es necesario revisar estos operadores. ** -->
  
  <!-- --- -->
  <!-- background-image: url(Imagenes/RLogo.png) -->
  <!-- background-size: 100px -->
  <!-- background-position: 96% 3% -->
  
  <!-- ## 1. `if` -->
  
  <!-- La sintaxis de una decisión de `if` es la siguiente:  -->
  
  <!-- ```{r, eval = FALSE} -->
  <!-- if (expresion_de_pureba) { -->
      <!-- operaciones -->
      <!-- } -->
  <!-- ``` -->
  
  <!-- Si la `expresion_de_prueba` es verdadera (la expresion de prueba debe resultar en `TRUE` o `FALSE`) entonces se ejecutan las `operaciones`. Si esta es falsa, no sucede nada.  -->
  
  <!-- --- -->
  <!-- ## 1. `if` -->
  
  <!-- <p style='text-align:center;'><img src="Imagenes/ExpresionIF.png" width="65%"></p> -->
    
    <!-- --- -->
    <!-- background-image: url(Imagenes/RLogo.png) -->
    <!-- background-size: 100px -->
    <!-- background-position: 96% 3% -->
    
    <!-- ## 1. `if` -->
    
    <!-- * Ejemplo práctico:  -->
    
    <!-- ```{r} -->
    <!-- x <- 5 -->
      <!-- if(x > 0){ -->
          <!-- print("Positive number") -->
          <!-- } -->
      <!-- ``` -->
      
      <!-- --- -->
      <!-- background-image: url(Imagenes/RLogo.png) -->
      <!-- background-size: 100px -->
      <!-- background-position: 96% 3% -->
      
      <!-- ## 2. `if ... else` -->
      
      <!-- Hay ocasiones en que en caso de que no se cumpla una condición, queremos que otro tipo de acción sea realizada. En este caso utilizamos el operador de decisión `else` como se ve a continuación:  -->
      
      <!-- ```{r, eval = FALSE} -->
      <!-- if (expresion_de_prueba) { -->
          <!-- operaciones1 -->
          <!-- } else { -->
              <!-- operaciones2 -->
              <!-- } -->
      <!-- ``` -->
      
      <!-- Hay que recordar que la parte del `else` es opcional y solo se evalúa en caso de que la `expresion_de_prueba` sea falsa.  -->
      
      <!-- Igualmente, es importante notar que el `else` debe colocarse en la misma línea que las llaves que cierran el `if` y que inicial el `else`. -->
      
      <!-- --- -->
      <!-- background-image: url(Imagenes/RLogo.png) -->
      <!-- background-size: 100px -->
      <!-- background-position: 96% 3% -->
      
      <!-- ## 2. `if ... else` -->
      
      
      <!-- <p style='text-align:center;'><img src="Imagenes/ExpresionElse.png" width="65%"></p> -->
        
        <!-- --- -->
        <!-- background-image: url(Imagenes/RLogo.png) -->
        <!-- background-size: 100px -->
        <!-- background-position: 96% 3% -->
        
        <!-- ## 2. `if ... else` -->
        
        <!-- * Ejemplo práctico:  -->
        
        <!-- ```{r} -->
        <!-- x <- -5 -->
          <!-- if(x > 0){ -->
              <!-- print("Non-negative number") -->
              <!-- } else { -->
                  <!-- print("Negative number") -->
                  <!-- } -->
          <!-- ``` -->
          
          <!-- --- -->
          <!-- background-image: url(Imagenes/RLogo.png) -->
          <!-- background-size: 100px -->
          <!-- background-position: 96% 3% -->
          
          <!-- ## 3. `if ... else` &nbsp;&nbsp;  anidados.  -->
          
          <!-- En caso de existan más de dos alternativas, aplicamos el uso de `if-elses` anidados. La sintaxis de estos es la siguiente:  -->
          
          <!-- ```r -->
          <!-- if (expresion_prueba_1) { -->
              <!-- operaciones1 -->
              <!-- } else if (expresion_prueba_2) { -->
                  <!-- operaciones2 -->
                  <!-- } else if (expresion_prueba_3) { -->
                      <!-- operaciones3 -->
                      <!-- } else { -->
                          <!-- operaciones4 -->
                          <!-- } -->
          <!-- ``` -->
          <!-- --- -->
          <!-- background-image: url(Imagenes/RLogo.png) -->
          <!-- background-size: 100px -->
          <!-- background-position: 96% 3% -->
          
          <!-- ## 3. `if ... else` &nbsp;&nbsp;  anidados.  -->
          
          <!-- * Ejemplo práctico:  -->
          
          <!-- ```{r} -->
          <!-- x <- 0 -->
            <!-- if (x < 0) { -->
                <!-- print("Negative number") -->
                <!-- } else if (x > 0) { -->
                    <!-- print("Positive number") -->
                    <!-- } else { -->
                        <!-- print("Zero") -->
                        <!-- } -->
            <!-- ``` -->
            
            <!-- --- -->
            <!-- background-image: url(Imagenes/RLogo.png) -->
            <!-- background-size: 100px -->
            <!-- background-position: 96% 3% -->
            
            <!-- ## 4. `switch` -->
            
            <!-- Otra opcion ante **múltiples opciones** consiste en utilizar la función `switch()`, la cual toma como argumento una variable con múltiples valores posibles y las acciones a tomar para cada una de estas posibilidades.  -->
            
            <!-- * Ejemplo práctico:  -->
            
            <!-- ```{r} -->
            <!-- # Ejemplo, creando una funcion con switch -->
            <!-- centre <- function(x, type) { -->
                <!--   switch(type, -->
                                <!--          "mean" = mean(x), -->
                                <!--          "median" = median(x), -->
                                <!--          "trimmed" = mean(x, trim = .1)) -->
                <!-- } -->
              
              <!-- ``` -->
              
              <!-- --- -->
              <!-- background-image: url(Imagenes/RLogo.png) -->
              <!-- background-size: 100px -->
              <!-- background-position: 96% 3% -->
              
              <!-- ## 4. `switch` -->
              
              <!-- ```{r} -->
              <!-- # Calculamos la mediana -->
              <!-- centre(1:10, type = "median") -->
              
              <!-- # Calculamos la media -->
              <!-- centre(1:10, type = "mean") -->
              
              <!-- ``` -->
              
              <!-- .footnote[A pesar de que pudiera parecer muy práctica, ante un numero reducido de opciones se recomienda más anidar los `if...else`. -->
                               
                               <!-- Para mayor información sobre esta función, checar la ayuda de RStudio. -->
                               <!-- ] -->
              
              
              <!-- --- -->
              <!-- background-image: url(Imagenes/RLogo.png) -->
              <!-- background-size: 100px -->
              <!-- background-position: 96% 3% -->
              
              <!-- ## 5. Función `ifelse()`.  -->
              
              <!-- Esta función nos brinda una manera más sencilla de escribir una decisión en R. Toma 3 argumentos: `test`, el cual es la expresión de prueba, `yes`, lo que ocurre si esa prueba es verdadera, y `no`, lo que ocurre si esa prueba es falsa.  -->
              
              <!-- ```{r} -->
              <!-- # El ejemplo del if...else -->
              <!-- x <- -5 -->
                <!-- if(x > 0){ -->
                    <!-- print("Non-negative number") -->
                    <!-- } else { -->
                        <!-- print("Negative number") -->
                        <!-- } -->
                <!-- ``` -->
                
                <!-- --- -->
                <!-- background-image: url(Imagenes/RLogo.png) -->
                <!-- background-size: 100px -->
                <!-- background-position: 96% 3% -->
                
                <!-- ## 5. Función `ifelse()`.  -->
                
                <!-- ```{r} -->
                
                <!-- x <- -5 -->
                  <!-- if(x > 0){ -->
                      <!-- print("Non-negative number") -->
                      <!-- } else { -->
                          <!-- print("Negative number") -->
                          <!-- } -->
                  
                  <!-- # Traducido a ifelse -->
                  <!-- ifelse(test = (x > 0),  -->
                                <!--        yes =  print("Non-negative number"),  -->
                                <!--        no = print("Negative number")) -->
                  
                  <!-- ``` -->
                  
                  <!-- --- -->
                  <!-- background-image: url(Imagenes/RLogo.png) -->
                  <!-- background-size: 100px -->
                  <!-- background-position: 96% 3% -->
                  
                  <!-- ## 5. Función `ifelse()`.  -->
                  
                  <!-- En caso de que tengamos múltiples opciones, igual podemos anidar `ifelses`. -->
                  
                  <!-- ```{r} -->
                  <!-- # Ejemplo de los ifelses anidados -->
                  <!-- x <- 1 -->
                    <!-- if (x < 0) { -->
                        <!--   print("Negative number") -->
                        <!-- } else if (x > 0) { -->
                            <!--   print("Positive number") -->
                            <!-- } else print("Zero") -->
                    
                    <!-- # Traducido con ifelses() -->
                    <!-- ifelse(test = (x < 0),  -->
                                  <!--        yes = print("Negative number"),  -->
                                  <!--        no = ifelse(test = (x > 0),  -->
                                                            <!--                    yes = print("Positive number"), no = print("Zero") -->
                                                            <!--                    ) -->
                                  <!--        ) -->
                    <!-- ``` -->
                    
                    <!-- --- -->
                    <!-- background-image: url(Imagenes/RLogo.png) -->
                    <!-- background-size: 100px -->
                    <!-- background-position: 96% 3% -->
                    
                    <!-- ## 6. Funcion `dplyr::case_when()`.  -->
                    
                    <!-- Otra función, para los casos en que tenemos múltiples opciones y no queramos anidar `ifelse()`´s es la función `case_when()`.  -->
                    
                    <!-- * Ejemplo práctico:  -->
                    
                    <!-- ```{r} -->
                    <!-- x <- 0 -->
                      
                      <!-- # Ejemplo anterior con un case_when -->
                      <!-- dplyr::case_when(x > 0 ~ "Numero positivo",  -->
                                              <!--           x < 0 ~ "Numero negativo",  -->
                                              <!--           TRUE ~ "Es un cero!") -->
                      <!-- ``` -->
                      
                      <!-- _En el caso de Arriba, el TRUE actúa como un `else`._ -->
                      
                      <!-- Esta función es muy poderosa a la hora de recodificar variables, así que hay que tenerla en cuenta a la hora de limpiar bases de datos.  -->
                      
                      