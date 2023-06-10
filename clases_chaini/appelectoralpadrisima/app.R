# Librerias:
library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
# Metadatos:
meta <- readxl::read_xlsx("www/bases_de_datos/PREP/BDs/meta_nuevos.xlsx")
pn <- function(t, redondeo = 1) prettyNum(format(round(t, redondeo)), big.mark = ",")
# Datos:

shell_edomex <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson") %>%
  filter(CVE_ENT == "15") %>%
  select(NOM_MUN, CVEGEO)
  # st_read("www/bases_de_datos/shapefiles/15 MEXICO/MUNICIPIO.shp") %>% st_transform(crs = 4326)
shell_coahuila <- st_read("www/bases_de_datos/shapefiles/05 COAHUILA/MUNICIPIO.shp") %>% st_transform(crs = 4326)

secciones_coahuila <- readRDS("www/bases_de_datos/PREP/BDs/secciones_Coahuila.rds")  %>%
  mutate(candidato_ganador = case_when(candidato_ganador == "manolo" ~ "Manolo Jiménez",
                                       candidato_ganador == "guadiana" ~ "Armando Guadiana",
                                       candidato_ganador == "mejia" ~ "Mejía Berdeja",
                                       candidato_ganador == "lenin" ~ "Lenin Pérez")) %>%
  rename(partido_ganador_coah = partido_ganador,
         candidato_ganador_coah = candidato_ganador,
         NOM_MUN = MUNICIPIO,
         CVEGEO = CVE_INE,
         SECCION = seccion) %>%
  mutate(abstencion = round(abstencion, 2)) %>%
  mutate(participacion = round(participacion, 2)) %>%
  mutate(abstencion = case_when(abstencion > 100 ~ 100,
                                abstencion < 0 ~ 0,
                                TRUE ~ abstencion
                                )) %>%
  mutate(participacion = case_when(participacion > 100 ~ 100,
                                   participacion < 0 ~ 0,
                                TRUE ~ participacion)) %>%
  mutate(abstencion_categoria = case_when(between(abstencion, 0, 20) ~  "Entre 0 y 20%",
                                          between(abstencion, 20, 40) ~ "Entre 20 y 40%",
                                          between(abstencion, 40, 50) ~ "Entre 40 y 50%",
                                          between(abstencion, 50, 60) ~ "Entre 50 y 60%",
                                          abstencion > 60 ~ "Mayor al 60%"))

secciones_edomex <- readRDS("www/bases_de_datos/PREP/BDs/secciones_Edomex.rds") %>%
  mutate(candidato_ganador = ifelse(candidato_ganador == "alejandra",
                                    "Alejandra del Moral", "Delfina Gómez")) %>%
  rename(partido_ganador_edomex = partido_ganador,
         candidata_ganadora_edomex = candidato_ganador) %>%
  mutate(abstencion = round(abstencion, 2))  %>%
  mutate(participacion = round(participacion, 2)) %>%
  mutate(abstencion = case_when(abstencion > 100 ~ 100,
                                abstencion < 0 ~ 0,
                                TRUE ~ abstencion
  )) %>%
  mutate(participacion = case_when(participacion > 100 ~ 100,
                                   participacion < 0 ~ 0,
                                   TRUE ~ participacion)) %>%
  mutate(abstencion_categoria = case_when(between(abstencion, 0, 20) ~ "Entre 0 y 20%",
                                          between(abstencion, 20, 40) ~ "Entre 20 y 40%",
                                          between(abstencion, 40, 50) ~ "Entre 40 y 50%",
                                          between(abstencion, 50, 60) ~ "Entre 50 y 60%",
                                          abstencion > 60 ~ "Mayor al 60%"))

bd_grafica_secciones_edomex <- secciones_edomex  %>% as_tibble() %>% select(-geometry)


mpios_coahuila <- readRDS("www/bases_de_datos/PREP/BDs/mpios_Coahuila.rds") %>%
  mutate(candidato_ganador = case_when(candidato_ganador == "manolo" ~ "Manolo Jiménez",
                                       candidato_ganador == "guadiana" ~ "Armando Guadiana",
                                       candidato_ganador == "mejia" ~ "Mejía Berdeja",
                                       candidato_ganador == "lenin" ~ "Lenin Pérez")) %>%
  rename(partido_ganador_coah = partido_ganador,
         candidato_ganador_coah = candidato_ganador) %>%
  mutate(abstencion = round(abstencion, 2))  %>%
  mutate(participacion = round(participacion, 2)) %>%
  mutate(abstencion = case_when(abstencion > 100 ~ 100,
                                abstencion < 0 ~ 0,
                                TRUE ~ abstencion
  )) %>%
  mutate(participacion = case_when(participacion > 100 ~ 100,
                                   participacion < 0 ~ 0,
                                   TRUE ~ participacion)) %>%
  mutate(abstencion_categoria = case_when(between(abstencion, 0, 20) ~ "Entre 0 y 20%",
                                          between(abstencion, 20, 40) ~ "Entre 20 y 40%",
                                          between(abstencion, 40, 50) ~ "Entre 40 y 50%",
                                          between(abstencion, 50, 60) ~ "Entre 50 y 60%",
                                          abstencion > 60 ~ "Mayor al 60%"))

mpios_edomex <- readRDS("www/bases_de_datos/PREP/BDs/mpios_Edomex.rds") %>%
  mutate(candidato_ganador = ifelse(candidato_ganador == "alejandra",
                                    "Alejandra del Moral", "Delfina Gómez")) %>%
  rename(partido_ganador_edomex = partido_ganador,
         candidata_ganadora_edomex = candidato_ganador)  %>%
  mutate(abstencion = round(abstencion, 2))  %>%
  mutate(participacion = round(participacion, 2)) %>%
  mutate(abstencion = case_when(abstencion > 100 ~ 100,
                                abstencion < 0 ~ 0,
                                TRUE ~ abstencion
  )) %>%
  mutate(participacion = case_when(participacion > 100 ~ 100,
                                   participacion < 0 ~ 0,
                                   TRUE ~ participacion)) %>%
  mutate(abstencion_categoria = case_when(between(abstencion, 0, 20) ~ "Entre 0 y 20%",
                                          between(abstencion, 20, 40) ~ "Entre 20 y 40%",
                                          between(abstencion, 40, 50) ~ "Entre 40 y 50%",
                                          between(abstencion, 50, 60) ~ "Entre 50 y 60%",
                                          abstencion > 60 ~ "Mayor al 60%"))

# Funcion generadora de mapa:
opciones_var <- meta$variables
opciones_agregacion <- c("Municipios", "Secciones")
opciones_estado <- c("Coahuila", "Estado de México")

edo_sel <- "Coahuila"
agg_sel <- "Secciones"
var_sel <- "partido_ganador_coah"

gen_mapa <- function(edo_sel,
                     agg_sel,
                     var_sel){

  meta_sel <- meta %>%
    filter(variables == var_sel)

  # Seleccionando la base
  if(edo_sel == "Coahuila" & agg_sel == "Municipios" ) bd <- mpios_coahuila
  if(edo_sel == "Estado de México" & agg_sel == "Municipios" ) bd <- mpios_edomex
  if(edo_sel == "Coahuila" & agg_sel == "Secciones") bd <- secciones_coahuila
  if(edo_sel == "Estado de México" & agg_sel == "Secciones") bd <- secciones_edomex

  if(edo_sel == "Coahuila" & agg_sel == "Secciones") {shp_muni <- shell_coahuila}
  if(edo_sel == "Estado de México" & agg_sel == "Secciones") {shp_muni <- shell_edomex}

  # bd$partido_ganador_edomex
  # Variable de interes
  bd$var_interes <- bd[,var_sel] %>%
    as_tibble() %>%
    select(-geometry) %>%
    pull()

  # Paleta de colores:
  if(meta_sel$tipo_dato == "numeric"){
    inicio = (str_split(meta_sel$categorias, ",") %>% unlist() %>% as.numeric())[1]
    fin = (str_split(meta_sel$categorias, ",") %>% unlist() %>% as.numeric())[2]

    pal_color <- colorNumeric(domain = inicio:fin,
                              palette = str_split(meta_sel$pal_color, ",") %>% unlist())
    # bd$var_interes <- round(var_interes, 2)

  } else {
    pal_color <- colorFactor(domain = factor(str_split(meta_sel$categorias, ",|;") %>% unlist(),
                                             levels = str_split(meta_sel$categorias, ",|;") %>% unlist() %>% str_squish()),
                             palette = str_split(meta_sel$pal_color, ",") %>% unlist())
    bd$var_interes <- factor(bd$var_interes, levels = str_split(meta_sel$categorias, ",|;") %>% unlist() %>% str_squish())
  }


  titulo <- meta_sel$nombre_variable

  if(agg_sel == "Municipios"){
    popup = str_c("<b>Municipio: </b>", bd$NOM_MUN %>% str_to_title(), "<br>",
                  "<b>", meta_sel$nombre_variable, ": </b>", bd$var_interes)
    label <- lapply(str_c("<b>Municipio: </b>", bd$NOM_MUN %>% str_to_title(), "<br>"),
                    htmltools::HTML)
    id_layer = bd$CVEGEO
    # layerId = datos_mapa$id_seccion,
  } else {
    id_layer = bd$SECCION
    popup = str_c("<b>Municipio: </b>", bd$NOM_MUN %>% str_to_title(), "<br>",
                  "<b>", meta_sel$nombre_variable, ": </b>", bd$var_interes)
    label <- lapply(str_c("<b>Municipio: </b>", bd$NOM_MUN %>% str_to_title(), "<br>",
                          "<b>Sección: </b>", bd$SECCION),
                    htmltools::HTML)
  }

  if(edo_sel == "Estado de México"){
    popup = str_c(popup, "<br>",
                  "<b style = 'color:#c90889;'>Votos Ale del Moral: </b>", bd$total_votos_alejandra %>% pn(), "<br>",
                  "<b style = 'color:brown;'>Votos Delfina Gómez: </b>", bd$total_votos_delfina %>% pn(), "<br>",
                  "<b style = 'color:red;'>% Abstención: </b>", pn(bd$abstencion), "%")

  } else {
    popup = str_c(popup, "<br>",
                  "<b style = 'color:#048a5b;'>Votos Manolo Jiménez: </b>", bd$total_votos_manolo %>% pn(), "<br>",
                  "<b style = 'color:brown;'>Votos Armando Guadiana: </b>", bd$total_votos_guadiana %>% pn(), "<br>",
                  "<b style = 'color:orange;'>Votos Lenin Pérez: </b>", bd$total_votos_lenin %>% pn(), "<br>",
                  "<b style = 'color:red;'>Votos Ricardo Mejía Berdeja: </b>", bd$total_votos_mejia %>% pn(), "<br>",
                  "<b style = 'color:red;'>% Abstención: </b>", pn(bd$abstencion), "%")
  }


  mapa_final <- bd %>%
    leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = pal_color(bd$var_interes),
                fillOpacity = 1,
                color = "white",
                weight = 1,
                popup = popup,
                layerId = id_layer,
                label = label) %>%
    addLegend(pal = pal_color,
              title = titulo,
              values = bd$var_interes,
              position = "bottomright")

  if(agg_sel == "Secciones"){
  mapa_final <- mapa_final %>%
    addPolygons(data = shp_muni,
                fill = NA,
                fillOpacity = 0,
                weight = 2,
                color = "white",
                opacity = 1
                )
  }
  return(mapa_final)
}
# pol_sel = c
# Gen Gráfica:
# gen_grafica(edo_sel = "Coahuila",
#             agg_sel = "Secciones",
#             pol_sel = 1)

gen_grafica <- function(edo_sel = "Coahuila",
                     agg_sel = "Municipios",
                     pol_sel = "05001"){

  # Seleccionando la base
  if(edo_sel == "Coahuila" & agg_sel == "Municipios" ){
    bd <- mpios_coahuila %>% as_tibble() %>% select(-geometry)
    partidos = c("PAN","PRI","PRD","UDC","PVEM","MORENA","PT", "NO_VOTOS_ABSTENCION")
    ids = c("CVEGEO", "NOM_MUN")
  }
  if(edo_sel == "Estado de México" & agg_sel == "Municipios" ){
    bd <- mpios_edomex  %>% as_tibble() %>% select(-geometry)
    partidos = c("PAN","PRI","PRD","NAEM","JHH", "NO_VOTOS_ABSTENCION")
    ids = c("CVEGEO", "NOM_MUN")
    }
  if(edo_sel == "Coahuila" & agg_sel == "Secciones"){
      bd <- secciones_coahuila  %>% as_tibble() %>% select(-geometry)
      partidos = c("PAN","PRI","PRD","UDC","PVEM","MORENA","PT", "NO_VOTOS_ABSTENCION")
      ids = c("CVEGEO", "SECCION", "NOM_MUN")
      }
  if(edo_sel == "Estado de México" & agg_sel == "Secciones"){
    bd <- bd_grafica_secciones_edomex
    partidos =  c("PAN","PRI","PRD","NAEM","JHH", "NO_VOTOS_ABSTENCION")
    ids = c("CVEGEO", "SECCION", "NOM_MUN")
    }

  bd_plot <- bd %>%
    select_at(c(ids, partidos)) %>%
    pivot_longer(partidos) %>%
    mutate(name = ifelse(name == "NO_VOTOS_ABSTENCION",
                         yes = "ABST",
                         no = name)) %>%
    {if(agg_sel == "Municipios") filter(., CVEGEO == pol_sel) else .} %>%
    {if(agg_sel == "Secciones") filter(., SECCION == pol_sel) else .}

  if(nrow(bd_plot) == 0){
    titulo = "Seleccione un municipio o sección"
  } else {
    titulo <- ifelse(agg_sel == "Secciones",
                   yes = str_c(bd_plot$CVEGEO[1], " ", bd_plot$NOM_MUN[1], "\n", "Sección: ", pol_sel),
                   no = str_c(bd_plot$CVEGEO[1], " ", bd_plot$NOM_MUN[1]))
  }

  bd_plot %>%
    ggplot(aes(x = reorder(name, -value),
               y = value, fill = name, color = name)) +
    geom_col() +
    geom_text(aes(label = prettyNum(round(value, 0), big.mark = ",")),
              vjust = -0.5,
              family = "Poppins",
              fontface = "bold") +
    scale_fill_manual(values = c("PAN" = "blue",
                                 "PRI" = "#048a5b",
                                 "PRD" = "#f0bd05",
                                 "MORENA" = "brown",
                                 "JHH" = "brown",
                                 "NAEM" = "turquoise",
                                 "UDC" = "orange",
                                 "PVEM" = "green",
                                 "NO_VOTOS_ABSTENCION" = "gray20",
                                 "PT" = "red")) +
    scale_color_manual(values = c("PAN" = "blue",
                                 "PRI" = "#048a5b",
                                 "PRD" = "#f0bd05",
                                 "MORENA" = "brown",
                                 "JHH" = "brown",
                                 "NAEM" = "turquoise",
                                 "UDC" = "orange",
                                 "PVEM" = "green",
                                 "NO_VOTOS_ABSTENCION" = "gray20",
                                 "PT" = "red")) +
    scale_y_continuous(expand = expansion(c(0, 0.3))) +
    labs(x = NULL, y = NULL, title = titulo, subtitle = "Total de votos") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          plot.title = element_text(face = "bold"),
          panel.border = element_blank(),
          axis.ticks =  element_blank(),
          legend.position = "none",
          axis.text.x = element_text(face = "bold")
          )
}


# Opciones.
opciones_var <- meta$variables
opciones_agregacion <- c("Municipios", "Secciones")
opciones_estado <- c("Coahuila", "Estado de México")

library(shiny)

ui <- navbarPage("Análisis de datos electorales",
                 id = "nav", # must give id here to add/remove tabs in server
                 collapsible = FALSE,

                 tabPanel(tagList(icon("map"), "Mapa de variables"),
                          div(class="outer",

                              # Header logo (no mover!!)
                              tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"https://github.com/JuveCampos/Repositorio-de-Mapas-y-Visualizaciones/raw/main/resources/logos/Mexico%20Big%20Data/MEXICO_BIGDATA_LOGOTIPO_HORIZONTAL_ESLOGAN-01.png\" alt=\"alt\" style=\"float:right;width:100px;height:42px;padding-top:10px;\"> </a>`</div>');
    console.log(header)")
                              ),

tags$head(
  HTML('<meta charset = "utf-8">'),
  HTML('<link rel="SHORTCUT ICON" href="https://github.com/JuveCampos/Repositorio-de-Mapas-y-Visualizaciones/raw/main/resources/logos/Mexico%20Big%20Data/mexicobigdata.ico">'),
  # Include our custom CSS
  includeCSS("styles.css"),
  includeScript("gomap.js")
),

leafletOutput(outputId = "mapa", width="100%", height="100%"),
# Panel de controles:
absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, top = 60, right = "auto", left = 25, bottom = "auto",
              width = 450, height = "auto",
              h2("Análisis de resultados electorales, 2023"),
              p("En esta aplicación, podrá consultar, de manera interactiva, los resultados de abstencionismo (y otras variables electorales de interés) derivados del pasado proceso electoral de 2023 para la elección a gobernador en el Estado de México y Coahuila"),
              tabsetPanel(
                tabPanel(title = "Controles",
                         p("Seleccione una entidad para visualizar el resultado"),
                         br(),
                         fluidRow(
                           column(6, radioButtons("selEstado",
                                                  "Seleccione Estado",
                                                  choices = opciones_estado
                           )),
                           column(6, radioButtons("selAgg",
                                                  "Seleccione Agregación",
                                                  choices = opciones_agregacion)),
                         ),
                         uiOutput("selVar")
                         ,
                         downloadButton("descarga_mapa", "Descargar mapa")
                         ),
                tabPanel(title = "Gráficas"
                         ,
                         plotOutput("grafica", height = "30vh")

                         )
              )
),
tags$div(id="cite", tags$br(),
         'Análisis proveniente de Mexico Bigdata ', tags$em('Todos los derechos reservados'), ' (2023). '
)
                          )
                 )

)


server <- function(input, output, session) {

  mapa <- reactive({
    gen_mapa(edo_sel = input$selEstado,
             agg_sel = input$selAgg,
             var_sel = req(input$selVar))
  })

  output$descarga_mapa <- downloadHandler(
    filename = function(){
       str_c(str_replace_all(input$selEstado,
                             pattern = " ",
                             replacement = "_"),
             str_replace_all(input$selVar,
                             pattern = " ",
                             replacement = "_"),
             str_replace_all(input$selAgg,
                             pattern = " ",
                             replacement = "_")
             , ".html", sep = "_")
    },
    content = function(file){
      showModal(modalDialog("Generando archivo", footer=NULL))
      on.exit(removeModal())
      if(input$selEstado == "Estado de México"  & input$selAgg == "Secciones"){
        file.copy(str_c("www/mapas_secciones_edomex_",
                        input$selVar,
                        ".html"), file)
      } else if(input$selEstado == "Coahuila"  & input$selAgg == "Secciones"){
        file.copy(str_c("www/mapas_secciones_coahuila_",
                        input$selVar,
                        ".html"), file)
      } else {
        htmlwidgets::saveWidget(mapa(), file)
      }
    }
  )

  output$mapa <- renderLeaflet({
    showModal(modalDialog("Generando mapa", footer=NULL))
    on.exit(removeModal())
    mapa()
  })

  output$selVar <- renderUI({
    opciones_var <- meta$variables[str_detect(meta$estado, input$selEstado)]
    names(opciones_var) <- meta$nombre_variable[str_detect(meta$estado, input$selEstado)]
    selectizeInput("selVar",
                   "Seleccione Variable",
                   choices = opciones_var)
  })

  observe({
    click <<- input$mapa_shape_click$id
    # print(click)
    if(is.null(click)) click <<- "05001"

    output$grafica <- renderPlot({
      gen_grafica(edo_sel = input$selEstado,
                  agg_sel = input$selAgg,
                  pol_sel = click)
    })
  })
}

shinyApp(ui, server)
