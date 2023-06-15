# 1. Librerias. ----
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(sf)
library(leaflet)
library(shinycssloaders)
library(shinyWidgets)
library(ggrepel)
library(readxl)

# 2. Cargar la información: ----
mcv_discrete <- c("#6950D8", "#3CEAFA", "#00B783", "#FF6260", "#FFAF84", "#FFBD41")
catalogo_entidades <- readRDS("www/catalogo_entidades.rds")
shp <- readRDS("www/shp.rds")
meta <- readxl::read_xlsx("www/meta.xlsx")
ips <- readRDS("www/ips.rds")
hojas <- readxl::excel_sheets("www/00_IPS_bd.xlsx")
hojas <- hojas[-c(1,2)]
indicadores <- lapply(hojas, function(hoja){
  readxl::read_xlsx("www/00_IPS_bd.xlsx", 
                    sheet = hoja) %>% 
    select(cve_ent,entidad_abr_m, anio,
           id_dimension, id_indicador, indicador_value)
}) %>% 
  do.call(rbind, .) %>% 
  mutate(anio = as.numeric(anio), 
         indicador_value = as.numeric(indicador_value))

# 3. 
datos_dimension_componente <- readxl::read_xlsx("www/02_CATALOGO_SECCIONES.xlsx")


opciones_dimension <- datos_dimension_componente$id_dimension %>% unique()
names(opciones_dimension) <- str_c(datos_dimension_componente$id_dimension %>% unique(), ". ", 
                                   datos_dimension_componente$dimension %>% unique())

# gen_opciones_componente("01")
gen_opciones_componente <- function(dimension_sel){
  opciones_componente <- datos_dimension_componente %>% 
    filter(id_dimension == dimension_sel) %>% 
    pull(id_componente) %>% 
    unique()
    
  names(opciones_componente) <- datos_dimension_componente %>% 
    filter(id_dimension == dimension_sel) %>% 
    pull(componente) %>% 
    unique()
  return(opciones_componente)
}



opciones_estado <- catalogo_entidades$cve_ent
names(opciones_estado) <- catalogo_entidades$entidad




gen_opciones_indicador <- function(componente_sel){
  opciones <- meta %>% 
    select(id_dimension, id_componente, id_indicador) %>% 
    unique() %>% 
    filter(id_componente == componente_sel) %>% 
    pull(id_indicador)
  nombres <- meta %>% 
    filter(id_componente == componente_sel) %>% 
    pull(indicador)
  names(opciones) <- nombres
  return(opciones)
}

gen_opciones_anio <- function(indicador_sel){
  indicadores %>% 
    filter(id_indicador == indicador_sel) %>% 
    pull(anio) %>% 
    unique() %>% 
    as.numeric() %>% 
    sort()
}


# Gen mapa indicadores ----
# indicador_sel = "01"
# anio_sel = 2008
gen_mapa_indicadores <- function(anio_sel,
                                 indicador_sel){
  
  meta_sel <- meta %>%  
    filter(id_indicador == indicador_sel) %>% 
    mutate(unidad = ifelse(is.na(unidad), yes = "", no = unidad))
  
  mapa <- left_join(shp, 
            indicadores %>% 
              filter(id_indicador == indicador_sel) %>% 
              filter(anio == anio_sel))
  
  pal <- colorNumeric(
    domain = mapa$indicador_value, 
    palette = meta_sel$paleta
  )
  
  mapa %>% 
    leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(color = "black", 
                fillColor = pal(mapa$indicador_value),
                weight = 1, 
                opacity = 1, 
                fillOpacity = 1
                ) %>% 
    addLegend(pal = pal, values = mapa$indicador_value, 
              title = meta_sel$indicador %>% 
                str_wrap(10) %>%
                str_replace_all(pattern = "\n", replacement = "<br>"),
              position = "bottomright", 
              labFormat = labelFormat(suffix = meta_sel$unidad)
              # ,
              # labels = labelOptions(suffix = meta_sel$unidad)
              )
  
}


# Funciones de visualizacion: ----

# 1. Evo del IPS Estado ----
gen_grafica_evo_ips <- function(edo_sel){
  
  ips_sel <- ips %>% 
    filter(cve_ent %in% edo_sel)
  
  edo_sel_nombre <- ips %>% 
    filter(cve_ent %in% edo_sel) %>% 
    pull(entidad) %>% 
    unique()
  
  # Evolución de los valores del IPS: 
  g <- ips_sel %>%   
    ggplot(aes(x = anio,
               y = ips, 
               group = entidad_abr_m, 
               color = entidad_abr_m)) + 
    geom_line(linewidth = 2) + 
    geom_text_repel(aes(label = format(round(ips, 1), nsmall = 1)), 
                    vjust = -1, 
                    direction = "y",
                    size = 8,
                    family = "Ubuntu",
                    fontface = "bold",
                    show.legend = F, 
                    alpha = 0.8) +
    geom_text(data = ips_sel %>% 
                filter(anio == max(anio)), 
              aes(label = ifelse(entidad_abr_m %in% edo_sel, 
                                 yes = str_replace_all(entidad_abr_m, edo_sel_nombre_x), 
                                 no = ""),
                  x = anio + .4), 
              hjust = 0.1,
              # direction = "y",
              family = "Ubuntu",
              fontface = "bold",
              size = 8,
              show.legend = F) +
    geom_point(pch = 21, 
               size = 3, 
               stroke = 2,
               fill = "white") + 
    labs(x = NULL, y = NULL, 
         title = "Índice de Progreso Social 2015 - 2021", 
         subtitle = str_c("Entidades: ", str_c(edo_sel_nombre, collapse = " - ")), 
         color = "Entidad:") + 
    scale_y_continuous(expand = expansion(c(0.2, 0.2))) + 
    scale_x_continuous(breaks = unique(ips_sel$anio), 
                       expand = expansion(c(0.05, 0.3))) + 
    scale_color_manual(values = mcv_discrete[c(1,3,4,5,6,2)]) + 
    theme_minimal() +
    theme(plot.title    = element_text(size = 35, face = "bold", colour = "#6950D8"),
          plot.subtitle     = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.margin       = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          plot.caption      = element_text(size = 20),
          strip.text.x      = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          panel.background  = element_rect(fill = "transparent",colour = NA),
          text              = element_text(family = "Ubuntu"),
          axis.title.x      = element_blank(),
          axis.title.y      = element_text(size = 25),
          axis.text.x       = element_text(size = 25, angle = 90),
          axis.text.y       = element_text(size = 25),
          legend.text       = element_text(size = 25),
          legend.title       = element_text(size = 25),
          legend.position   = "top") 
  g
}

# 2. Gráfica de cambio de lugar ----
gen_grafica_ranking <- function(edo_sel){
  
  edo_sel_nombre_x <- catalogo_entidades %>% 
    filter(cve_ent %in% edo_sel) %>% 
    pull(entidad)
  names(edo_sel_nombre_x) <- catalogo_entidades %>% 
    filter(cve_ent %in% edo_sel) %>% 
    pull(entidad_abr_m)
  
  ips_sel <- ips %>% 
    filter(entidad != "Nacional") %>% 
    mutate(ips_rank_nacional = rank(-ips)) %>% 
    mutate(lugar_rev = 33 - ips_rank_nacional) %>% 
    mutate(ent_interes = ifelse(cve_ent %in% edo_sel, 
                                yes = str_replace_all(entidad_abr_m, edo_sel_nombre_x), 
                                no = "")) %>% 
    mutate(ent_interes_etiqueta = ifelse(cve_ent %in% edo_sel, 
                                         yes = str_replace_all(entidad_abr_m, edo_sel_nombre_x), 
                                         no = entidad_abr_m)) %>% 
    ungroup()
  
  # Evolución en los lugares del IPS: 
  g <-
    ips_sel %>%   
    ggplot(aes(x = anio, 
               y = lugar_rev, 
               group = entidad_abr_m, 
               color = ent_interes)) + 
    geom_line(aes(linewidth = ent_interes)) + 
    scale_linewidth_manual(values = c(0.5, rep(2.5, length(edo_sel)))) +
    geom_text(data = ips_sel %>% filter(anio == min(anio)) %>% filter(ent_interes == ""),
              aes(x = 2014.6, 
                  label = ent_interes_etiqueta, 
                  family = "Ubuntu", 
                  size = 12), 
              alpha = 0.8,
              hjust = 1) +
    geom_text(data = ips_sel %>% filter(anio == max(anio)) %>% filter(ent_interes == ""),
              aes(x = 2021.4, 
                  label = ent_interes_etiqueta, 
                  family = "Ubuntu", 
                  size = 12), 
              alpha = 0.8,
              hjust = 0) +
    geom_text(data = ips_sel %>% filter(anio == min(anio)) %>% filter(ent_interes != ""),
              aes(x = 2014.6, 
                  label = ent_interes_etiqueta, 
                  family = "Ubuntu", 
                  fontface = "bold",
                  size = 12), 
              hjust = 1) +
    geom_text(data = ips_sel %>% filter(anio == max(anio)) %>% filter(ent_interes != ""),
              aes(x = 2021.4, 
                  label = ent_interes_etiqueta, 
                  family = "Ubuntu", 
                  fontface = "bold",
                  size = 12), 
              hjust = 0) +
    geom_label(data = ips_sel %>% filter(ent_interes == ""),
               aes(label = ips_rank_nacional)) +
    geom_label(data = ips_sel %>% filter(ent_interes != ""),
               aes(label = ips_rank_nacional), 
               fontface = "bold") +
    scale_x_continuous(breaks = unique(ips_sel$anio), 
                       expand = expansion(c(0.25, 0.25))) + 
    scale_color_manual(values = c("gray50",
                                  mcv_discrete[c(1,3,4,5,6,2)]
    )) + 
    labs(x = NULL, y = NULL, 
         title = "Lugar en el Índice de Progreso Social 2015 - 2021"
         # , 
         # subtitle = "Destacando al estado de Coahuila"
    ) + 
    theme_minimal() +
    theme(plot.title    = element_text(size = 25, face = "bold", colour = "#6950D8"),
          plot.subtitle     = element_text(size = 20, colour = "#777777", margin=margin(0,0,30,0)),
          plot.margin       = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          plot.caption      = element_text(size = 20),
          strip.text.x      = element_text(size = 15),
          panel.grid  = element_blank(),
          panel.background  = element_rect(fill = "transparent",colour = NA),
          text              = element_text(family = "Ubuntu"),
          axis.title.x      = element_blank(),
          axis.title.y      = element_blank(),
          axis.text.x       = element_text(size = 15, angle = 90),
          axis.text.y       = element_blank(),
          legend.text       = element_text(size = 25),
          legend.title       = element_text(size = 25),
          legend.position   = "none") 
  g
}


# 3. Gráfica de evolución del indicador ----
# indicador_sel = "01"
# edo_sel = "01"
# gen_grafica_evo_indicador("01", "05")
gen_grafica_evo_indicador <- function(indicador_sel, edo_sel){
  
  ind_sel <- indicadores %>% 
    filter(id_indicador %in% indicador_sel) %>% 
    filter(cve_ent %in% edo_sel) %>% 
    mutate(indicador_value = as.numeric(indicador_value), 
           anio = as.numeric(anio))
  
  edo_sel_nombre <- ips %>% 
    filter(cve_ent %in% edo_sel) %>% 
    pull(entidad) %>% 
    unique()
  
  meta_sel <- meta %>% 
    filter(id_indicador %in% indicador_sel)
  
  # Evolución de los valores del IPS: 
  ind_sel %>%   
    ggplot(aes(x = anio,
               y = indicador_value, 
               group = entidad_abr_m, 
               color = entidad_abr_m)) + 
    geom_line(linewidth = 2) + 
    geom_text_repel(aes(label = format(round(indicador_value, 1), nsmall = 1)), 
                    vjust = -1, 
                    direction = "y",
                    size = 8,
                    family = "Ubuntu",
                    fontface = "bold",
                    show.legend = F, 
                    alpha = 0.8) +
    geom_text(data = ind_sel %>% 
                filter(anio == max(anio)), 
              aes(label = ifelse(entidad_abr_m %in% edo_sel, 
                                 yes = str_replace_all(entidad_abr_m, edo_sel_nombre_x), 
                                 no = ""),
                  x = anio + .4), 
              hjust = 0.1,
              # direction = "y",
              family = "Ubuntu",
              fontface = "bold",
              size = 8,
              show.legend = F) +
    geom_point(pch = 21, 
               size = 3, 
               stroke = 2,
               fill = "white") +
    labs(x = NULL, y = NULL, 
         title = meta_sel$indicador[1], 
         subtitle = str_c("Entidades: ", str_c(edo_sel_nombre, collapse = " - ")), 
         color = "Entidad:") +
    scale_y_continuous(expand = expansion(c(0.2, 0.2))) +
    scale_x_continuous(breaks = unique(ind_sel$anio), 
                       expand = expansion(c(0.05, 0.3))) +  
    scale_color_manual(values = mcv_discrete[c(1,3,4,5,6,2)]) +
    theme_minimal() +
    theme(plot.title    = element_text(size = 35, face = "bold", colour = "#6950D8"),
          plot.subtitle     = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.margin       = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          plot.caption      = element_text(size = 20),
          strip.text.x      = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          panel.background  = element_rect(fill = "transparent",colour = NA),
          text              = element_text(family = "Ubuntu"),
          axis.title.x      = element_blank(),
          axis.title.y      = element_text(size = 25),
          axis.text.x       = element_text(size = 25, angle = 90),
          axis.text.y       = element_text(size = 25),
          legend.text       = element_text(size = 25),
          legend.title       = element_text(size = 25),
          legend.position   = "top") 
}

# 4. Mapa del indicador ----
# year_sel = 2015
# indicador_sel = "05"

gen_grafica_mapa_ips <- function(year_sel){
  ind_sel_data <- ips %>% 
    # filter(id_indicador == indicador_sel) %>%
    filter(anio == year_sel) %>% 
    mutate(anio = as.numeric(anio), 
           indicador_value = ips)
  
  # meta_sel <- meta %>% 
  #   filter(id_indicador == indicador_sel)
  
  pal <- colorNumeric(
    palette = c("red", "yellow","green"), 
    domain = ind_sel_data$indicador_value
  )
  
  left_join(shp, ind_sel_data) %>% 
    leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(fillColor = ~pal(indicador_value), 
                fillOpacity = 1, 
                color = "black", 
                weight = 1) %>% 
    addLegend(pal = pal, values = ~indicador_value, 
              title = meta_sel$indicador %>% 
                str_wrap(10) %>% 
                str_replace_all(pattern = "\n", replacement = "<br>"))
}

# 5. Mapa indicadores ----
gen_grafica_mapa_indicador <- function(year_sel, indicador_sel){
  ind_sel_data <- indicadores %>% 
    filter(id_indicador == indicador_sel) %>%
    filter(anio == year_sel) %>% 
    mutate(anio = as.numeric(anio), 
           indicador_value = as.numeric(indicador_value))
  
  meta_sel <- meta %>% 
    filter(id_indicador == indicador_sel)
  
  pal <- colorNumeric(
    palette = "YlOrRd", 
    domain = ind_sel_data$indicador_value
  )
  
  left_join(shp, ind_sel_data) %>% 
    leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(fillColor = ~pal(indicador_value), 
                fillOpacity = 1, 
                color = "white", 
                weight = 1) %>% 
    addLegend(pal = pal, values = ~indicador_value, 
              title = meta_sel$indicador %>% 
                str_wrap(10) %>% 
                str_replace_all(pattern = "\n", replacement = "<br>"))
}

# Shiny ----
library(shiny)

header <- dashboardHeader(title = "IPS", 
                          titleWidth = "250px", 
                          tags$li(a(href = 'https://mexicocomovamos.mx/home',
                                    img(src = 'https://mexicocomovamos.mx/wp-content/uploads/2022/06/logo-mexico-como-vamos.svg',
                                        title = "mcv", height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown"), 
                          tags$li(a(href = 'https://mexicocomovamos.mx/home',
                                    img(src = 'https://mexicocomovamos.mx/wp-content/uploads/2022/06/logo-mexico-como-vamos.svg',
                                        title = "mcv", height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown")
                          )

sidebar <- dashboardSidebar(
  width = "250px",
  sidebarMenu(
    menuItem("Índice de progreso social", tabName = "tabAnalisis", icon = icon("star")), 
    menuItem("Indicadores", tabName = "tabIndicadores",icon = icon("star"))
  )
)

body <- dashboardBody(
  tags$head(
    includeCSS("estilos.css")
  ), 
  
  tabItems(
    tabItem("tabIndicadores", h1("Indicadores")), 
    tabItem("tabAnalisis", 
            h1("Analisis"), 
            fluidRow(
              column(3, 
                     selectizeInput(inputId = "selDimension", 
                                    "Dimensión", 
                                    choices = opciones_dimension), 
                     uiOutput("selComponente"), 
                     uiOutput("selIndicador"), 
                     uiOutput("selAnio"), 
                     selectizeInput("selEdo", 
                                    "Estado", 
                                    choices = opciones_estado, 
                                    selected = "00",
                                    multiple = T)
                     ), 
              column(9, 
                     h1("Mapa de indicadores"), 
                     p("El Índice de Progreso Social utiliza una variedad de indicadores para evaluar el bienestar y el progreso de una sociedad. Estos indicadores se agrupan en tres dimensiones principales: Necesidades Humanas Básicas, Fundamentos del Bienestar y Oportunidades. Algunos de los indicadores utilizados en estas dimensiones incluyen el acceso a agua potable y saneamiento, la nutrición, la esperanza de vida, la mortalidad infantil, la seguridad personal, la educación básica, el acceso a tecnología, la calidad del medio ambiente, la inclusión social, el acceso a derechos políticos, la equidad de género y la movilidad social. Estos indicadores permiten obtener una visión más completa del desarrollo de una sociedad, más allá del enfoque tradicional basado únicamente en el crecimiento económico."),
                     
                     box(
                       title = "Mapa de indicadores", 
                       footer = "Elaboración propia con datos de MCV, 2022", 
                       status = "success", 
                       solidHeader = T, 
                       width = 12, 
                       leafletOutput("mapa_indicadores") %>% withSpinner()
                     ),
                      br(), br(),
                     
                     h1("Evolución del indicador"), 
                     p(em("A continuación")," se puede visualizar la evolución de cada uno de los 53 indicadores para las 32 entidades federativas y a nivel nacional. Esta herramienta permitirá visualizar, de manera comparativa, la evolución de las entidades seleccionadas en uno de los indicadores de interés. "),
                     box(
                       title = "Mapa de indicadores", 
                       footer = "Elaboración propia con datos de MCV, 2022", 
                       status = "primary", 
                       solidHeader = T, 
                       width = 12, 
                       plotOutput("grafica_evo_indicador") %>% withSpinner()
                     )    
                  )
            )
        )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black")

server <- function(input, output, session) {
 
  output$selComponente <- renderUI({
    selectizeInput("selComponente", 
                   "Componente", 
                   choices = gen_opciones_componente(input$selDimension))
  })
  
  output$selIndicador <- renderUI({
    selectizeInput("selIndicador", 
                   "Indicador", 
                   choices = gen_opciones_indicador(input$selComponente))
  })
  
  output$selAnio <- renderUI({
    sliderTextInput(inputId = "selAnio", 
                    "Año", 
                    choices = gen_opciones_anio(input$selIndicador), 
                    grid = T)
  })
  
  output$mapa_indicadores <- renderLeaflet({
    gen_mapa_indicadores(anio_sel = req(input$selAnio), 
                         indicador_sel = req(input$selIndicador))
  })
  
  output$grafica_evo_indicador <- renderPlot({
    gen_grafica_evo_indicador(indicador_sel = req(input$selIndicador), 
                              edo_sel = req(input$selEdo))
  })
  
}

shinyApp(ui, server)
