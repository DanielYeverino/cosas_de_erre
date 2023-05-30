# Mi primer shiny
# Juvenal Campos - 17 de mayo, 2023

# Opciones: 
options(scipen = 999)

# Librerias: 
library(tidyverse)
library(sf)
library(readxl)
library(numform)
library(plotly)
library(leaflet)

# Datos: 
catalogo_estatal <- readxl::read_excel("www/catalogo_estatal.xlsx")
catalogo_municipal <- readxl::read_excel("www/catalogo_municipal.xlsx")
meta <- read_excel("www/metadatos_01.xlsx")
bd <- read_excel("www/datos_01.xlsx")
shp <- st_read("www/municipios.geojson")

# 
catalogo_municipal <- left_join(catalogo_municipal, catalogo_estatal)


# Gráfica de lineas: 
# Municipio de Aguascaloentes, Aguascalientes. 

# meta
# "Población total a nivel municipal" 

###
no_sel <- 9
cve_mun_sel <- "01002"
cve_ent_sel <- "01"
###

gen_metadatos <- function(no_sel){
  meta_sel <- meta %>% 
    filter(no == no_sel)
  return(meta_sel)
}




# Grafica de línea ----
gen_linea <- function(no_sel, 
                      cve_mun_sel, 
                      cve_ent_sel, 
                      metadatos){
  
  # 1. Filtrar los datos para quedarnos con la info del indicador 1 y del municipio 01001
  bd_sel_ts <- bd %>% 
    filter(no == no_sel) %>% 
    filter(cve_mun == cve_mun_sel)
  
  # Etiquetas: 
  meta_sel <- metadatos
  
  info_mpio <- catalogo_municipal %>% 
    filter(cve_mun == cve_mun_sel)
  
  plt_lineas <- bd_sel_ts %>% 
    ggplot(aes(x = year, 
               y = valor, 
               group = no,
               text = paste0(
                 "<b>", meta_sel$indicador, "</b>",  "<br>",
                 "<b>Valor: </b>", valor, "<br>",
                             "<b>Año: </b>", year
                             )
               )) + 
    geom_line() + 
    geom_point() + 
    labs(title = meta_sel$indicador, 
         subtitle = str_c("Municipio: ", info_mpio$municipio, 
                          " - Entidad: ", info_mpio$entidad), 
         caption = str_wrap(meta_sel$fuentes, 
                            60), 
         y = meta_sel$umedida
    ) + 
    scale_y_continuous(labels = scales::comma_format()) + 
    theme_bw()
  
  plt_lineas %>% 
    ggplotly(tooltip = "text")
  
}

# gen_linea(no_sel = 1, 
#           cve_mun_sel = "17016", 
#           cve_ent_sel = "17")

##### Gráfica de barras ----
no_sel = 9
year_sel = 2000
cve_ent_sel = "01"

gen_barras <- function(no_sel, 
                       year_sel, 
                       cve_ent_sel, 
                       metadatos){
  
  # Informacion complementaria: 
  meta_sel <- metadatos
  info_mpio <- catalogo_municipal %>% 
    filter(cve_ent == cve_ent_sel)
  
  bd_barras <- bd %>% 
    left_join(catalogo_municipal) %>% 
    filter(year == year_sel) %>% 
    filter(cve_ent == cve_ent_sel)
  # mutate(cve_ent = str_extract(cve_mun, pattern = "^\\d\\d"))
  
  g1 <- bd_barras %>% 
    ggplot(aes(x = reorder(municipio, valor), 
               y = valor)) + 
    geom_col() + 
    coord_flip() + 
    labs(title = meta_sel$indicador, 
         subtitle = str_c("Entidad: ", info_mpio$entidad), 
         caption = str_wrap(meta_sel$fuentes, 
                            60), 
         y = meta_sel$umedida, 
         x = NULL) + 
    theme_bw()
  
  plotly::ggplotly(g1)
  
}

# gen_barras(cve_ent_sel = "02", 
#            no_sel = 1, 
#            year_sel = 2010)

# Mapa ----
ent_sel <- "17"
no_sel <- 9
year_sel <- 2010

gen_mapa <- function(ent_sel, 
                     no_sel, 
                     year_sel, 
                     metadatos){
  
  bd_sel <- bd %>% 
    filter(!str_detect(cve_mun, pattern = "000")) %>% 
    mutate(cve_ent = str_extract(cve_mun, pattern = "^\\d\\d")) %>% 
    filter(no == no_sel) %>% 
    filter(year == year_sel) %>% 
    filter(cve_ent == ent_sel)
  
  meta_sel <- metadatos
  # meta_sel$indicador
  datos_mapa <- right_join(shp, bd_sel, by = c("CVEGEO" = "cve_mun"))
  
  pal_colores <- colorNumeric(
    palette = wesanderson::wes_palettes$Zissou1,
      # c("green", "yellow", "red"),
      # RColorBrewer::brewer.pal(9, "Blues")
    domain = datos_mapa$valor
    )
  
  
  
  if(meta_sel$umedida == "Porcentaje"){
    unidad <- "%"
  } else {
    unidad <- ""
  }
  
  
  popups <- paste0("<b>Municipio: </b>", datos_mapa$NOM_MUN, "<br>", 
                   "<b>Indicador: </b>", meta_sel$indicador[1], "<br>", 
                   "<b>Valor: </b>", datos_mapa$valor, unidad)

  etiquetas <- paste0("<b>Municipio: </b>", datos_mapa$NOM_MUN) %>% 
    lapply(htmltools::HTML)
  
  leaflet(datos_mapa) %>% 
    # addTiles() %>% 
    addProviderTiles(provider = providers$CartoDB.Positron) %>% 
    addPolygons(fillColor = pal_colores(datos_mapa$valor), 
                fillOpacity = 1, 
                color = "black", 
                opacity = 1,
                popup = popups,
                label = etiquetas,
                weight = 0.5) %>% 
    addLegend(title = str_wrap(meta_sel$indicador, 10) %>% str_replace_all("\n", "<br>"), 
              position = "bottomright", 
              pal = pal_colores, 
              values = datos_mapa$valor, 
              labels = labelOptions(suffix = unidad)
              )
  
  # datos_mapa %>% 
  #   ggplot(aes(fill = valor)) + 
  #   geom_sf(color = "white") + 
  #   labs(title = meta_sel$indicador, 
  #        subtitle = datos_mapa$NOM_ENT, 
  #        caption = str_wrap(meta_sel$fuentes, 50)) + 
  #   theme_bw() + 
  #   theme(legend.position = "bottom")
  
}

# gen_mapa(ent_sel = "22",
#          no_sel = 1,
#          year_sel = 2010)

# Valores disponibles ----
# Indicadores disponibles
indicadores_disponibles <- meta$no
names(indicadores_disponibles) <- str_c(meta$no, ": ",  meta$indicador)

anio_disponible_por_indicador <- function(no_sel){
  bd %>% 
    filter(no == no_sel) %>% 
    pull(year) %>% 
    unique()  
}

entidades_a_escoger <- catalogo_estatal$cve_ent[-c(1, 34:36)]
names(entidades_a_escoger) <- catalogo_estatal$entidad[-c(1, 34:36)]

# cve_ent_sel = "17"
# gen_municipios("17")
gen_municipios <- function(cve_ent_sel){
  
  claves_municipios <- catalogo_municipal %>% 
    filter(cve_ent == cve_ent_sel) %>% 
    filter(!str_detect(cve_mun, pattern = "000|999")) %>%
    pull(cve_mun) %>% 
    sort()  
  
  nombres_municipios <- catalogo_municipal %>% 
    filter(cve_ent == cve_ent_sel) %>% 
    filter(!str_detect(cve_mun, pattern = "000|999")) %>%
    pull(municipio) %>% 
    sort()  
  
  names(claves_municipios) <- nombres_municipios
  return(claves_municipios)  
  
}

