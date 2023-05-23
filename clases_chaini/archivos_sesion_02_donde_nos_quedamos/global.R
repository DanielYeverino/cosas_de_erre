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
no_sel <- 196
cve_mun_sel <- "01002"
cve_ent_sel <- "01"
###

gen_linea <- function(no_sel, 
                      cve_mun_sel, 
                      cve_ent_sel){
  
  # 1. Filtrar los datos para quedarnos con la info del indicador 1 y del municipio 01001
  bd_sel_ts <- bd %>% 
    filter(no == no_sel) %>% 
    filter(cve_mun == cve_mun_sel)
  
  # Etiquetas: 
  meta_sel <- meta %>% 
    filter(no == no_sel)
  info_mpio <- catalogo_municipal %>% 
    filter(cve_mun == cve_mun_sel)
  
  bd_sel_ts %>% 
    ggplot(aes(x = year, y = valor)) + 
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
}

# gen_linea(no_sel = 1, 
#           cve_mun_sel = "17016", 
#           cve_ent_sel = "17")

##### Gráfica de barras ----
no_sel = 1
year_sel = 2019
cve_ent_sel = "09"

gen_barras <- function(no_sel, 
                       year_sel, 
                       cve_ent_sel){
  
  # Informacion complementaria: 
  meta_sel <- meta %>% 
    filter(no == no_sel)
  info_mpio <- catalogo_municipal %>% 
    filter(cve_ent == cve_ent_sel)
  
  bd_barras <- bd %>% 
    left_join(catalogo_municipal) %>% 
    filter(year == year_sel) %>% 
    filter(cve_ent == cve_ent_sel)
  # mutate(cve_ent = str_extract(cve_mun, pattern = "^\\d\\d"))
  
  bd_barras %>% 
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
    theme_bw() -> g1
  
  plotly::ggplotly(g1)
  
}

gen_barras(cve_ent_sel = "02", 
           no_sel = 1, 
           year_sel = 2010)

# Mapa ----
ent_sel <- "17"
no_sel <- 1
year_sel <- 2010

gen_mapa <- function(ent_sel, 
                     no_sel, 
                     year_sel){
  
  bd_sel <- bd %>% 
    filter(!str_detect(cve_mun, pattern = "000")) %>% 
    mutate(cve_ent = str_extract(cve_mun, pattern = "^\\d\\d")) %>% 
    filter(no == no_sel) %>% 
    filter(year == year_sel) %>% 
    filter(cve_ent == ent_sel)

  meta_sel <- meta %>% 
    filter(no == no_sel)
  info_mpio <- catalogo_municipal %>% 
    filter(cve_ent == cve_ent_sel)
  
  datos_mapa <- right_join(shp, bd_sel, by = c("CVEGEO" = "cve_mun"))
  
  datos_mapa %>% 
    ggplot(aes(fill = valor)) + 
    geom_sf(color = "white") + 
    labs(title = meta_sel$indicador, 
         subtitle = datos_mapa$NOM_ENT, 
         caption = str_wrap(meta_sel$fuentes, 50)) + 
    theme_bw() + 
    theme(legend.position = "bottom")
  
}

# gen_mapa(ent_sel = "22",
#          no_sel = 1,
#          year_sel = 2010)

# Valores disponibles ----
# Indicadores disponibles
indicadores_disponibles <- meta$no
names(indicadores_disponibles) <- str_c(meta$no, ": ", meta$indicador)

anio_disponible_por_indicador <- function(no_sel){
  bd %>% 
    filter(no == no_sel) %>% 
    pull(year) %>% 
    unique()  
}

entidades_a_escoger <- catalogo_estatal$cve_ent[-c(1, 34:36)]
names(entidades_a_escoger) <- catalogo_estatal$entidad[-c(1, 34:36)]

gen_municipios <- function(cve_ent_sel){
  
  catalogo_municipal %>% 
    filter(cve_ent == cve_ent_sel) %>% 
    filter(!str_detect(cve_mun, pattern = "000|999")) %>%
    pull(cve_mun) %>% 
    sort() -> claves_municipios
  
  catalogo_municipal %>% 
    filter(cve_ent == cve_ent_sel) %>% 
    filter(!str_detect(cve_mun, pattern = "000|999")) %>%
    pull(municipio) %>% 
    sort() -> nombres_municipios
  
  names(claves_municipios) <- nombres_municipios
  
  return(claves_municipios)
  
}

