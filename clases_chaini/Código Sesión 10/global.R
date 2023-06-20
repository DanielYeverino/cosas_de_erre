# Librerias ----
library(tidyverse)
library(sf)
library(leaflet)

# Cargar los datos ----
tacos_cdmx <- readRDS("www/tacos_cdmx.rds") %>% 
  st_as_sf(coords = c("longitud", "latitud"), 
           crs = "4326")
mpios <- readRDS("www/mpios.rds")
ent <- readRDS("www/ents.rds")
# plot(mpios, max.plot = 1)

# Funcion generadora de mapa de taquerías ----
alcaldia_sel <- "Iztapalapa"
# gen_mapa(alcaldia_sel = "Tlalpan")
gen_mapa <- function(alcaldia_sel){
  
  alcaldia_shp <- mpios %>% 
    filter(NOM_MUN == alcaldia_sel)
  
  tacos_alcaldia <- tacos_cdmx %>% 
    filter(municipio == alcaldia_sel)
  
  label <- str_c(
    "<b style = 'color:green;'>", "Nombre del establecimiento: ",  "</b><br>", tacos_alcaldia$nom_estab, "<br>",
    "<b>", "Nombre de actividad: ",   "</b><br>", tacos_alcaldia$nombre_act, "<br>"
  ) %>% 
    lapply(htmltools::HTML)
  
  popups <- str_c(
    "<b style = 'color:green;'>", "Nombre del establecimiento: ",  "</b>", tacos_alcaldia$nom_estab, "<br>",
    "<b>", "Nombre de actividad: ",   "</b>", tacos_alcaldia$nombre_act, "<br>",
    "<b>", "Personal ocupado: ",  "</b>", tacos_alcaldia$per_ocu, "<br>",
    "<b>", "Asentamiento: ",  "</b>", tacos_alcaldia$nomb_asent,  "<br>"
  )
  

  leaflet() %>% 
    addProviderTiles("CartoDB.DarkMatter") %>% 
    addPolygons(data = alcaldia_shp, 
                color = "white", 
                fill = NA) %>% 
    addCircleMarkers(data = tacos_alcaldia, 
                     radius = 1,
                     popup = popups,
                     color = "orange", 
                     label = label,
                     opacity = 0.4, 
                     layerId = tacos_alcaldia$id)
  
}


# Construir las opciones de los controles -----
opcion_alcaldias <- mpios$NOM_MUN %>% sort()



