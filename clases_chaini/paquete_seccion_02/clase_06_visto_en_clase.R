# Librerias 
library(tidyverse)
library(readxl)
library(leaflet)
library(microbenchmark)
library(sf)

# 1er Ejemplo: Guardar objetos en un archivo .RData
bitcoin <- read_csv("01_Datos/Bases de datos/bitcoin.csv")
pokemon <- read_xlsx("01_Datos/Bases de datos/datos_pokemon.xlsx")
presupuesto <- read_csv("01_Datos/Bases de datos/pef.csv")

# Enlatamos datos ----
save(bitcoin, pokemon, presupuesto, 
     file = "enlatado.RData")

# Desenlatar el RData ----
load(file = "enlatado.RData")

# RDS
saveRDS(object = pokemon, 
        file = "pokemonEnlatado.rds")

pokemon2 <- readRDS("01_Datos/Bases de datos/pokemonEnlatado.rds")


benchmark <- microbenchmark(readRDS("01_Datos/Bases de datos/pokemonEnlatado.rds"), 
  readxl::read_excel("01_Datos/Bases de datos/datos_pokemon.xlsx"), 
  times = 10)


ags <- readRDS(gzcon(url("https://github.com/JuveCampos/ProyectosEnShiny/raw/master/shinyRedNacionalCaminos/www/BasesDeDatosAbreviadas/AGUASCALIENTES.rds")))
class(ags)


ags <- readRDS(gzcon(url("https://github.com/JuveCampos/ProyectosEnShiny/raw/master/shinyRedNacionalCaminos/www/BasesDeDatosAbreviadas/AGUASCALIENTES.rds"))) %>% 
  mutate(is.Carretera2 = ifelse(TIPO_VIAL == "Carretera", 
                                "red", 
                                "blue"))
leaf <- leaflet(ags) %>% 
  addProviderTiles(provider = providers$CartoDB.Positron) %>% 
  addPolylines(label = ags$NOMBRE, 
               opacity = 1, 
               color = ags$is.Carretera2, 
               weight = 3*ags$isCarretera)
leaf

class(leaf)
htmlwidgets::saveWidget(leaf, "02_Codigo/mapaAguascalientes.html")

saveRDS(leaf, "leaf.rds")
rm(leaf)
readRDS("leaf.rds")

?source
source(file = "01_Datos/Bases de datos/stock_analysis_functions.R")
get_stock_list()
datos_aal <- get_stock_data("AAL")
plot_stock_data(datos_aal)
generate_commentary(data = datos_aal, 
                    "AAL")

