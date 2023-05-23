#Librerias 
library(tidyverse)
library(readxl)
library(microbenchmark)
library(leaflet)

# Datos en un *.RData
bitcoin <- read_csv("01_Datos/Bases de datos/bitcoin.csv")
pokemon <- read_xlsx("01_Datos/Bases de datos/datos_pokemon.xlsx")
presupuesto <- read_csv("01_Datos/Bases de datos/pef.csv")

# Guardamos un conjunto de datos----
save(bitcoin, pokemon, presupuesto, 
     file = "01_Datos/conjunto_datos.RData")

# Cargamos un conjunto de datos----
load("01_Datos/conjunto_datos.RData")

# Guardar archivos RDS ----
saveRDS(bitcoin, "01_Datos/Bases de datos/bitcoin.rds")
bd <- readRDS("01_Datos/Bases de datos/bitcoin.rds")

# Leer archivos RDS de internet ----
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
saveRDS(leaf, "mapa_aguascalientes.rds")
htmlwidgets::saveWidget(leaf, "mapaAguascalientes.html")

# Compare the two functions
compare <- microbenchmark(read_csv("01_Datos/Bases de datos/bitcoin.csv"), 
                          readRDS("01_Datos/Bases de datos/bitcoin.rds"), 
                          times = 10)
compare

# Comparación de tamaños ----
file.size("01_Datos/Bases de datos/bitcoin.csv")
file.size("01_Datos/Bases de datos/bitcoin.rds")

# Ejecutar y cargar scripts ----
source("stock_analysis_functions.R")
get_stock_list()
datos = get_stock_data(stock_symbol = "TSLA")
plt <- plot_stock_data(datos)
plt
generate_commentary(data=datos, user_input = "TSLA")
