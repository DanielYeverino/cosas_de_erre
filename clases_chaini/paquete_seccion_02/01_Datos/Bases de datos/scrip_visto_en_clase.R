
# Librerias: 
library(tidyverse)
library(readxl)
library(sf)
library(microbenchmark)
library(leaflet)
library(tidyquant)

# Datos en un *csv: 
agro <- read_csv("datos_agricolas_2019.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))

# Datos en un excel: 
tweet <- read_excel("tweets_vacunacion.xlsx")

# Cargamos un rdata
load("enlatado.RData")
save(agro, tweet, file = "enlatado_2.RData")
load("enlatado_2.RData")

# Cargar un RDS: 
btc <- readRDS("bitcoin.rds")
saveRDS(agro, "agro.rds")
"https://github.com/JuveCampos/ProyectosEnShiny/raw/master/shinyRedNacionalCaminos/www/BasesDeDatosAbreviadas/AGUASCALIENTES.rds"
ags <- readRDS(gzcon(url("https://github.com/JuveCampos/ProyectosEnShiny/raw/master/shinyRedNacionalCaminos/www/BasesDeDatosAbreviadas/AGUASCALIENTES.rds")))
plot(ags, max.plot = 1)
?gzcon

mapa <- leaflet(ags) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%  
  addPolylines()

mapa

saveRDS(mapa, "mapa_ags_caminos.rds")
readRDS("mapa_ags_caminos.rds")

# Compare the two functions
compare <- microbenchmark(read_csv("bitcoin.csv"), 
                          readRDS("bitcoin.rds"), 
                          times = 10)

# Leemos código 
source("stock_analysis_functions.R")
get_stock_list(stock_index = "SP500")
apl = get_stock_data(stock_symbol = "AAPL")

