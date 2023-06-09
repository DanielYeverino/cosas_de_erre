
# 1. Librerias. ----
library(magick)
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(sf)
library(leaflet)
library(shinycssloaders)
library(shinyWidgets)
library(ggrepel)
library(webshot)

# 2. Cargar la información: ----
mcv_discrete <- c("#6950D8", "#3CEAFA", "#00B783", "#FF6260", "#FFAF84", "#FFBD41")
catalogo_entidades <- readRDS("www/catalogo_entidades.rds")
shp <- readRDS("www/shp.rds")
meta <- readRDS("www/meta.rds")
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

year_sel = 2015
# 
ips$ips %>% min()
ips$ips %>% max()

gen_grafica_mapa_ips <- function(year_sel){
  ind_sel_data <- ips %>% 
    # filter(id_indicador == indicador_sel) %>%
    filter(anio == year_sel) %>% 
    mutate(anio = as.numeric(anio), 
           indicador_value = ips)

  pal <- colorNumeric(
    palette = c("red", "yellow","green"), 
    domain = c(45,76)
  )

  left_join(shp, ind_sel_data) %>% 
    leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(fillColor = ~pal(indicador_value), 
                fillOpacity = 1, 
                color = "black", 
                weight = 1) %>% 
    addLegend(pal = pal, values = 45:76, 
              title = str_c("Índice de Progreso<br>Social", 
                            " <b style='color:red;'>",
                            year_sel, 
                            "</b>")) 
}
            
year_sel = 2021
gen_mapa_ggplot_ips <- function(year_sel){
  
  ind_sel_data <- ips %>% 
    # filter(id_indicador == indicador_sel) %>%
    filter(anio == year_sel) %>% 
    mutate(anio = as.numeric(anio), 
           indicador_value = ips)
  
  mapa <- left_join(shp, ind_sel_data) %>% 
    mutate(ctrd_x = (st_centroid(.) %>% st_coordinates())[,1],
           ctrd_y = (st_centroid(.) %>% st_coordinates())[,2])
  
  g_plot = mapa %>% 
    ggplot(aes(fill = ips)) + 
    geom_sf(color = "gray50") + 
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = str_c(
                    # entidad_abr_m, "\n",
                                format(round(ips, 1), nsmall = 1))), 
                  size = 5.5, 
              family = "Ubuntu", 
              fontface = "bold") + 
    scale_fill_gradientn(colors = c("red", "yellow", "green"), 
                         limits = c(45,76)) + 
    labs(x = NULL, y = NULL, fill = "Índice de Progreso Social", 
         title = str_c("\nÍndice de Progreso Social ", year_sel)) + 
    theme_bw() + 
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_text(), 
          panel.grid = element_blank(), 
          panel.border = element_blank(), 
          legend.title = element_text(face = "bold", 
                                      family = "Ubuntu", 
                                      size = 35,
                                      color = mcv_discrete[1]),
          plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    size = 50,
                                      family = "Ubuntu", 
                                    hjust = 0.5,
                                      color = mcv_discrete[1]),
          legend.text = element_text(face = "bold", 
                                      family = "Ubuntu", 
                                     size = 30,
                                      color = mcv_discrete[4]),
          legend.position = c(0.2,0.1),
          # legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(fill = guide_colorbar(barwidth = 40, 
                                 barheight = 0.5, 
                                 title.position = "top", 
                                 title.hjust = 0.5))
  g_plot
  # tibble(anio = 2015:2021, 
  #        valor = (2015:2021)-2014
  #        ) %>% 
  #   ggplot(aes(x = 1, 
  #              y = valor)) + 
  #   geom_col(color = "white") + 
  #   scale_y_continuous(limits = c(0,6)) + 
  #   scale_x_continuous(expand = expansion(c(3,3))) + 
  #   coord_flip()
  
  
  g_plot <- ggimage::ggbackground(g_plot, str_c("/Users/juvenalcampos/Documents/GitHub/mcv_infobites/03_infobites/00_plantillas/99_PLANTILLA_IPS.pdf"))
  
  ggsave(g_plot,
         filename = str_c(year_sel, ".png"),
         width = 25, height = 17, dpi = 200,
         bg= "transparent")
}

lapply(2015:2021, gen_mapa_ggplot_ips)

# y = 2015
# library(webshot)
# mapas <- lapply(2015:2021, function(y){
#   gen_grafica_mapa_ips(y) %>% 
#     htmlwidgets::saveWidget(str_c("temp.html"), 
#                selfcontained = FALSE)  
#   webshot::webshot(url = "temp.html",
#           file = str_c("temp", y, ".png"))
#   print(y)
# })


library(magick)
list.files(pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=0.8) %>% # animates, can opt for number of loops
  image_write("gif_ips.gif") # write
