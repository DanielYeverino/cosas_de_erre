# Setup----
gc()

Sys.setlocale(locale = "es_ES.UTF-8")
options(scipen = 999)

source("tema_chaini.R")

pacman::p_load(
  sf, readxl, tidyverse
  )

########################################### SHINY MUNICIPAL ###########################################

# Cargar bases----
read_excel(
  "www/catalogo_estatal.xlsx"
  ) -> df_catest 

read_excel(
  "www/catalogo_municipal.xlsx"
  ) -> df_catmun

read_excel(
  "www/metadatos_01.xlsx"
) -> df_meta

read_excel(
  "www/datos_01.xlsx"
  ) -> df_dat 

sf::st_read(
  "www/municipios.geojson"
  ) -> shp_mun

# Limpieza de bases----
left_join(
  df_catmun,
  df_catest
) -> df_catalogojoin

# Grafica de lineas----
# Municipio de Aguascalientes, Aguascalientes

######
# no_sel = 196
# cve_mun_sel <- "01002"
# cve_ent_sel <- "01"
######

gen_linea <- function(no_sel, cve_mun_sel, cve_ent_sel) {
  
  #* 1. Filtrar de los datos ags ye indicador----
  df_dat %>% 
    filter(
      no == no_sel,
      cve_mun == cve_mun_sel
    ) -> df_graf1
  
  df_meta %>% 
    filter(
      no == no_sel,
    ) -> meta_sel
  
  df_catalogojoin %>% 
    filter(
      cve_mun == cve_mun_sel
    ) -> info_mpio
  
  ggplot(
    df_graf1,
    aes(
      x = year,
      y = valor
    )
  ) +
    labs(
      title = meta_sel$indicador,
      subtitle = paste0(
        str_c(
          "Municipio: ", info_mpio$municipio,
          " - Entidad: ", info_mpio$entidad
        )
      ),
      caption = str_wrap(
        meta_sel$fuentes,
        60
      ),
      y = meta_sel$umedida
    ) +
    geom_line() +
    geom_point() +
    tema_chaini(size_var = 20) +
    scale_x_continuous(
      labels = unique(df_graf1$year),
      breaks = unique(df_graf1$year)
    ) +
    scale_y_continuous(
      labels = scales::comma_format()
    )
  
}

# gen_linea(no_sel = 161, cve_mun_sel = "17016", cve_ent_sel = "17")

# Grafica de barras----

gen_barras <- function(no_sel, year_sel, cve_ent_sel) {
  
  df_dat %>% 
    left_join(
      df_catalogojoin
    ) %>% 
    filter(
      year == year_sel,
      cve_ent == cve_ent_sel,
      no == no_sel
    ) -> df_graf2
  
  df_meta %>% 
    filter(
      no == no_sel,
    ) -> meta_sel
  
  df_catalogojoin %>% 
    filter(
      cve_mun == cve_mun_sel
    ) -> info_mpio
  
  ggplot(
    df_graf2,
    aes(
      x = reorder(municipio, valor),
      y = valor
    )
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = meta_sel$indicador,
      subtitle = paste0(
        str_c(
          "Municipio: ", info_mpio$municipio,
          " - Entidad: ", info_mpio$entidad
        )
      ),
      caption = str_wrap(
        meta_sel$fuentes,
        60
      ),
      y = meta_sel$umedida
    ) +
    tema_chaini(size_var = 20) 
  
}

# Mapa ----
# ent_sel <- "01"
# no_sel <- 196
# year_sel <- 2015

gen_mapa <- function(ent_sel, no_sel, year_sel) {
  
  df_meta %>% 
    filter(
      no == no_sel,
    ) -> meta_sel
  
  df_dat %>% 
    mutate(
      cve_ent = str_extract(cve_mun, pattern = '^\\d\\d')
    ) %>%
    filter(
      !str_detect(cve_mun, pattern = "000"),
      cve_ent == ent_sel,
      no == no_sel,
      year == year_sel,
    ) -> df_sel
  
  right_join(
    shp_mun,
    df_sel, 
    by = c(
      "CVEGEO" = "cve_mun"
    )
  ) -> df_map
  
  ggplot(
    df_map,
    aes(
      fill = valor
    ) 
  ) +
    labs(
      title = meta_sel$indicador,
      subtitle = df_map$NOM_ENT,
      caption = str_wrap(
        meta_sel$fuentes,
        50
      )
    ) +
    geom_sf() +
    tema_chaini(size_var = 20) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      plot.caption = element_markdown(hjust = 0)
    )
  
}

# gen_mapa(ent_sel = "22", no_sel = 10, year_sel = 2010)

# Valores disponibles----
#* Indicadores disponibles----
indicadores_disponibles <- unique(df_meta$no)

anio_disponible_por_indicador <- function(no_sel) {
  
  df_dat %>% 
    filter(
      no == no_sel
    ) %>% 
    pull(year) %>% 
    unique()
  
}

# anio_disponible_por_indicador(no_sel = 210)

#* Entidades a escoger----
entidades_a_escoger <- df_catest$cve_ent[-c(1, 34:36)]
names(entidades_a_escoger) <- df_catest$entidad[-c(1, 34:36)]

gen_municipios <- function(cve_ent_sel) {

df_catmun %>% 
  filter(
    cve_ent == cve_ent_sel,
    !str_detect(cve_mun, pattern = "000|999")
  ) %>% 
  pull(cve_mun) %>% 
    sort()

}

# gen_municipios(cve_ent_sel = "05")
