# Referencias: https://elartedeldato.com/blog/como-hacer-un-treemap-en-highcharter/


library(tidyverse)
library(highcharter)


set.seed(110)

ex <- data.frame(
  # Nivel 1: 
  Level1 = rep(paste0("Country", seq(1:5)), each = 5),
  # Nivel 2: 
  Level2 = rep(paste0("Sector", seq(1:5)), 5),
  # Valor: 
  Percentage = runif(25, 0, 1)
)

# Datos: 
ex %>% 
  data_to_hierarchical(c(Level1, Level2), 
                       Percentage, 
                       colors = wesanderson::wes_palettes$Zissou1) %>%
  hchart(type = "treemap",
         # hcaes(color = Level1),
         allowTraversingTree = T,
         levelIsConstant = F,
         drillUpButton = list(
           text = "← Volver"
         ),
         levels = list(
           list(level = 1, dataLabels = list(enabled = TRUE, 
                                             format = "{point.name}<br>
                                                      {point.value}%"), borderColor = "black", borderWidth = 2),
           list(level = 2, dataLabels = list(enabled = FALSE))
         )
  ) 
# %>% 
#   hc_colorAxis(stops = color_stops(colors = viridis::inferno(10)))



summary.table %>%
  hchart("treemap", hcaes(x = manufacturer,
                          value = nb_cars,
                          color = nb_model)) %>%
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10)))
