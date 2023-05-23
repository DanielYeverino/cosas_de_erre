tema_chaini <- function(
    font_var = "Montserrat", 
    hjust_var = 0,
    size_var = 30,
    is.map = FALSE,
    dark.back = FALSE,
    lineheight_var = .8
) {
  
  pacman::p_load(showtext, ggtext, extrafont)
  
  election_theme <- theme_minimal() +
    theme(
      text = element_text(
        family = font_var,
        lineheight = lineheight_var
      ),
      plot.title = element_text(
        face = "bold",
        size = size_var,
        hjust = hjust_var, 
        margin = ggplot2::margin(t = 0, r = 0, b = 12.5, l = 0, unit = "pt"), 
        color = case_when(
          dark.back == FALSE ~ "black",
          dark.back == TRUE ~ "white"
        )
      ),
      plot.subtitle = element_text(
        face = "italic",
        size = (size_var - 6), 
        hjust = hjust_var, 
        margin = ggplot2::margin(t = 0, r = 0, b = 15, l = 0, unit = "pt"),
        color = case_when(
          dark.back == FALSE ~ "gray20",
          dark.back == TRUE ~ "gray80"
        )
      ),
      legend.text = element_text(
        size = (size_var - 10), 
        face = "bold",
        color = case_when(
          dark.back == FALSE ~ "gray10",
          dark.back == TRUE ~ "gray100"
        )
      ),
      axis.ticks = element_line(
        size = 0.5, 
        color = case_when(
          dark.back == FALSE ~ "gray40",
          dark.back == TRUE ~ "white"
        )
      ),
      axis.text = element_text(
        size = (size_var - 6),
        face = "bold", 
        color = case_when(
          dark.back == FALSE ~ "gray40",
          dark.back == TRUE ~ "white"
        )
      ),
      axis.text.x = element_text(
        hjust = 0.5, 
        face = "bold", 
        size = (size_var - 7),
        family = font_var
      ),
      axis.text.y = element_text(
        hjust = 0.5, 
        face = "bold",
        size = (size_var - 7),
        family = font_var
      ),
      axis.title = element_text(
        size = (size_var - 8), 
        face = "bold", 
        color = case_when(
          dark.back == FALSE ~ "black",
          dark.back == TRUE ~ "gray80"
        )
      ),
      axis.title.y = element_text(
        margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)
      ),
      axis.title.x = element_text(
        margin = ggplot2::margin(t = 15, r = 0, b = 10, l = 0)
      ),
      axis.line = element_line(
        color = case_when(
          dark.back == FALSE ~ "gray40",
          dark.back == TRUE ~ "white"
        ), 
        size = 0.5
      ),
      panel.grid.major = element_line(
        linetype = 'dashed', 
        size = 0.4,
        color = case_when(
          dark.back == FALSE ~ "lightgray",
          dark.back == TRUE ~ "white"
        )
      ),
      panel.grid.minor = element_blank(),
      strip.text = element_text(
        size = (size_var - 5), 
        face = "bold", 
        color = case_when(
          dark.back == FALSE ~ "black",
          dark.back == TRUE ~ "gray80"
        )
      ),
      plot.caption = element_text(
        color = case_when(
          dark.back == FALSE ~ "black",
          dark.back == TRUE ~ "white"
        ), 
        face = "italic",
        size = (size_var - 12.5), 
        hjust = 1
      ),
      plot.margin = unit(c(1.05, 1.05, 1.05, 1.05), "cm")
    ) 
  
}

