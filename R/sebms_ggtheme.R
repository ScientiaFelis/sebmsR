#' Theme for Swedish Butterfly Monitoring scheme for use in ggplot
#' 
#' @param title_sz font size for title
#' @param x_title_sz font size for x axis title
#' @param y_title_sz font size for y axis title
#' @param x_sz font size for x axis text
#' @param y_sz font size for y axis text
#' @param legend_position char indicating legend_position such as "none"
#' @return a theme that can be used for ggplot plot objects
#' @import ggplot2
#' @export
theme_sebms <- function(title_sz = 24, 
                        x_title_sz = 14, y_title_sz = 14, 
                        x_sz = 12, y_sz = 12, legend_position = "none", fontfamily = "Arial") 
{
  
  theme_sb <- theme_bw() +
    theme(
      text = element_text(family = fontfamily),
      plot.title = element_text(size = 24, face = "bold", 
                                margin = margin(0, 0, 25, 0)),
      axis.text.x = element_text(size = x_sz, 
                                 margin = margin(5, 0, 0, 0)),
      axis.text.y = element_text(size = y_sz, 
                                 margin = margin(0, 7, 0, 0)),
      axis.title.x = element_text(size = x_title_sz, face = "bold", 
                                  margin = margin(0, 20, 0, 0)),
      axis.title.y = element_text(size = y_title_sz, face = "bold", 
                                  margin = margin(0, 15, 0, 0), angle = 90),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(size = 0.5, colour = "grey"),
      panel.background = element_rect(size = 0.5),
      panel.border = element_rect(color = "black"),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 16, margin = margin(t = 0, b = 15)),
      legend.position = legend_position
    )
  
  return(theme_sb)
}

