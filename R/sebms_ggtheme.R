#' Theme for Swedish Butterfly Monitoring Scheme for Weather Figures
#' 
#' @param title_sz font size for title
#' @param x_title_sz font size for x axis title
#' @param y_title_sz font size for y axis title
#' @param x_sz font size for x axis text
#' @param y_sz font size for y axis text
#' @param legend_position char indicating legend_position such as "none"
#' @param fontfamily the font family to use
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
      panel.grid.major.y = element_line(linewidth = 0.5, colour = "grey"),
      panel.background = element_rect(linewidth = 0.5),
      panel.border = element_rect(color = "black"),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 16, margin = margin(t = 0, b = 15)),
      legend.position = legend_position
    )
  
  return(theme_sb)
}

#' Theme for Swedish Butterfly Monitoring Scheme for Species Figures
#' 
#' @inheritParams theme_sebms
#' 
#' @return a theme that can be used for ggplot plot objects
#' @import ggplot2
#' @export
theme_sebms_species <- function(title_sz = 18, 
                                x_title_sz = 16, y_title_sz = 16, 
                                x_sz = 16, y_sz = 16,
                                legend_position = "none", fontfamily = "Arial") 
{
  
  theme_sb <- theme_bw() +
    theme(
      plot.title = element_text(family = fontfamily,
                                face = "bold",
                                size = title_sz,
                                colour = "black",
                                hjust = 0.5,
                                margin = margin(0, 0, 5, 0)),
      plot.tag = element_text(vjust = 0, size = 14),
      plot.tag.position = c(0.05, 0.039),
      axis.title.x = element_text(family = fontfamily,
                                  face = "bold",
                                  size = x_title_sz, 
                                  colour = "black",
                                  margin = margin(9, 20, 0, 0)),
      axis.title.y = element_text(family = fontfamily,
                                  face = "bold",
                                  size = y_title_sz,
                                  colour = "black",
                                  margin = margin(0, 15, 0, 0), angle = 90),
      axis.text.x = element_text(family = fontfamily,
                                 face = "plain",
                                 size = x_sz,
                                 colour = "black",
                                 margin = margin(5, 0, 0, 0),
                                 lineheight = 1.3),
      axis.text.y = element_text(family = fontfamily,
                                 face = "plain",
                                 size = y_sz,
                                 colour = "black",
                                 margin = margin(0, 4, 0, 0)),
      axis.ticks = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.7, colour = "grey65"),
      panel.background = element_rect(linewidth = 0.5),
      panel.border = element_rect(color = "black", linewidth = 0.8),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 16, margin = margin(t = 0, b = 15)),
      legend.position = legend_position
    )
  
  return(theme_sb)
}


#' Palette Used in ggplots for Weather Plots
#' 
#' This is also used in some of the trim plots.
#' 
#' @return vector of color hex codes
#' @export
sebms_palette <- c("#BE4B48", "#9BBB59") #"#C0504D", 


#' Palette Used in ggplots for Trim Index
#' 
#' @return vector of color hex codes
#' @export
sebms_trimpal <- c("#FFB000", "#648FFF", "#DC267F")



#' Modify y axis Max Value for Trim Plots
#' 
#' Sets the max value and steps on y-axis
#'
#' @param x the max of the trim values 
#'
#' @return a max value, a step value and ??
#' @noRd
yAxisModifier <- function(x) {
  case_when(x < 5 ~ c(4,.5, 8),
            x <10 ~ c(10,2, 5),
            x <15 ~ c(15,3, 5),
            x <20 ~ c(20, 5, 4),
            x <25 ~ c(25, 5, 5),
            x <30 ~ c(30, 5, 6),
            x <40 ~ c(40,10, 4),
            x <50 ~ c(50,10, 5),
            x <100 ~ c(100,20, 5),
            x <250 ~ c(250,50, 5),
            TRUE ~c(500,100, 5))
}



#' Modify y axis Max Value for Indicator Plots
#' 
#' Sets the max value and steps on y-axis
#'
#' @param x the max of the trim values 
#'
#' @return a max value, a step value and ??
#' @noRd
yIndicatorAxisMod <- function(x) {
  case_when(x < 5 ~ c(4, .5, 8),
            x <10 ~ c(10, 2, 5),
            x <15 ~ c(15, 3, 5),
            x <20 ~ c(20, 5, 4),
            x <25 ~ c(25, 5, 5),
            x <30 ~ c(30, 5, 6),
            x <40 ~ c(40, 10, 4),
            x <50 ~ c(50, 10, 5),
            x <100 ~ c(100, 20, 5),
            x <150 ~ c(160, 20,5),
            x <250 ~ c(250, 50, 5),
            TRUE ~c(500, 100, 5))
}




#' Save Plots as png Files with Given File Name and Name Extension
#'
#' Saves a ggplot object as a PNG file, resizing using pixel dimensions and a
#' text scaling factor. I also adds given file name and file name extension,
#' such as a weather variable.
#'
#' @param plot a ggplot object
#' @param filename the path to the output file
#' @param width pixel width
#' @param height pixel height
#' @param text.factor text scaling factor (default is 3)
#' @param weathervar which weather variable it should put in the name; 'Temp' or
#'   'Precip'
#' @importFrom ggplot2 ggsave
#' @importFrom glue glue
#' @export
sebms_ggsave <- function(plot, filename, width = 12.67, height = 9.722, text.factor = 3, weathervar = "Temp") 
{
  dpi <- text.factor * 100
  width.calc <- width #/ dpi
  height.calc <- height # / dpi
  ggsave(filename = glue("{filename}_{weathervar}.png"), plot = plot,
         device = "png", dpi = dpi, width = width.calc, height = height.calc, units = 'cm')
}
