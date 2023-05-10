# TODO FIX THE FUNCTION BELOW THAT PRODUCE A FIGURE IN THE RMD 

#' Plot of temperature and precipitation data for 2015
#' 
#' @return a ggplot plot object
#' @import ggplot2
#' @importFrom purrr map map2
#' @importFrom magick image_append image_read
#' @export
sebms_precip_temp_2015_plot <- function() {
  sebms_precip_temp_plot()
}

#' Plot of temperature and precipitation data
#' 
#' @param filter_cities a vector of cities, if missing a default of c("Umeå", "Stockholm", "Visby", "Lund") is used
#' @param df_precip a data frame with precipitation data in a specific format, see vignette for details, default uses packaged data sebms_data_precip_2015
#' @param df_temp a data frame with temperature data in a the same format, see vignette for details, default uses packaged data sebms_data_temp_2015
#' @return a ggplot plot object
#' @import ggplot2
#' @importFrom purrr map map2
#' @importFrom magick image_append image_read
#' @export
sebms_precip_temp_plot <- function(filter_cities, df_precip, df_temp) {
  
  if (missing(filter_cities))
    filter_cities <- c("Umeå", "Stockholm", "Visby", "Lund")
  
  if (missing(df_precip))
    df_precip <- sebms_data_precip_2015
  
  if (missing(df_temp))
    df_temp <- sebms_data_temp_2015
  
  save_pplot <- function(g, fn, w = 24, h = 18) {
    ggsave(
      filename = fn, 
      plot = g, device = "png", 
      width = w, height = h, units = "cm")
  }
  
  tempfile_png <- function(prefix) {
    file.path(dirname(tempdir()), 
              paste0(prefix, basename(tempfile(fileext = ".png"))))
  }
  
  fn_temp <- map_chr(rep("temp-", length(filter_cities)), tempfile_png)
  tplots <- map(filter_cities, function(x) sebms_temp_plot(df_temp, x))
  map2(tplots, fn_temp, save_pplot)
  
  fn_precip <- map_chr(rep("precip-", length(filter_cities)), tempfile_png)
  pplots <- map(filter_cities, function(x) sebms_precip_plot(df_precip, x))
  map2(pplots, fn_precip, save_pplot)
  
  stack_right <- image_append(stack = TRUE, image_read(fn_precip))
  stack_left <- image_append(stack = TRUE, image_read(fn_temp))
  
  stack <- image_append(c(
    stack_left,
    stack_right
  ))
  
  unlink(c(fn_temp, fn_precip))
  
  stack
  
}




#QUESTION: What to do with the following funtions()

#' Cumulative specielist plots
#' @import dplyr
#' @import ggplot2
#' @return a list with two ggplot objects, named p1 and p2
#' @export
#' 
sebms_specieslist_cum_plots <- function() {
  
  n <- nrow(sebms_data_specieslist_cum)
  col_palette <- sebms_palette
  
  s1 <- 
    sebms_data_specieslist_cum %>% 
    filter(count >= 200)
  #  slice(1 : floor(n/2))
  
  s2 <- 
    sebms_data_specieslist_cum %>% 
    filter(count < 200)
  #  slice(-c(1 : floor(n/2)))
  
  p1 <- 
    ggplot(data = s1, 
           aes(x = reorder(name, count), y = count)) +
    geom_bar(stat='identity', color = col_palette[1], 
             fill = col_palette[1], width = 0.5) +
    geom_text(aes(label = count), colour = "grey10", hjust = -0.5, size = 2.5) +
    xlab("") + ylab("") +
    ggtitle("Antal individer (n >= 200)") +
    scale_y_continuous(breaks = c(1000 * 1:12), labels = c(1000 * 1:11, ""),
                       position = "top", limits = c(0, 12000), expand = c(0, 0)) +
    theme_sebms() +
    theme(
      panel.grid.major.x = element_line(color = "darkgray"),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.text.x.top = element_text(color = "darkgray"),
      axis.ticks.y = element_blank(),
      axis.line = element_line(color = "darkgray"),
      plot.title = element_text(hjust = 0.5)) +
    coord_flip()
  
  p2 <- 
    ggplot(data = s2, 
           aes(x = reorder(name, count), y = count)) +
    geom_bar(stat='identity', color = col_palette[1], fill = col_palette[1], width = 0.5) +
    geom_text(aes(label = count), colour = "grey10", hjust = -0.5, size = 2.5) +
    xlab("") + ylab("") +
    ggtitle("Antal individer (n < 200)") +
    scale_y_continuous(breaks = c(20 * 1:11), labels = c(20 * 1:10, ""),
                       position = "top", limits = c(0, 210), expand = c(0, 0)) +
    theme_sebms() +
    theme(panel.grid.major.x = element_line(color = "darkgray"),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.text.x.top = element_text(color = "darkgray"),
          axis.ticks.y = element_blank(),
          axis.line = element_line(color = "darkgray"),
          plot.title = element_text(hjust = 0.5)) +
    coord_flip()
  
  res <- list(p1 = p1, p2 = p2)
  
}

#' Species histo plot
#' @import dplyr
#' @import ggplot2
#' @export
#' 
sebms_species_histo_plot <- function() {
  
  col_palette <- sebms_palette
  
  df <- 
    sebms_data_species_histo %>%
    group_by(artnamn, vecka) %>%
    summarise(count = sum(sumval))
  
  p <- ggplot(data = df, 
              aes(x = vecka, y = count)) +
    geom_bar(stat = 'identity', color = col_palette[1], fill = col_palette[1], width = 0.5) +
    xlab("") + ylab("") +
    scale_y_continuous(breaks = c(10 * 1:10), limits = c(0, 100), expand = c(0, 0)) +
    scale_x_continuous(
      minor_breaks = c(14:42), 
      breaks = c(13, 18, 22, 26, 31, 35, 40), 
      labels = c("april", "maj", "juni", "juli", "augusti", "september", "oktober"),
      limits = c(12, 42), expand = c(0, 0), 
      sec.axis = sec_axis( ~ ., breaks = c(12:42), 
                           labels = c("", paste0("", c(13:41)), ""))) +
    theme_sebms() +
    theme(panel.grid.major.y = element_line(color = "gray"),
          panel.grid.minor.x = element_line(color = "gray"),
          panel.grid.major.x = element_line(color = "gray40"),
          axis.ticks.x = element_line(color = "gray5"),
          axis.line = element_line(color = "gray5"),
          plot.title = element_text(hjust = 0.5))
  
  return (p)
}

#' Species histo plot - original version
#' 
#' This plot should probably be parameterized with the week range (defaulted to 13..42) used for the x-axis and with the relevant year context, also the range of y-axis should not be hardcoded in the future.
#' @import dplyr
#' @import ggplot2
#' @importFrom lubridate month weeks ymd
#' @export
#' 
sebms_species_histo_plot_orig <- function() {
  
  df <- 
    sebms_data_species_histo %>%
    group_by(artnamn, vecka) %>%
    summarise(count = sum(sumval))
  
  #df$count <- as.integer(floor(runif(7, 0, 10)))
  
  col_palette <- sebms_palette
  
  fmt_label <- function(w) {
    
    se_months <- c(
      "januari", "februari", "mars",
      "april", "maj", "juni",
      "juli","augusti", "september",
      "oktober", "november", "december")
    
    if_else(is.na(lag(w)) | !month(ymd("2015-01-01") + weeks(lag(w))) == month(ymd("2015-01-01") + weeks(w)), 
            paste0(sprintf("%2i", w), "\n", se_months[month(ymd("2015-01-01") + weeks(w))]), 
            paste(w))
  }
  
  p <- 
    ggplot(data = df, 
           aes(x = vecka, y = count)) +
    geom_bar(stat = 'identity', color = col_palette[1], fill = col_palette[1], width = 0.5) +
    xlab("") + ylab("") +
    scale_y_continuous(limits = c(0, max(10, df$count)), expand = c(0, 0.6)) +
    scale_x_continuous(
      #minor_breaks = c(10, 13:42), 
      #breaks = c(13, 18, 22, 26, 31, 35, 40),
      breaks = c(10, 13:42),
      labels = c("Vecka: ", fmt_label(13:42)),
      limits = c(10, 43), 
      expand = c(0, 0) 
      #sec.axis = sec_axis( ~ ., breaks = c(12:42), labels = c("", paste0("v", c(13:41)), "")
    ) + 
    #annotate("text", x = 12, y = 0, label = "Vecka", size = 4) + 
    theme_sebms() +
    theme(panel.grid.major.y = element_line(color = "gray"),
          # panel.grid.minor.x = element_line(color = "gray"),
          # panel.grid.major.x = element_line(color = "gray40"),
          axis.ticks.x = element_line(color = "gray5"),
          axis.ticks.length = unit(0, "cm"),
          #  axis.text.x = element_text(hjust = 0),
          axis.line = element_line(color = "gray5"),
          plot.title = element_text(hjust = 0.5))
  
  return (p)
  # remove clipping of x axis labels
  #g <- ggplot_gtable(ggplot_build(p))
  #g$layout$clip[g$layout$name == "panel"] <- "off"
  #grid::grid.draw(g)
}


#' Species per site and sitetype histo plot
#' @import dplyr
#' @import ggplot2
#' @export
#' 
sebms_species_per_site_sitetype_plot <- function() {
  
  b <- seq(1, 50, by = 5)
  l <- paste0(b, "-", b + 4)
  
  sebms_spss <- 
    sebms_data_species_per_site_sitetype %>%
    mutate(
      interval = l[findInterval(species, b)], 
      sortorder = findInterval(species, b)) %>%
    group_by(interval, sortorder, sitetype) %>%
    summarize(site_count = n_distinct(id)) %>%
    arrange(-desc(sortorder)) %>%
    dplyr::select(interval, sortorder, sitetype, site_count)
  
  col_palette <- sebms_palette
  
  #sbm_spss$sitetype <- factor(sbm_spss$sitetype)
  
  ggplot(data = sebms_spss, aes(x = reorder(interval, sortorder), 
                                y = site_count, fill = sitetype)) +
    geom_bar(aes(fill = sitetype), stat = "identity", 
             position = position_dodge(), width = 0.7) +
    xlab("Antal olika arter på lokalen") + ylab("Antal lokaler") +
    scale_y_continuous(breaks = c(10 * 1:10), limits = c(0, 100), expand = c(0, 0)) +
    scale_fill_manual("Metod", values = c("P" = col_palette[1], "T" = col_palette[2])) +
    theme_sebms() +
    theme(panel.grid.major.y = element_line(color = "gray"),
          panel.grid.minor.x = element_line(color = "gray"),
          axis.ticks.x = element_blank(),
          axis.line = element_line(color = "gray5"),
          plot.title = element_text(hjust = 0.5))
  
}
