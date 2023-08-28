
#' Cumulative Species List Plots
#' 
#' @import dplyr
#' @import ggplot2
#' @return a list with two ggplot objects, named p1 and p2
#' @export
#' 
sebms_specieslist_cum_plots <- function(database = TRUE) {
  
  if (database) {
    sp <- sebms_species_count()
    
    s1 <- sp %>% 
      group_by(art) %>%
      summarise(count = as.double(sum(antal, na.rm = T))) %>% 
      filter(count >= 200) 
    
    s2 <- sp %>% 
      group_by(art) %>%
      summarise(count = as.double(sum(antal, na.rm = T))) %>% 
      filter(count < 200) 
    
    
  }else {
    n <- nrow(sebms_data_specieslist_cum)
    
    s1 <- 
      sebms_data_specieslist_cum %>% 
      filter(count >= 200)
    #  slice(1 : floor(n/2))
    
    s2 <- 
      sebms_data_specieslist_cum %>% 
      filter(count < 200)
    #  slice(-c(1 : floor(n/2)))
  }
  
  # Modify theme
  theme_sebms2 <- function() {
    theme_sebms() +
    theme(
      axis.text = element_text(color = "black"),
      panel.grid.major.x = element_line(color = "darkgrey", size = 0.3),
      #panel.grid.minor.x = element_line(color = "darkgray"),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      # adjust X-axis labels; also adjust their position using margin (acts like a bounding box)
      # using margin was needed because of the inwards placement of ticks
      axis.text.x = element_text(size = 6, margin = unit(c(t = 0, r = 0, b = 0.25, l = 0), "mm")),
      axis.ticks.x.top = element_line(color = "darkgray"),
      axis.ticks.length.x = unit(-1, "mm"),
      axis.text.y = element_text(size = 6, margin = margin(0,0,0,0, unit = "mm")),
      axis.ticks.y = element_blank(),
      axis.line = element_line(color = "darkgray"),
      plot.title = element_text(hjust = 0.5, size = 6, margin = margin(0,0,2,0, unit = "mm"))) 
  }

  # insert_minor <- function(major_labs, n_minor) {
  #   labs <- c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  #   labs[1:(length(labs)-n_minor)]
  # }

  p1 <- s1 %>%  
    ggplot(aes(y = reorder(art, count), x = count)) +
    geom_bar(stat='identity', color = sebms_palette[2], fill = sebms_palette[2], width = 0.3, just = 0.5) +
    geom_text(aes(label = count), colour = "grey10", hjust = -0.5, size = 2.5) +
    labs(title = "Antal individer", x = NULL, y = NULL) +
    scale_x_continuous(breaks = c(2000 * 0:12),
                       labels = c(2000 * 0:11, ""),
                       #labels = insert_minor(c(4000*0:6), 1),
                       position = "top",
                       limits = c(0, 10000),
                       expand = c(0, 0)) +
    scale_y_discrete(expand = c(0.011,0.011)) +
    theme_sebms2()
  
  p2 <- s2 %>% 
    ggplot(aes(y = reorder(art, count), x = count)) +
    geom_bar(stat='identity', color = sebms_palette[2], fill = sebms_palette[2], width = 0.3) +
    geom_text(aes(label = count), colour = "grey10", hjust = -0.5, size = 2.5) +
    labs(title = "Antal individer", x = NULL, y = NULL) +
    scale_x_continuous(breaks = c(20 * 0:11),
                       labels = c(20 * 0:10, ""),
                       position = "top",
                       limits = c(0, 210),
                       expand = c(0, 0)) +
    theme_sebms2()
  
  res <- list(p1 = p1, p2 = p2)
  name <- list("Över200", "Under200")
  map2(res, name, ~sebms_ggsave(.x, "Species_tot_count", width = 12, height=18, weathervar = .y))
  
  return(res)
}

#' Butterfly Number Histogram  Plot
#' 
#' Show the number of found butterflies per week
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom lubridate month weeks ymd
#' @export
#' 
sebms_species_count_histo_plot <- function(year = 2021:2022, database = TRUE) {
  
  if (database) {
    df <- sebms_species_count(year = year) %>%
      group_by(year = as.factor(year(datum)), vecka = week(datum)) %>%
      summarise(count = as.double(sum(antal, na.rm = T)), .groups = "drop")
  }else {
    df <- 
      sebms_data_species_histo %>%
      group_by(artnamn, vecka) %>%
      summarise(count = sum(sumval))
  }
  
  
  
  fmt_label <- function(w) {
    
    # se_months <- c(
    #   "januari", "februari", "mars",
    #   "april", "maj", "juni",
    #   "juli","augusti", "september",
    #   "oktober", "november", "december")
    
    if_else(is.na(lag(w)) | !month(ymd("2021-01-01") + weeks(lag(w))) == month(ymd("2021-01-01") + weeks(w)), 
            paste0(sprintf("%2i", w), "\n   ", month(ymd("2021-01-01") + weeks(w), label = T, abbr = T, locale = "sv_SE.UTF-8")),
            paste(w))
  }
  
  p <- 
    ggplot(data = df, 
           aes(x = vecka, y = count, fill = year)) +
    geom_bar(stat = 'identity', position = position_dodge(), width = 0.7) +
    scale_y_continuous(limits = c(0, max(10, max(df$count)*1.2)), # Set Y-axis limits to 10 or the max value of the butterfly count
                       # labels = seq(0,max(df$count)*1.2, 10^ceiling(log10(max(df$count)/100))*2), # Set labels from 0 to max of count
                       breaks = seq(0,max(df$count)*1.2, 10^ceiling(log10(max(df$count)/100))*2), # 
                       expand = c(0,0)) +
    #expand_limits(y=max(df$count)*1.1) +
    scale_x_continuous(
      breaks = c(10, 14:40),
      labels = c("Vecka: ", fmt_label(14:40)),
      limits = c(13.5, 40), 
      expand = c(0, 0) 
    ) +
    scale_fill_manual("Year", values = c(sebms_palette[1], sebms_palette[2])) +
    labs(y = "Antal individer", x = NULL, tag = "Vecka:") +
    theme_sebms() +
    theme(panel.grid.major.y = element_line(color = "gray"),
          axis.ticks.x = element_line(color = "gray5"),
          axis.ticks.length = unit(0, "cm"),
          axis.text.x = element_text(hjust = 0.5, face = "bold", margin = margin(t=3, unit = "mm")),
          axis.text.y = element_text(face = "bold", margin = margin(r=4, unit = "mm")),
          axis.line = element_line(color = "gray5"),
          plot.title = element_text(hjust = 0.5),
          plot.tag = element_text(vjust = 0),
          plot.tag.position = c(0.09, 0.039))
  
  yearname <- paste0(year, collapse = ":")
  sebms_ggsave(p, "Butterflynumber", width = 22, height = 16, weathervar = yearname)
  
  return(p)
}


#' Species Histogram Plot
#' 
#' Show the number of individuals per week of a given species and year.
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom lubridate month weeks ymd
#' @export
#' 
sebms_species_histo_plot <- function(year = 2021, Art = "Luktgräsfjäril", database = TRUE) {
  
  if (database) {
    df <- sebms_species_count(year = year) %>% 
      filter(str_detect(art, Art)) %>% 
      mutate(vecka = week(datum)) %>% 
      group_by(art, vecka) %>%
      summarise(count = as.double(sum(antal, na.rm = T)), .groups = "drop")
  }else {
    df <- 
      sebms_data_species_histo %>%
      group_by(artnamn, vecka) %>%
      summarise(count = sum(sumval))
  }
  
 
  
  fmt_label <- function(w) {
    
    # se_months <- c(
    #   "januari", "februari", "mars",
    #   "april", "maj", "juni",
    #   "juli","augusti", "september",
    #   "oktober", "november", "december")
    
    if_else(is.na(lag(w)) | !month(ymd("2021-01-01") + weeks(lag(w))) == month(ymd("2021-01-01") + weeks(w)), 
            paste0(sprintf("%2i", w), "\n   ", month(ymd("2021-01-01") + weeks(w), label = T, abbr = T, locale = "sv_SE.UTF-8")),
            paste(w))
  }
  
  p <- 
    ggplot(data = df, 
           aes(x = vecka, y = count)) +
    geom_bar(stat = 'identity', color = sebms_palette[2], fill = sebms_palette[2], width = 0.5) +
    scale_y_continuous(limits = c(0, max(10, max(df$count)*1.2)), # Set Y-axis limits to 10 or the max value of the butterfly count
                       # labels = seq(0,max(df$count)*1.2, 10^ceiling(log10(max(df$count)/100))*2), # Set labels from 0 to max of count
                       breaks = seq(0,max(df$count)*1.2, 10^ceiling(log10(max(df$count)/100))*2), # 
                       expand = c(0,0)) +
    #expand_limits(y=max(df$count)*1.1) +
    scale_x_continuous(
      breaks = c(10, 14:40),
      labels = c("Vecka: ", fmt_label(14:40)),
      limits = c(13.5, 40), 
      expand = c(0, 0) 
    ) + 
    labs(y = "Antal", x = NULL, tag = "Vecka:") +
    theme_sebms() +
    theme(panel.grid.major.y = element_line(color = "gray"),
          axis.ticks.x = element_line(color = "gray5"),
          axis.ticks.length = unit(0, "cm"),
          axis.text.x = element_text(hjust = 0.5, face = "bold", margin = margin(t=3, unit = "mm")),
          axis.text.y = element_text(face = "bold", margin = margin(r=4, unit = "mm")),
          axis.line = element_line(color = "gray5"),
          plot.title = element_text(hjust = 0.5),
          plot.tag = element_text(vjust = 0),
          plot.tag.position = c(0.05, 0.039))
  
  sebms_ggsave(p, Art, width = 22, height = 16, weathervar = "")
  return(p)
}


#' Species per Site and Site Type Plot
#' 
#' Show the number of sites within a range of species number found at the site. Also show the mean number of species per site in each site type.
#'  
#' @import dplyr
#' @import forcats
#' @import ggplot2
#' @export
#' 
sebms_species_per_sitetype_plot <- function(year = 2021, database = TRUE) {
  
  b <- seq(1, 50, by = 5)
  l <- paste0(b, "-", b + 4)
  
  if (database) {
    
    sebms_spss <- sebms_species_site_count(year = year) %>% 
      group_by(situid, lokalnamn, sitetype) %>% 
      summarise(species = n_distinct(speuid), .groups = "drop") %>% 
      group_by(sitetype) %>% 
      mutate(medel = mean(species)) %>% 
      ungroup() %>% 
      mutate(interval = l[findInterval(species, b)],
             sortorder = findInterval(species, b)) %>%
      group_by(interval, sortorder, sitetype) %>%
      summarize(site_count = n_distinct(situid),
                medel = mean(medel), .groups = "drop") %>%
      arrange(-desc(sortorder)) %>%
      select(interval, sortorder, sitetype, site_count, medel)
    
  }else{
    sebms_spss <- 
      sebms_data_species_per_site_sitetype %>%
      mutate(
        interval = l[findInterval(species, b)], 
        sortorder = findInterval(species, b)) %>%
      group_by(interval, sortorder, sitetype) %>%
      summarize(site_count = n_distinct(id), .groups = "drop") %>%
      arrange(-desc(sortorder)) %>%
      select(interval, sortorder, sitetype, site_count)
  }
  
  
  lab <- sebms_spss %>% distinct(sitetype, medel) # unique mean labels
  
  sebms_spss %>% 
    ggplot(aes(x = reorder(interval, sortorder), y = site_count)) +
    geom_bar(aes(fill = forcats::fct_rev(sitetype)), stat = "identity", 
             position = position_dodge(), width = 0.7) +
    stat_summary(aes(x = l[findInterval(medel, b)], y = 102, fill = sitetype), position = position_dodge(width = 0.9), fun = "mean", geom = "point", size = 4, shape = 25) +
    geom_text(aes(x = l[findInterval(medel, b)], y = 107, label = round(medel, 1)), data = lab, position = position_dodge2(width = 1.1), inherit.aes = F) +
    scale_y_continuous(breaks = c(10 * 1:10), limits = c(0, 120), expand = c(0, 0)) +
    scale_fill_manual("Metod", values = c("P" = sebms_palette[2], "T" = sebms_palette[1])) +
    labs(x = "Antal olika arter på lokalen", y = "Antal lokaler") +
    theme_sebms() +
    theme(panel.grid.major.y = element_line(color = "gray"),
          axis.ticks = element_blank(),
          axis.line = element_line(color = "gray5"),
          axis.title.x = element_text(margin = margin(t = 9)),
          panel.border = element_rect(colour = "black", size = 1)
    )
  
}
