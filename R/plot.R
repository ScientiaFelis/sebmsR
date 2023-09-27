
#' Cumulative Species List Plots
#' 
#' Produce a plot with number of individuals per species a given year
#' 
#' @param year year to use for plot
#' @param Län character; which county you want the data from
#' @param Landskap character; which region you want the data from
#' @param Kommun character; which municipality you want the data from
#' @param database logical; if the data should be based on the sebms database
#'
#' @import dplyr
#' @importFrom plyr round_any
#' @import glue
#' @import ggplot2
#' @return a list with two ggplot objects, named p1 and p2
#' @export
#' 
sebms_specieslist_cum_plots <- function(year = 2021, Län = ".", Landskap = ".", Kommun = ".", database = TRUE) {
  
  if (database) {
    sp <- sebms_species_count_filtered(year = year, Län = Län, Landskap = Landskap, Kommun = Kommun) %>%
      group_by(art) %>%
      summarise(count = as.double(sum(antal, na.rm = T)), .groups = "drop") 
    
    #if (sp$count > 200) {
    
    s1 <- sp %>% 
      filter(count >= median(count)) 
    
    s2 <- sp %>% 
      filter(count < median(count), !str_detect(art, "[Nn]oll")) 
    # }
    
  }else {
    # n <- nrow(sebms_data_specieslist_cum)
    
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
        axis.text = element_text(color = "black", family = "Arial"),
        plot.margin = margin(r=5, unit = "mm"),
        panel.grid.major.x = element_blank(), #element_line(color = "darkgrey", size = 0.3),
        #panel.grid.minor.x = element_line(color = "darkgray"),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.title.x.top = element_text(size = 10, margin = margin(b=5, unit = "mm")),
        # adjust X-axis labels; also adjust their position using margin (acts like a bounding box)
        # using margin was needed because of the inwards placement of ticks
        axis.text.x.top = element_text(size = 8, margin = margin(t = 0, r = 0, b = 3, l = 0, unit = "mm")),
        axis.ticks.x.top = element_line(color = "darkgray"),
        axis.ticks.length.x = unit(-1, "mm"),
        axis.text.y = element_text(size = 10, margin = margin(0,2,0,0, unit = "mm")),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = "darkgray", size = 0.35),
        plot.title = element_text(hjust = 0.5, size = 6, margin = margin(0,0,2,0, unit = "mm"))) 
  }
  
  insert_minor <- function(major_labs, n_minor) {
    labs <- c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
    labs[1:(length(labs)-n_minor)]
  }
  
  #QUESTION: Is this the correct steps?
  acc <- case_when(max(sp$count) >4000 ~ 2000,
                   between(max(sp$count), 1000,4000) ~ 500,
                   TRUE ~ 100)
  maxlim <- round_any(max(sp$count), accuracy = acc, ceiling)
  
  tickmarks1 <- length(unique(s1$art))-0.5
  tickmarks2 <- length(unique(s2$art))-0.5
  
  p1 <- s1 %>%  
    ggplot(aes(y = reorder(art, count), x = count)) +
    geom_col(color = sebms_palette[2], fill = sebms_palette[2], width = 0.5) +
    geom_text(aes(label = count), colour = "grey10", hjust = -0.2, size = 2.5) +
    geom_vline(xintercept = seq(0,maxlim, acc), colour = "darkgrey") +
    geom_segment(aes(y = stage(reorder(art, count), after_scale = seq(0.5, tickmarks1,1)),
                     yend = stage(reorder(art, count), after_scale = seq(0.5, tickmarks1,1)),
                     x = -maxlim*0.005,
                     xend = 0),
                 linewidth = 0.5,
                 colour = "darkgrey") +
    scale_x_continuous(#breaks = seq(0,12000, 2000),
      #labels = seq(0,12000, 2000),
      breaks = seq(0,maxlim, acc/4),
      labels = insert_minor(c(acc*0:(maxlim/acc)), 3),
      position = "top",
     # limits = c(0, maxlim),
      expand = c(0, 0)
    ) +
    scale_y_discrete(expand = c(0.017,0.017)) +
    coord_cartesian(xlim = c(0,maxlim), clip = "off") +
    labs(x = "Antal individer", y = NULL) +
    theme_sebms2()
  
  p2 <- s2 %>% 
    ggplot(aes(y = reorder(art, count), x = count)) +
    geom_col(color = sebms_palette[2], fill = sebms_palette[2], width = 0.5) +
    geom_text(aes(label = count), colour = "grey10", hjust = -0.5, size = 2.5) +
    geom_vline(xintercept = seq(0,maxlim, acc), colour = "darkgrey") +
    geom_segment(aes(y = stage(reorder(art, count), after_scale = seq(0.5, tickmarks2,1)),
                     yend = stage(reorder(art, count), after_scale = seq(0.5, tickmarks2,1)),
                     x = -maxlim*0.005,
                     xend = 0),
                 linewidth = 0.5,
                 colour = "darkgrey") +
    labs(x = "Antal individer", y = NULL) +
    scale_x_continuous(#breaks = seq(0,12000, 2000),
      #labels = seq(0,12000, 2000),
      breaks = seq(0,maxlim, acc/4),
      labels = insert_minor(c(acc*0:(maxlim/acc)), 3),
      position = "top",
  #  limits = c(0, maxlim),
      expand = c(0, 0)
    )  +
    scale_y_discrete(expand = c(0.017,0.017)) +
    coord_cartesian(xlim = c(0,maxlim), clip = "off") +
    theme_sebms2()
  
  res <- list(p1 = p1, p2 = p2)
  name <- list(glue("Öv200_{year}"), glue("Und200_{year}"))
  map2(res, name, ~sebms_ggsave(.x, "Species_tot_count", width = 22, height=32, weathervar = .y))
  
  return(res)
}

#' Butterfly Number Histogram  Plot
#' 
#' Show the number of found butterflies per week, compared between two years.
#' 
#' @inheritParams sebms_specieslist_cum_plots
#' 
#' @import dplyr
#' @importFrom plyr round_any
#' @import ggplot2
#' @importFrom lubridate month weeks ymd
#' @export
#' 
sebms_species_count_histo_plot <- function(year = 2021:2022, Län = ".", Landskap = ".", Kommun = ".", database = TRUE) {
  
  if (database) {
    df <- sebms_species_count_filtered(year = year, Län = Län, Landskap = Landskap, Kommun = Kommun) %>%
      mutate(year = as.factor(year(datum)), vecka = isoweek(datum)) %>%
      filter(datum > ymd(glue("{year}-04-01")), datum < ymd(glue("{year}-09-30"))) %>% 
      group_by(year, vecka) %>%
      summarise(count = as.double(sum(antal, na.rm = T)), .groups = "drop")
  }else {
    df <- 
      sebms_data_species_histo %>%
      group_by(artnamn, vecka) %>%
      summarise(count = sum(sumval))
  }
  
  
  # This makes a label that have a row of weeks and then a row of months in text 
  fmt_label <- function(w) {
    
    # se_months <- c(
    #   "januari", "februari", "mars",
    #   "april", "maj", "juni",
    #   "juli","augusti", "september",
    #   "oktober", "november", "december")
    
    if_else(is.na(lag(w)) | !month(ymd("2021-01-01") + weeks(lag(w))) == month(ymd("2021-01-01") + weeks(w)), 
            paste0(sprintf("%2i", w), "\n      ", month(ymd("2021-01-01") + weeks(w), label = T, abbr = F, locale = "sv_SE.UTF-8")),
            paste(w))
  }
  
  #QUESTION: Is this the correct steps?
  
  steps <- case_when(max(df$count) < 12 ~ 1,
                     between(max(df$count),12,30) ~ 2,
                     between(max(df$count),30,60) ~ 5,
                     between(max(df$count),60,100) ~ 10,
                     between(max(df$count),110,300) ~ 20,
                     between(max(df$count),300,600) ~ 50,
                     between(max(df$count),600,1000) ~ 100,
                     between(max(df$count),1000,5000) ~ 200,
                     TRUE ~1000)
  # 
  # steps <- case_when(max(df$count) <600 ~ 100,
  #                    between(max(df$count), 600,10000) ~ 10,
  #                    between(max(df$count), 10001,40000) ~ 100,
  #                    TRUE ~ 20)
  
  acc <- case_when(between(max(df$count), 1000,4000) ~ 500,
                   max(df$count) >4000 ~ 2000,
                   TRUE ~ 10)
  
  maxlim <- round_any(max(df$count), acc, f = ceiling) # Makes a rounded to nearest 1000 of max value to be at top of Y-axis
  Hweeklim <- max(df$vecka)
  Lweeklim <- min(df$vecka)
  
  p <- 
    ggplot(data = df, 
           aes(x = vecka, y = count, fill = year)) +
    geom_bar(stat = 'identity', position = position_dodge(), width = 0.7) +
    scale_y_continuous(limits = c(0, max(10, maxlim)), # Set Y-axis limits to 10 or the max value of the butterfly count
                       # labels = seq(0,max(df$count)*1.2, 10^ceiling(log10(max(df$count)/100))*2), # Set labels from 0 to max of count
                       breaks = seq(0,max(10, maxlim), steps), #10^ceiling(log10(max(df$count)/(steps)))*2), # Make breaks at even 200 0r 2000 marks depending on max number
                       expand = c(0,0.05)) +
    #expand_limits(y=max(df$count)*1.1) +
    scale_x_continuous(
      breaks = c(10, Lweeklim:Hweeklim),
      labels = c("Vecka: ", fmt_label(Lweeklim:Hweeklim)),
      limits = c(Lweeklim - 0.5, Hweeklim + 0.4), 
      expand = c(0, 0) 
    ) +
    scale_fill_manual("Year", values = c(sebms_palette[1], sebms_palette[2])) +
    labs(y = "Antal individer", x = NULL, tag = "Vecka:") +
    theme_sebms(y_title_sz = 16) +
    theme(panel.grid.major.y = element_line(color = "gray"),
          panel.border = element_rect(colour = "black", linewidth = 0.8),
          axis.ticks.x = element_line(color = "gray5"),
          axis.ticks.length = unit(0, "cm"),
          axis.text.x = element_text(hjust = 0.5, face = "bold", margin = margin(t=3, unit = "mm"), lineheight = 1.3),
          axis.text.y = element_text(face = "bold", margin = margin(r=4, unit = "mm")),
          axis.line = element_line(color = "gray5", linewidth = 0.3),
          plot.margin = margin(t=2, r=7, b=2, l=1, unit = "mm"),
          plot.title = element_text(hjust = 0.5),
          plot.tag = element_text(vjust = 0),
          plot.tag.position = c(0.06, 0.039))
  
  yearname <- paste0(year, collapse = ":")
  sebms_ggsave(p, "Butterflynumber", width = 28, height = 16, weathervar = yearname)
  
  return(p)
}


#' Species Histogram Plot
#' 
#' Show the number of individuals per week of a given species and year.
#' 
#' @inheritParams sebms_specieslist_cum_plots
#' @param Art The species id of interest
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom lubridate month weeks ymd
#' @export
#' 
sebms_species_histo_plot <- function(year = 2021, Art = 1:200, Län = ".", Landskap = ".", Kommun = ".", database = TRUE) {
  
  if (database) {
    df <- sebms_species_count_filtered(year = year, Art = Art, Län = Län, Landskap = Landskap, Kommun = Kommun) %>% 
      #filter(str_detect(art, Art)) %>% 
      filter(#speuid %in% Art, # Make the filter in the SQL query instead
        !str_detect(art, "[Nn]oll"), 
        !speuid %in% c(131,133)) %>% 
      mutate(vecka = isoweek(datum)) %>% 
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
  
  Lweeklim <- min(isoweek(glue("{year}-04-01")))
  Hweeklim <- max(isoweek(glue("{year}-09-30")))
  
  # Plotting function that make all limit and steps per species
  plotSP <- function(df){
    
    # This makes a rounded to nearest 10, 100 or 1000 of max value to be at top of Y-axis to align with gridline at top
    maxlim <-  case_when(max(df$count) < 100 ~ round_any(max(df$count), 10, f = ceiling),
                         max(df$count) < 1000 ~ round_any(max(df$count), 100, f = ceiling),
                         max(df$count) < 10000 ~ round_any(max(df$count), 200, f = ceiling),
                         max(df$count) > 10000 ~ round_any(max(df$count), 1000, f = ceiling)
    )
    
    # This makes the steps right between labels
    steps <- case_when(max(df$count) < 12 ~ 1,
                       between(max(df$count),12,30) ~ 2,
                       between(max(df$count),30,60) ~ 5,
                       between(max(df$count),60,100) ~ 10,
                       between(max(df$count),110,300) ~ 20,
                       between(max(df$count),300,600) ~ 50,
                       between(max(df$count),600,1000) ~ 100,
                       between(max(df$count),1000,5000) ~ 200,
                       TRUE ~1000)
    
    ggplot(data = df, 
           aes(x = vecka, y = count)) +
      geom_bar(stat = 'identity', color = sebms_palette[2], fill = sebms_palette[2], width = 0.5) +
      scale_y_continuous(limits = c(0, maxlim), #nice_lim(df$count),#c(0, max(10, max(df$count)*1.2)), # Set Y-axis limits to 10 or the max value of the butterfly count
                         # labels = seq(0,max(df$count)*1.2, 10^ceiling(log10(max(df$count)/100))*2), # Set labels from 0 to max of count
                         breaks = seq(0,maxlim, steps), #10^ceiling(log10(max(df$count)/100))*2), # 
                         expand = c(0,0)) +
      #expand_limits(y=max(df$count)*1.1) +
      scale_x_continuous(
        breaks = c(10, seq(Lweeklim,Hweeklim,2)),
        labels = c("Vecka: ", fmt_label(seq(Lweeklim,Hweeklim,2))),
        limits = c(Lweeklim - 0.5, Hweeklim + 0.4), 
        expand = c(0, 0) 
      ) + 
      labs(y = "Antal", x = NULL, tag = "Vecka:") +
      theme_sebms(y_title_sz = 18, fontfamily = "Arial") +
      theme(panel.grid.major.y = element_line(color = "gray"),
            axis.ticks.x = element_line(color = "gray5"),
            axis.ticks.length = unit(0, "cm"),
            axis.text.x = element_text(hjust = 0.5, face = "bold", margin = margin(t=4, unit = "mm"), family = "Arial", size = 14, lineheight = 1.3),
            axis.text.y = element_text(face = "bold", margin = margin(r=4, unit = "mm")),
            axis.line = element_line(color = "gray5", linewidth = 0.3),
            plot.title = element_text(hjust = 0.5),
            plot.tag = element_text(vjust = 0, size = 14),
            plot.tag.position = c(0.05, 0.039))
  }  
  
  ggs <- df %>% 
    nest(.by = art) %>% 
    mutate(plots = map(data, ~plotSP(df=.x)))
  
  map2(ggs$plots, ggs$art, ~sebms_ggsave(.x, filename = .y, width = 26, height = 12, weathervar = year))
  
  #sebms_ggsave(p, Art, width = 26, height = 12, weathervar = year)
  return(ggs$plots)
}


#' Species per Site and Site Type Plot
#' 
#' Show the number of sites within a range of species richness found at the site. Also show the mean number of species per site in each site type.
#'  
#' @inheritParams sebms_specieslist_cum_plots
#'  
#' @import dplyr
#' @import forcats
#' @import ggplot2
#' @export
#' 
sebms_species_per_sitetype_plot <- function(year = 2021,  Län = ".", Landskap = ".", Kommun = ".", database = TRUE) {
  
  b <- seq(1, 50, by = 5)
  l <- paste0(b, "-", b + 4)
  
  if (database) {
    
    df <- sebms_species_site_count_filtered(year = year, Län = Län, Landskap = Landskap, Kommun = Kommun) %>% 
      group_by(situid, lokalnamn, sitetype) %>% 
      summarise(species = n_distinct(speuid), .groups = "drop") %>% # Number of species per site and site type 
      group_by(sitetype) %>% 
      mutate(medel = mean(species)) %>% # mean number of species per site type
      ungroup() %>% 
      mutate(interval = l[findInterval(species, b)],
             sortorder = findInterval(species, b)) %>%
      group_by(interval, sortorder, sitetype) %>%
      summarize(site_count = n_distinct(situid),
                medel = mean(medel), .groups = "drop") %>%
      arrange(sortorder) %>%
      select(interval, sortorder, sitetype, site_count, medel) %>% 
      complete(interval, sitetype) %>%
      mutate(site_count=replace_na(site_count, 0)) %>%
      group_by(interval) %>%
      fill(sortorder, .direction = "updown") %>%
      group_by(sitetype) %>%
      fill(medel, .direction = "down") %>%
      ungroup()
    
  }else{
    df <- sebms_data_species_per_site_sitetype %>%
      mutate(
        interval = l[findInterval(species, b)], 
        sortorder = findInterval(species, b)) %>%
      group_by(interval, sortorder, sitetype) %>%
      summarize(site_count = n_distinct(id), .groups = "drop") %>%
      arrange(-desc(sortorder)) %>%
      select(interval, sortorder, sitetype, site_count)
  }
  
  
  options(OutDec = ",") # Set decimal separator to comma
  
  lab <- df %>% distinct(sitetype, medel) # unique mean labels
  
  # insert_minor <- function(major_labs, n_minor) {
  #   labs <- c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  #   labs[1:(length(labs)-n_minor)]
  # } # insert minor ticks without labels
  # 
  # Another try to get tickmarks between bars
  # df$x1 <- as.integer(as.factor(df$interval))
  # x_tick <- c(0, unique(df$x1)) + 0.5
  # len <- length(x_tick)
  # 
  # r <- rbind(unique(df$interval),matrix(rep(c(""), 10),ncol=length(unique(df$interval))))
  # labname <- c("", r)
  tickmarks <- (df %>% distinct(interval, sitetype) %>% pull(interval) %>% length()) /2 +0.5
  
  p <- df %>% 
    ggplot(aes(x = fct_reorder(interval, sortorder), y = site_count)) +
    geom_col(aes(fill = forcats::fct_rev(sitetype)), 
             position = position_dodge(preserve = "single"), width = 0.7) +
    stat_summary(aes(x = l[findInterval(medel, b)], y = 104, colour = sitetype, fill = sitetype),
                 position = position_dodge(width = 1.1),
                 fun = "mean",
                 geom = "point",
                 size = 4,
                 shape = 25) +
    geom_text(aes(x = l[findInterval(medel, b)], y = 113, label = format(round(medel, 1), nsmall = 1)),
              data = lab,
              position = position_dodge2(width = 1.3),
              fontface = "bold",
              inherit.aes = F) +
    geom_segment(aes(x = stage(reorder(interval, sortorder), after_scale = rep(seq(1.5, tickmarks,1), each=2)),
                     xend = stage(reorder(interval, sortorder), after_scale = rep(seq(1.5, tickmarks,1), each=2)),
                     y = -1,
                     yend = 0)) + # Making segments between groups on x-axis.
    scale_y_continuous(breaks = seq(0,120,20),
                       labels = seq(0,120,20),
                       #limits = c(0, 120),
                       expand = c(0, 0)) +
    coord_cartesian(ylim = c(0,120), clip = "off") +
    # scale_x_discrete(breaks = sort(c(unique(df$x1), x_tick)),
    #                  labels = labname) +
    scale_fill_manual("Metod", values = c("P" = sebms_palette[2], "T" = sebms_palette[1])) +
    scale_colour_manual("Metod", values = c("P" = sebms_palette[2], "T" = sebms_palette[1])) +
    labs(x = "Antal arter på lokalen", y = "Antal lokaler") +
    theme_sebms(fontfamily = "Arial") +
    theme(panel.grid.major.y = element_line(color = "gray"),
          axis.ticks.x =  element_blank(),#element_line(linewidth = 1, colour = "black"),
          axis.ticks.y = element_blank(),
          axis.line = element_line(color = "gray5", linewidth = 0.21),
          axis.text = element_text(face = "bold"),
          #axis.text.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0, unit = "mm")),
          axis.title.x = element_text(margin = margin(t = 9)),
          panel.border = element_rect(colour = "black", linewidth = 1)
    )
  
  sebms_ggsave(p, "Species_per_site", width = 16, height = 12, weathervar = year)
  return(p)
  
  options(OutDec = ".") # Restore decimal separator to dot
  
}
