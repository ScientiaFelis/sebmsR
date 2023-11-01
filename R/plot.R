
#' Individuals per Species Plots
#'
#' Produce a plot with number of individuals per species a given year
#'
#' @param year year to use for plot
#' @param Län character or regular expression; which county you want the data
#'   from
#' @param Landskap character or reg ex; which region you want the data from
#' @param Kommun character or reg ex; which municipality you want the data from
#' @param database logical; if the data should be based on the sebms database
#' @param source the database sources as id numbers,
#' defaults to `54,55,56,63,64,66,67,84`
#'
#' @importFrom plyr round_any
#' @importFrom glue glue
#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_detect
#' @importFrom purrr map2
#'
#' @return Two png figures with the abundance data for each species. One for
#'   species count below the median for that year and one for species above
#'   median.
#' @export
#' 
sebms_abundance_per_species_plot <- function(year = 2021, Län = ".", Landskap = ".", Kommun = ".", database = TRUE, source = c(54,55,56,63,64,66,67,84)) {
  
  if (database) {
    sp <- sebms_species_count_filtered(year = year, Län = Län, Landskap = Landskap, Kommun = Kommun, source = source) %>%
      group_by(art) %>%
      summarise(count = as.double(sum(antal, na.rm = T)), .groups = "drop")
    
    # Split data on the median to get to pngs that can be inserted in the report
    s1 <- sp %>%
      filter(count >= median(count))
    s2 <- sp %>%
      filter(count < median(count), !str_detect(art, "[Nn]oll"))
    
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
    theme_sebms_species() +
      theme(
        plot.margin = margin(r=5, unit = "mm"),
        plot.title = element_text(hjust = 0.5, size = 6, margin = margin(0,0,2,0, unit = "mm")),
        panel.grid.major.x = element_blank(), #element_line(color = "darkgrey", size = 0.3),
        #panel.grid.minor.x = element_line(color = "darkgray"),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.title.x.top = element_text(family = "Arial",
                                        face = "bold",
                                        size = 10,
                                        colour = "black",
                                        margin = margin(b=5, unit = "mm")),
        # adjust X-axis labels; also adjust their position using margin (acts like a bounding box)
        # using margin was needed because of the inwards placement of ticks
        axis.text.x.top = element_text(family = "Arial",
                                       face = "plain",
                                       size = 10,
                                       colour = "black",
                                       margin = margin(t = 0, r = 0, b = 3, l = 0, unit = "mm")),
        axis.text.y = element_text(family = "Arial",
                                   face = "plain",
                                   size = 10,
                                   colour = "black",
                                   margin = margin(0,2,0,0, unit = "mm")),
        axis.ticks.x.top = element_line(color = "darkgray"),
        axis.ticks.length.x = unit(-1, "mm"),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = "darkgray", size = 0.35)) 
  }
  
  # A function that makes tickmarks without labels between the labels.
  insert_minor <- function(major_labs, n_minor) {
    labs <- c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
    labs[1:(length(labs)-n_minor)]
  }
  
  # Make accurate distances between x-axis numbers based on max counts
  acc <- case_when(max(sp$count) >4000 ~ 2000,
                   between(max(sp$count), 1000,4000) ~ 500,
                   TRUE ~ 100)
  maxlim <- round_any(max(sp$count), accuracy = acc, ceiling)
  
  # To make tickmarks between the species on y-axis by using geom_segment().
  tickmarks1 <- length(unique(s1$art))-0.5
  tickmarks2 <- length(unique(s2$art))-0.5
  
  p1 <- s1 %>%  
    ggplot(aes(y = reorder(art, count), x = count)) +
    geom_vline(xintercept = seq(0,maxlim, acc), colour = "darkgrey") +
    geom_col(color = sebms_palette[2], fill = sebms_palette[2], width = 0.5) +
    geom_text(aes(label = count), colour = "grey10", hjust = -0.2, size = 3) +
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
    geom_vline(xintercept = seq(0,maxlim, acc), colour = "darkgrey") +
    geom_col(color = sebms_palette[2], fill = sebms_palette[2], width = 0.5) +
    geom_text(aes(label = count), colour = "grey10", hjust = -0.5, size = 3) +
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
  
  # Make correct file name for png.
  res <- list(p1 = p1, p2 = p2)
  name <- list(glue("Above-median_{year}"), glue("Below-median_{year}"))
  map2(res, name, ~sebms_ggsave(.x, "Species_tot_count", width = 22, height=32, weathervar = .y, text.factor = 4))
  
  return(res)
}

#' Butterfly Number Two Year Comparison Plot
#'
#' Show the number of found butterflies per week, compared between two years.
#'
#' @inheritParams sebms_abundance_per_species_plot
#' @param year two years to compare, e.g. 2021:2022
#'
#' @importFrom plyr round_any
#' @import dplyr
#' @import ggplot2
#' @importFrom lubridate month weeks ymd
#'
#' @return A png figure with the number of individuals found each of the
#'   comparing years per week,
#' @export
#' 
sebms_abundance_year_compare_plot <- function(year = 2021:2022, Län = ".", Landskap = ".", Kommun = ".", database = TRUE, source = c(54,55,56,63,64,66,67,84)) {
  
  if (database) {
    df <- sebms_species_count_filtered(year = year, Län = Län, Landskap = Landskap, Kommun = Kommun, source = source) %>%
      mutate(year = as.factor(year(datum)), vecka = isoweek(datum)) %>%
      filter(datum > ymd(glue("{year}-04-01")), datum < ymd(glue("{year}-09-30"))) %>% 
      filter(!speuid %in% c(131,133,135)) %>% 
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
            paste0(sprintf("%2i", w), "\n   ", month(ymd("2021-01-01") + weeks(w), label = T, abbr = T, locale = "sv_SE.UTF-8")),
            paste(w))
  }
  # Hard coded labesl instead
  veckamån <- c("13\n   apr","14", "15", "16","17", "18\n   maj", "19", "20", "21","22\n   jun", "23", "24","25", "26\n   jul","27", "28","29", "30\n   aug","31","32","33","34\n   sep","35","36","37","38\n  okt", "39","40")
  
  # To produce the correct steps betweeen y-axis number.
  steps <- case_when(max(df$count) < 12 ~ 1,
                     between(max(df$count),12,30) ~ 2,
                     between(max(df$count),31,60) ~ 5,
                     between(max(df$count),61,100) ~ 10,
                     between(max(df$count),101,300) ~ 20,
                     between(max(df$count),301,600) ~ 50,
                     between(max(df$count),601,1000) ~ 100,
                     between(max(df$count),1001,5000) ~ 200,
                     TRUE ~2000)
  # 
  # steps <- case_when(max(df$count) <600 ~ 100,
  #                    between(max(df$count), 600,10000) ~ 10,
  #                    between(max(df$count), 10001,40000) ~ 100,
  #                    TRUE ~ 20)
  
  acc <- case_when(between(max(df$count), 1000,4000) ~ 500,
                   max(df$count) >4000 ~ 2000,
                   TRUE ~ 10)
  
  maxlim <- round_any(max(df$count), acc, f = ceiling) # Makes a rounded to nearest 1000 of max value to be at top of Y-axis
  Hweeklim <- 40 #max(df$vecka)
  Lweeklim <- 13 #min(df$vecka)
  
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
      breaks = c(Lweeklim:Hweeklim),
      labels = c(fmt_label(Lweeklim:Hweeklim)),
      #labels = c(veckamån),
      limits = c(Lweeklim - 0.5, Hweeklim + 0.4), 
      expand = c(0, 0) 
    ) +
    scale_fill_manual("Year", values = c(sebms_palette[1], sebms_palette[2])) +
    labs(y = "Antal individer", x = NULL, tag = "Vecka:") +
    theme_sebms_species() +
    theme(plot.margin = margin(t=2, r=7, b=2, l=1, unit = "mm"),
          axis.line = element_line(color = "gray5", linewidth = 0.3),
          axis.text.y = element_text(size = 18),
          plot.tag.position = c(0.05,0.075)
    )
  
  yearname <- paste0(year, collapse = ":")
  sebms_ggsave(p, "Butterflynumber", width = 30, height = 15, weathervar = yearname)
  
  return(p)
}


#' Species Histogram Plot
#'
#' Show the number of individuals per week of a given species and year.
#'
#' @inheritParams sebms_abundance_per_species_plot
#' @param Art integer; the species id of interest
#' @param plotname logical; if you want the species name inside the plot
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom lubridate month weeks isoweek ymd
#' @importFrom stringr str_detect
#' @importFrom tidyr nest
#' @importFrom purrr map2
#'
#' @return A png per species showing the number of individuals per week.
#' @export
#' 
sebms_species_abundance_plot <- function(year = 2021, Art = 1:200, Län = ".", Landskap = ".", Kommun = ".", plotname = FALSE, database = TRUE, source = c(54,55,56,63,64,66,67,84)) {
  
  if (database) {
    df <- sebms_species_count_filtered(year = year, Art = Art, Län = Län, Landskap = Landskap, Kommun = Kommun, source = source) %>% 
      filter(!str_detect(art, "[Nn]oll"), 
             !speuid %in% c(131,133)) %>%
      mutate(antal = as.double(antal)) %>% 
      group_by(art, vecka = isoweek(datum)) %>%
      summarise(count = sum(antal, na.rm = T), .groups = "drop")
  }else {
    df <- 
      sebms_data_species_histo %>%
      group_by(artnamn, vecka) %>%
      summarise(count = sum(sumval))
  }
  
  # Week / month label to get label of month below first week in month.
  fmt_label <- function(w) {
    
    # se_months <- c(
    #   "januari", "februari", "mars",
    #   "april", "maj", "juni",
    #   "juli","augusti", "september",
    #   "oktober", "november", "december")
    
    if_else(is.na(lag(w)) | !month(ymd("2021-01-01") + weeks(lag(w))) == month(ymd("2021-01-01") + weeks(w)), 
            paste0(sprintf("%2i", w), "\n  ", month(ymd("2021-01-01") + weeks(w), label = T, abbr = T, locale = "sv_SE.UTF-8")),
            paste(w))
  }
  
  # Hard coded labesl instead
  veckamån <- c("14\n   apr", "16","18\n   maj","20", "22\n   jun","24", "26\n   jul","28", "30\n   aug","32","34\n   sep","36","38\n  okt", "40")
  
  # Make week limits
  # QUESTION: add filter of week in df instead?
  Lweeklim <- 14 #min(isoweek(glue("{year}-04-01")))
  Hweeklim <- 40 #max(isoweek(glue("{year}-09-30")))
  
  # Plotting function, making all limit and steps per species
  plotSP <- function(df, Art){
    
    # This makes groups of max limits and set the maxlimit of figure based on max values of data
    # If value fall above a certain threshold it is set at a certain maxlimit
    
    maxlim <-  case_when(max(df$count) <= 9 ~ 10,
                         between(max(df$count), 9,19) ~ 20,
                         between(max(df$count), 20,47) ~ 50,
                         between(max(df$count), 48,95) ~ 100,
                         between(max(df$count), 96,190) ~ 200,
                         between(max(df$count), 191, 475) ~ 500,
                         between(max(df$count), 476, 950) ~ 1000,
                         between(max(df$count), 951,1350) ~ 1400,
                         between(max(df$count), 1351,1900) ~ 2000,
                         between(max(df$count), 1901,2900) ~ 3000,
                         between(max(df$count), 2901,4500) ~ 5000,
                         TRUE ~10000
    )
    
    # maxlim <- if_else(between(max(df$count), 10,12), maxlim-6, maxlim)
    # # Fix odd number which does not fit in 20 steps
    # maxlim <- if_else(maxlim %in% seq(130, 290, 20), maxlim+10, maxlim)
    # This makes the steps between labels correct based on max value of count.
    steps <- case_when(max(df$count) < 10 ~ 1,
                       between(max(df$count), 9,19) ~ 2,
                       between(max(df$count), 20,47) ~ 5,
                       between(max(df$count), 48,95) ~ 10,
                       between(max(df$count), 96,190) ~ 20,
                       between(max(df$count), 191, 475) ~ 50,
                       between(max(df$count), 476, 950) ~ 100,
                       between(max(df$count), 951,1350) ~ 200,
                       between(max(df$count), 1351,1900) ~ 200,
                       between(max(df$count), 1901,2900) ~ 200,
                       between(max(df$count), 2901,4500) ~ 200,
                       TRUE ~500)
    #FIXME: Add species name as title or subtitle instead of inside plot
    ggplot(data = df, 
           aes(x = vecka, y = count)) +
      geom_col(color = sebms_palette[2], fill = sebms_palette[2], width = 0.6) +
      #geom_text(aes(x = Lweeklim+1, y = maxlim*0.99,label = unique(Art)), family = "Arial", size = 4, hjust = 0, vjust = 1, check_overlap = T) +
      scale_y_continuous(limits = c(0, maxlim), #nice_lim(df$count),#c(0, max(10, max(df$count)*1.2)), # Set Y-axis limits to 10 or the max value of the butterfly count
                         # labels = seq(0,max(df$count)*1.2, 10^ceiling(log10(max(df$count)/100))*2), # Set labels from 0 to max of count
                         labels = function(x) formatC(x, width = 4),
                         breaks = seq(0,maxlim, steps), #10^ceiling(log10(max(df$count)/100))*2), # 
                         expand = c(0,0)) +
      #expand_limits(y=max(df$count)*1.1) +
      scale_x_continuous(
        breaks = c(seq(Lweeklim,Hweeklim,2)),
        #labels = c(fmt_label(seq(Lweeklim,Hweeklim,2))),
        labels = veckamån,
        limits = c(Lweeklim - 0.5, Hweeklim + 0.4), 
        expand = c(0, 0) 
      ) +
      labs(y = "Antal", x = NULL, tag = "Vecka:", title = glue::glue("{unique(Art)}")) +
      theme_sebms_species() +
      theme(axis.ticks = element_blank(),
            axis.line = element_line(color = "gray5",
                                     linewidth = 0.3),
            plot.tag.position = c(0.05, 0.08),
            axis.text.y = element_text(family = "Arial")) #Arial mono ist it possible?
  }  
  
  ## Add Species name to plot if requested
  if (plotname) {
    
    ggs <- df %>% 
      nest(.by = art) %>% 
      mutate(plots = map2(data, art, ~plotSP(df=.x, Art = .y), .progress = "Creating individual species plots...."))
  }else {
    ggs <- df %>% 
      nest(.by = art) %>% 
      mutate(plots = map(data, ~plotSP(df=.x, Art = NULL), .progress = "Creating individual species plots...."))
    
  }
  
  map2(ggs$plots, ggs$art, ~sebms_ggsave(.x, filename = .y, width = 24, height = 14, weathervar = year), .progress = "Saving individual species plots as png.....")
  
  #sebms_ggsave(p, Art, width = 26, height = 12, weathervar = year)
  return(ggs$plots)
}


#' Number of Sites per Species Number Category and Site Type
#'
#' Show the number of sites within a range of species richness found at the
#' site. Also show the mean number of species per site in each site type.
#'
#' @inheritParams sebms_abundance_per_species_plot
#'
#' @import dplyr
#' @import forcats
#' @import ggplot2
#'
#' @returns A png with the number of sites within each category of number of
#'   species, for both slingor and transects. It also shows the mean number of
#'   species per site type.
#' @export
#' 
sebms_species_per_sitetype_plot <- function(year = 2021,  Län = ".", Landskap = ".", Kommun = ".", database = TRUE, source = c(54,55,56,63,64,66,67,84)) {
  
  b <- seq(1, 50, by = 5) # make the start of species number groups
  l <- paste0(b, "-", b + 4) # This maes the group intervals
  
  if (database) {
    
    df <- sebms_species_site_count_filtered(year = year, Län = Län, Landskap = Landskap, Kommun = Kommun, source = source) %>% 
      # Om art per site innehåller någon av agg (131-133 + 139), finns någon av arterna, gör då om till den arten, annars första arten i agg.
      mutate(groupid = case_when(speuid %in% c(131,58,57) ~ 1,
                                 speuid %in% c(139,29,30) ~ 2,
                                 speuid %in% c(133,24,25) ~ 3,
                                 speuid %in% c(132,74,73,72) ~ 4,
                                 TRUE ~ 5)) %>% 
      group_by(groupid, situid, sitetype) %>% 
      mutate(speuid = case_when(speuid == 131 ~ if_else(any(speuid %in% c(57,58)),NA_integer_, 131),
                                speuid == 132 ~ if_else(any(speuid %in% c(72,73,74)),NA_integer_, 132),
                                speuid == 133 ~ if_else(any(speuid %in% c(24,25)),NA_integer_, 133),
                                speuid == 139 ~ if_else(any(speuid %in% c(29,30)),NA_integer_, 139),
                                TRUE ~speuid)) %>% 
      # ungroup() %>%
      arrange(situid, groupid, speuid) %>% 
      fill(speuid, .direction = "down") %>% #Fyller NA med det värdet över
      ungroup() %>% 
      mutate(speuid = if_else(speuid == 135, NA_integer_, speuid)) %>% # Sätter nollobs til NA så de inte räknas i artmångfaldsmått
      group_by(situid, sitetype) %>% 
      summarize(species = n_distinct(speuid, na.rm = T), .groups = "drop") %>% # Number of species per site and site type 
      group_by(sitetype) %>% 
      mutate(medel = mean(species)) %>% # mean number of species per site type
      ungroup() %>% 
      mutate(interval = l[findInterval(species, b, all.inside = T)], #QUESTION: should we set 0 sites as category 1-5
             sortorder = findInterval(species, b),
             interval = if_else(sortorder == 0, "0", interval)) %>%
      group_by(interval, sortorder, sitetype, medel) %>%
      summarize(site_count = n_distinct(situid),
                #medel = mean(medel),
                .groups = "drop") %>%
      arrange(sortorder) %>%
      select(interval, sortorder, sitetype, site_count, medel) %>% 
      complete(interval, sitetype, fill = list(site_count = 0)) %>%
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
  
  
  tickmarks <- (df %>%
                  distinct(interval, sitetype) %>% 
                  pull(interval) %>% 
                  length()) /2 +0.5 # Produce the correct numbet of tickmarks
  
  p <- df  %>%
    mutate(interval = fct_reorder(interval, sortorder)) %>% 
    arrange(sortorder) %>% 
    ggplot(aes(x = interval, y = site_count)) +
    geom_col(aes(fill = forcats::fct_rev(sitetype)), 
             position = position_dodge(preserve = "single"), width = 0.7) +
    stat_summary(aes(x = l[findInterval(medel+0.2, b)], y = 104, colour = sitetype, fill = sitetype),
                 position = position_dodge2(width = 1.1),
                 fun = "mean",
                 geom = "point",
                 size = 5,
                 shape = 25) +
    geom_text(aes(x = l[findInterval(medel+0.2, b)], y = 113, label = format(round(medel, 1), nsmall = 1)),
              data = lab,
              position = position_dodge2(width = 1.4),
              fontface = "plain",
              size = 5,
              inherit.aes = F) +
    geom_segment(aes(x = stage(reorder(interval, sortorder),
                               after_scale = rep(seq(1.5, tickmarks,1),each = 2)), # This adds a segment, that looks like a tickmark, after each group
                     xend = stage(reorder(interval, sortorder),
                                  after_scale = rep(seq(1.5, tickmarks,1), each = 2)),
                     y = -1.1, # How long the tickmark is, we want negative as it should go down
                     yend = 0)) + # The start of the tickmark
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
    theme_sebms_species(x_sz = 12, y_sz = 12)
  
  sebms_ggsave(p, "Species_per_site", width = 22, height = 13, weathervar = year)
  return(p)
  
  options(OutDec = ".") # Restore decimal separator to dot
  
}
