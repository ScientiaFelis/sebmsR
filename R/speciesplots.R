
#' Individuals per Species Plots
#'
#' Produce a plot with number of individuals per species a given year
#'
#' @param year year or years to use for plot
#' @param Län character or regular expression; which county you want the data from
#' @param Region character or reg ex; which region do you want. Possible values are:
#'   'SGot', 'OGot', 'VGotSve', 'OSve', 'NSveNor', 'NNor'.
#' @param Landskap character or reg ex; which region you want the data from
#' @param Kommun character or reg ex; which municipality you want the data from
#' @param filepath a path to a folder where the plot file should be saved
#' @param tag a tag to be added at the end of the file name; optional. For instance to
#'   show the county the plot or data contain.
#' @param verification a verification code that filter out verified occurenses of species,
#'   default to 109.
#' @param source the database sources as id numbers, defaults to `54,55,56,63,64,66,67,84`
#' @param print logical; if FALSE (default) the function does not print to plot window.
#'
#' @importFrom plyr round_any
#' @importFrom glue glue
#' @import dplyr
#' @importFrom forcats fct_reorder
#' @import ggplot2
#' @importFrom stringr str_detect
#' @importFrom purrr map2
#' @importFrom stats median
#'
#' @return Two png figures with the abundance data for each species. One for species count
#'   below the median for that year and one for species above median.
#' @export
#'
sebms_abundance_per_species_plot <- function(year = 2021, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filepath = getwd(), tag = NULL, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), print = FALSE) {

  sp <- sebms_species_count_filtered(year = year, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
    filter(!speuid %in% c(131,132,133,139,135)) %>%
    group_by(art) %>%
    summarise(count = as.double(sum(antal, na.rm = T)), .groups = "drop")

  # Split data on the median to get to pngs that can be inserted in the report
  s1 <- sp %>%
    filter(count >= median(count))
  s2 <- sp %>%
    filter(count < median(count), !str_detect(art, "[Nn]oll"))

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
        axis.line = element_line(color = "darkgray", linewidth = 0.35))
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
    ggplot(aes(y = fct_reorder(art, count), x = count)) +
    geom_vline(xintercept = seq(0,maxlim, acc), colour = "darkgrey") +
    geom_col(color = sebms_palette[2], fill = sebms_palette[2], width = 0.5) +
    geom_text(aes(label = count), colour = "grey10", hjust = -0.2, size = 3) +
    geom_segment(aes(y = stage(fct_reorder(art, count), after_scale = seq(0.5, tickmarks1,1)),
                     yend = stage(fct_reorder(art, count), after_scale = seq(0.5, tickmarks1,1)),
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
    ggplot(aes(y = fct_reorder(art, count), x = count)) +
    geom_vline(xintercept = seq(0,maxlim, acc), colour = "darkgrey") +
    geom_col(color = sebms_palette[2], fill = sebms_palette[2], width = 0.5) +
    geom_text(aes(label = count), colour = "grey10", hjust = -0.5, size = 3) +
    geom_segment(aes(y = stage(fct_reorder(art, count), after_scale = seq(0.5, tickmarks2,1)),
                     yend = stage(fct_reorder(art, count), after_scale = seq(0.5, tickmarks2,1)),
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

  if (all(Län == ".", Region == ".", Landskap == ".",Kommun == ".")) {
    origin = "Sweden" # If no region was selected use Sweden
  }else {
    origin <- glue("{Län}{Region}{Landskap}{Kommun}") %>% str_remove_all("\\.") %>% str_replace_all(" ", "-") # If any region was chosen, add that to origin
  }

  if (is.null(tag)) {
    tag = ""

  }
  yearname <- glue("{origin}_{year}{tag}")

  res <- list(p1 = p1, p2 = p2) # list with the two plots
  name <- list(glue("Above-median_{yearname}"), glue("Below-median_{yearname}"))

  # fix filepath
  filepath <- normalizePath(filepath)
  filepath <- glue("{filepath}/Species_tot_count")

  walk2(res, name, ~sebms_ggsave(.x, filepath, width = 22, height = 32, weathervar = .y, text.factor = 4), .progress = "Saving images...")

  if (print) {
    return(res)
  }
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
sebms_abundance_year_compare_plot <- function(year = 2021:2022, Art = 1:200, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filepath = getwd(), tag = NULL, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), print = FALSE) {

  if (length(year) > 2) {
    return(cat("More than two year in interval.\n\nGIVE ONLY TWO YEARS TO COMPARE!")
    )
    stop()
  }

  df <- sebms_species_count_filtered(year = year, Art = Art, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
    mutate(year = as.factor(year(datum)), vecka = isoweek(datum)) %>%
    filter(datum > ymd(glue("{year}-04-01")), datum < ymd(glue("{year}-09-30"))) %>%
    #filter(!speuid %in% c(131,133,135)) %>%
    group_by(year, vecka) %>%
    summarise(count = as.double(sum(antal, na.rm = T)), .groups = "drop")


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
  maxlim <-  case_when(max(df$count) <= 9 ~ 10,
                       between(max(df$count), 9,19) ~ 20,
                       between(max(df$count), 20,28) ~ 30,
                       between(max(df$count), 29,47) ~ 50,
                       between(max(df$count), 48,95) ~ 100,
                       between(max(df$count), 96,190) ~ 200,
                       between(max(df$count), 191,275) ~ 300,
                       between(max(df$count), 276,475) ~ 500,
                       between(max(df$count), 476,750) ~ 800,
                       between(max(df$count), 476,950) ~ 1000,
                       between(max(df$count), 951,1350) ~ 1400,
                       between(max(df$count), 1351,1900) ~ 2000,
                       between(max(df$count), 1901,2900) ~ 3000,
                       between(max(df$count), 2901,3750) ~ 4000,
                       between(max(df$count), 3751,4500) ~ 5000,
                       between(max(df$count), 4501,5500) ~ 6000,
                       between(max(df$count), 5501,6500) ~ 7000,
                       between(max(df$count), 6501,7500) ~ 8000,
                       between(max(df$count), 7501,9000) ~ 10000,
                       between(max(df$count), 9001,11000) ~ 12000,
                       between(max(df$count), 11001,13000) ~ 14000,
                       between(max(df$count), 13001,15000) ~ 16000,
                       between(max(df$count), 15001,17000) ~ 18000,
                       between(max(df$count), 17001,19000) ~ 20000,
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
                     between(max(df$count), 191,475) ~ 50,
                     between(max(df$count), 476,950) ~ 100,
                     between(max(df$count), 951,2900) ~ 200,
                     between(max(df$count), 2901,4500) ~ 500,
                     between(max(df$count), 4501,8000) ~ 1000,
                     between(max(df$count), 8001,18000) ~ 2000,
                     between(max(df$count), 18001,20000) ~ 5000,
                     TRUE ~10000)
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
    theme(plot.margin = margin(t=2.5, r=4, b=2, l=1, unit = "mm"),
          axis.line = element_line(color = "gray5", linewidth = 0.3),
          axis.text.y = element_text(size = 18),
          plot.tag.position = c(0.05,0.075)
    )

  if (all(Län == ".", Region == ".", Landskap == ".",Kommun == ".")) {
    origin = "Sweden" # If no region was selected use Sweden
  }else {
    origin <- glue("{Län}{Region}{Landskap}{Kommun}") %>% str_remove_all("\\.") %>% str_replace_all(" ", "-") # If any region was chosen, add that to origin
  }

  if (is.null(tag)) {
    tag = ""

  }
  yearname <- paste0(year, collapse = "-")
  yearname <- glue("{origin}_{yearname}{tag}")



  # fix filepath
  filepath <- normalizePath(filepath)
  filepath <- glue("{filepath}/Butterflynumber")

  sebms_ggsave(p, filepath, width = 30, height = 15, weathervar = yearname)

  if (print) {
    return(p)
  }

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
sebms_species_abundance_plot <- function(year = 2021, Art = 1:200, Län = ".", Region = ".", Landskap = ".", Kommun = ".", plotname = FALSE, filepath = getwd(), tag = NULL, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), print = FALSE) {

  df <- sebms_species_count_filtered(year = year, Art = Art, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
    filter(!str_detect(art, "[Nn]oll"), speuid != 132) %>%
    mutate(antal = as.double(antal)) %>%
    group_by(art, vecka = isoweek(datum)) %>%
    summarise(count = sum(antal, na.rm = T), .groups = "drop") %>%
    mutate(art = str_replace(art, "/", "_"))

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
                         between(max(df$count), 20,28) ~ 30,
                         between(max(df$count), 29,47) ~ 50,
                         between(max(df$count), 48,95) ~ 100,
                         between(max(df$count), 96,190) ~ 200,
                         between(max(df$count), 191,275) ~ 300,
                         between(max(df$count), 276,475) ~ 500,
                         between(max(df$count), 476,750) ~ 800,
                         between(max(df$count), 476,950) ~ 1000,
                         between(max(df$count), 951,1350) ~ 1400,
                         between(max(df$count), 1351,1900) ~ 2000,
                         between(max(df$count), 1901,2900) ~ 3000,
                         between(max(df$count), 2901,3750) ~ 4000,
                         between(max(df$count), 3751,4500) ~ 5000,
                         between(max(df$count), 4501,5500) ~ 6000,
                         between(max(df$count), 5501,6500) ~ 7000,
                         between(max(df$count), 6501,7500) ~ 8000,
                         between(max(df$count), 7501,9000) ~ 10000,
                         between(max(df$count), 9001,11000) ~ 12000,
                         between(max(df$count), 11001,13000) ~ 14000,
                         between(max(df$count), 13001,15000) ~ 16000,
                         between(max(df$count), 15001,17000) ~ 18000,
                         between(max(df$count), 17001,19000) ~ 20000,
                         TRUE ~ 40000
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
                       between(max(df$count), 191,475) ~ 50,
                       between(max(df$count), 476,950) ~ 100,
                       between(max(df$count), 951,2200) ~ 200,
                       between(max(df$count), 2201,4500) ~ 500,
                       between(max(df$count), 4501,8000) ~ 1000,
                       between(max(df$count), 8001,18000) ~ 2000,
                       between(max(df$count), 18001,20000) ~ 5000,
                       TRUE ~ 10000)
    #FIXME: Add species name as title or subtitle instead of inside plot
    ggplot(data = df,
           aes(x = vecka, y = count)) +
      geom_col(color = sebms_palette[2], fill = sebms_palette[2], width = 0.6) +
      #geom_text(aes(x = Lweeklim+1, y = maxlim*0.99,label = unique(Art)), family = "Arial", size = 4, hjust = 0, vjust = 1, check_overlap = T) +
      scale_y_continuous(limits = c(0, maxlim), #nice_lim(df$count),#c(0, max(10, max(df$count)*1.2)), # Set Y-axis limits to 10 or the max value of the butterfly count
                         # labels = seq(0,max(df$count)*1.2, 10^ceiling(log10(max(df$count)/100))*2), # Set labels from 0 to max of count
                         labels = function(x) formatC(x, width = 4, format = "d"),
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
      theme_sebms_species(x_sz = 20, y_sz = 20) +
      theme(axis.ticks = element_blank(),
            axis.line = element_line(color = "gray5",
                                     linewidth = 0.3),
            plot.tag.position = c(0.04, 0.097),
            plot.tag = element_text(family = "Arial", size = 18),
            axis.text.y = element_text(family = "Arial"), #Arial mono is it possible?
            axis.text.x = element_text(family = "Arial"))
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

  if (length(year)>1) {
    yearname <- paste0(min(year),"-",max(year))
  }else {
    yearname <- year
  }

   if (all(Län == ".", Region == ".", Landskap == ".",Kommun == ".")) {
        origin = "Sweden" # If no region was selected use Sweden
      }else {
        origin <- glue("{Län}{Region}{Landskap}{Kommun}") %>% str_remove_all("\\.") %>% str_replace_all(" ", "-") # If any region was chosen, add that to origin
      }

  if (is.null(tag)) {
    tag = ""

  }
  yearname <- glue("{origin}_{yearname}{tag}")


  # fix filepath
  filepath <- normalizePath(filepath)

  map2(ggs$plots, ggs$art, ~sebms_ggsave(.x, filename = glue("{filepath}/{.y}"), width = 24, height = 14, weathervar = yearname), .progress = "Saving individual species plots as png.....")

  #sebms_ggsave(p, Art, width = 26, height = 12, weathervar = year)
  if (print) {
    return(ggs$plots)
  }

}


#' Number of Sites per Species Number Category and Site Type
#'
#' Show the number of sites within a range of species richness found at the
#' site. Also show the mean number of species per site in each site type.
#'
#' @inheritParams sebms_abundance_per_species_plot
#'
#' @import dplyr
#' @importFrom forcats fct_reorder fct_rev
#' @import ggplot2
#'
#' @returns A png with the number of sites within each category of number of
#'   species, for both slingor and transects. It also shows the mean number of
#'   species per site type.
#' @export
#'
sebms_species_per_sitetype_plot <- function(year = 2021,  Län = ".", Region = ".", Landskap = ".", Kommun = ".", filepath = getwd(), tag = NULL, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84), print = FALSE) {

  b <- seq(1, 65, by = 5) # make the start of species number groups
  l <- paste0(b, "-", b + 4) # This maes the group intervals

  df <- sebms_species_site_count_filtered(year = year, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
    group_by(situid, sitetype) %>%
    mutate(speuid = case_when(speuid == 131 ~ if_else(any(speuid %in% c(57,58)),NA_integer_, 131),
                              speuid == 132 ~ if_else(any(speuid %in% c(72,73,74)),NA_integer_, 132),
                              speuid == 133 ~ if_else(any(speuid %in% c(24,25)),NA_integer_, 133),
                              speuid == 139 ~ if_else(any(speuid %in% c(29,30)),NA_integer_, 139),
                              TRUE ~speuid)) %>%
    filter(!is.na(speuid)) %>%
    mutate(speuid = if_else(speuid == 135, NA_integer_, speuid)) %>% # Sätter nollobs til NA så de inte räknas i artmångfaldsmått
    group_by(situid, sitetype) %>%
    summarize(species = n_distinct(speuid, na.rm = T), .groups = "drop") %>% # Number of species per site and site type
    group_by(sitetype) %>%
    mutate(medel = mean(species)) %>% # mean number of species per site type
    ungroup() %>%
    #filter(species != 0) %>%  #REMOVE to get a zero species category OBS: add the all.inside=T in the interval calc below also
    mutate(interval = l[findInterval(species, b, all.inside = T)], #, all.inside = T
           sortorder = findInterval(species, b),
           interval = if_else(sortorder == 0, "0", interval)
    ) %>%
    group_by(interval, sortorder, sitetype, medel) %>%
    summarize(site_count = n_distinct(situid),
              #medel = mean(medel),
              .groups = "drop") %>%
    select(interval, sortorder, sitetype, site_count, medel) %>%
    complete(interval, sitetype, fill = list(site_count = 0)) %>%
    group_by(interval) %>%
    fill(sortorder, .direction = "updown") %>%
    group_by(sitetype) %>%
    fill(medel, .direction = "downup") %>%
    ungroup() %>%
    group_by(interval, sitetype) %>%
    summarise(sortorder = first(sortorder),
              medel = max(medel),
              site_count = sum(site_count), .groups = "drop")

  options(OutDec = ",") # Set decimal separator to comma. Perhaps test `withr::local_options(list(OutDec=",))`


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


  # This makes groups of max limits and set the maxlimit of figure based on max values of data
  # If value fall above a certain threshold it is set at a certain maxlimit

  maxlim <-  case_when(max(df$site_count) <= 7 ~ 10,
                       between(max(df$site_count), 8,15) ~ 20,
                       between(max(df$site_count), 16,28) ~ 40,
                       between(max(df$site_count), 29,40) ~ 50,
                       between(max(df$site_count), 41,90) ~ 100,
                       between(max(df$site_count), 91,190) ~ 200,
                       between(max(df$site_count), 191,275) ~ 300,
                       between(max(df$site_count), 276,475) ~ 500,
                       between(max(df$site_count), 476,750) ~ 800,
                       between(max(df$site_count), 476,950) ~ 1000,
                       between(max(df$site_count), 951,1350) ~ 1400,
                       between(max(df$site_count), 1351,1900) ~ 2000,
                       between(max(df$site_count), 1901,2900) ~ 3000,
                       between(max(df$site_count), 2901,3750) ~ 4000,
                       between(max(df$site_count), 3751,4500) ~ 5000,
                       between(max(df$site_count), 4501,5500) ~ 6000,
                       between(max(df$site_count), 5501,6500) ~ 7000,
                       between(max(df$site_count), 6501,7500) ~ 8000,
                       between(max(df$site_count), 7501,9000) ~ 10000,
                       between(max(df$site_count), 9001,11000) ~ 12000,
                       between(max(df$site_count), 11001,13000) ~ 14000,
                       between(max(df$site_count), 13001,15000) ~ 16000,
                       between(max(df$site_count), 15001,17000) ~ 18000,
                       between(max(df$site_count), 17001,19000) ~ 20000,
                       TRUE ~10000
  )

  # This makes the steps between labels correct based on max value of count.
  steps <- case_when(max(df$site_count) < 10 ~ 1,
                     between(max(df$site_count), 9,15) ~ 2,
                     between(max(df$site_count), 16,47) ~ 5,
                     between(max(df$site_count), 48,95) ~ 10,
                     between(max(df$site_count), 96,190) ~ 20,
                     between(max(df$site_count), 191,475) ~ 50,
                     between(max(df$site_count), 476,950) ~ 100,
                     between(max(df$site_count), 951,2900) ~ 200,
                     between(max(df$site_count), 2901,4500) ~ 500,
                     between(max(df$site_count), 4501,8000) ~ 1000,
                     between(max(df$site_count), 8001,18000) ~ 2000,
                     between(max(df$site_count), 18001,20000) ~ 5000,
                     TRUE ~10000)

  # Make tickmarks for x-axis
  tickmarks <- (df %>%
                  distinct(interval, sitetype) %>%
                  pull(interval) %>%
                  length()) /2 +0.5 # Produce the correct number of tick marks

  tri_y <- max(df$site_count) * 1.1  #maxlim-(steps*1.8)
  text_y <- max(df$site_count) * 1.165 #maxlim-steps*1.4

  p <- df  %>%
    mutate(interval = fct_reorder(interval, sortorder)) %>%
    arrange(sortorder) %>%
    ggplot(aes(x = interval, y = site_count)) +
    geom_col(aes(fill = forcats::fct_rev(sitetype)),
             position = position_dodge(preserve = "single"), width = 0.7) + # make the bars of the site types beside each other for each group.
    stat_summary(aes(x = l[findInterval(medel+0.2, b)], y = tri_y, colour = sitetype, fill = sitetype),
                 position = position_dodge2(width = 1.1),
                 fun = "mean",
                 geom = "point",
                 size = 5,
                 shape = 25) + # This creates the triangles at the right place on the x- and y-axis.
    geom_text(aes(x = l[findInterval(medel+0.2, b)], y = text_y, label = format(round(medel, 1), nsmall = 1)),
              data = lab,
              position = position_dodge2(width = 1.4),
              fontface = "plain",
              size = 5,
              inherit.aes = F) + # This creates the numbers of the average species number at the right place (group) on the x-axis and y-axis.
    geom_segment(aes(x = stage(fct_reorder(interval, sortorder),
                               after_scale = rep(seq(1.5, tickmarks,1),each = 2)), # This adds a segment, that looks like a tick mark, after each group
                     xend = stage(fct_reorder(interval, sortorder),
                                  after_scale = rep(seq(1.5, tickmarks,1), each = 2)),
                     y = -maxlim * 0.011, # How long the tick mark is, we want them negative as they should go below x-axis and based on how large the max y-axis is.
                     yend = 0)) + # The start of the tick mark
    scale_y_continuous(breaks = seq(0,maxlim,steps),
                       labels = seq(0,maxlim,steps),
                       #limits = c(0, 120),
                       expand = c(0, 0)) + # This distribute values at 'steps' interval up to 'maxlim'
    coord_cartesian(ylim = c(0,maxlim), clip = "off") + # This sets the y-axis at 0 to 'maxlim' and clip = "off" makes it possible to set x-axis ticks below the axis
    # scale_x_discrete(breaks = sort(c(unique(df$x1), x_tick)),
    #                  labels = labname) +
    scale_fill_manual("Metod", values = c("P" = sebms_palette[2], "T" = sebms_palette[1])) +
    scale_colour_manual("Metod", values = c("P" = sebms_palette[2], "T" = sebms_palette[1])) +
    labs(x = "Antal arter på lokalen", y = "Antal lokaler") +
    theme_sebms_species()

  if (length(year)>1) {
    yearname <- paste0(min(year),"-",max(year))
  }else {
    yearname <- year
  }

   if (all(Län == ".", Region == ".", Landskap == ".",Kommun == ".")) {
        origin = "Sweden" # If no region was selected use Sweden
      }else {
        origin <- glue("{Län}{Region}{Landskap}{Kommun}") %>% str_remove_all("\\.") %>% str_replace_all(" ", "-") # If any region was chosen, add that to origin
      }

  if (is.null(tag)) {
    tag = ""

  }
  yearname <- glue("{origin}_{yearname}{tag}")

   # fix filepath
  filepath <- normalizePath(filepath)
  filepath <- glue("{filepath}/Species_per_site")

  sebms_ggsave(p, filepath, width = 22, height = 13, weathervar = yearname)

  if (print) {
    return(p)

  }

  options(OutDec = ".") # Restore decimal separator to dot

}
