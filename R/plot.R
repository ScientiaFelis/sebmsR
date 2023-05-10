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
      strip.text.x = element_text(size = 16),
      legend.position = legend_position
    )
  
  return(theme_sb)
}


## Below are functions that creates the weather figures for the butterfly monitoring reports

#' Extract default Station names and id
#' 
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @import stringr
#' @noRd

sebms_default_station <- function(my_place, tempstat = TRUE) {
  
  if(tempstat){
    my_place <- c("Stock.*Obs.*len$|^Lund$|Visby.*Flyg|Umeå.*Flyg")
    stations <- fromJSON("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22.json")$station %>% 
      filter(str_detect(name, my_place)) %>% 
      select(name, id, latitude, longitude) %>% 
      mutate(id = str_squish(id))
    
  }else{
    my_place <- c("Stock.*Observ.*len$|^Lund$|Visby$|Umeå-Röbäck")
    stations <- fromJSON("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22.json")$station %>% 
      filter(str_detect(name, my_place)) %>% 
      select(name, id, latitude, longitude) %>% 
      mutate(id = str_squish(id))
  }
  
  return(stations)
}


#' Downloads and Exctract Stations Given by User
#' 
#' This takes the given list by users and match only one station to the possible with similar names
#' 
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @import stringr
#' @noRd
sebms_user_station <- function(my_place) {
  
  my_place <- my_place %>% 
    paste0(collapse = "|")
  
  stations <- fromJSON("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22.json")$station %>% 
    filter(str_detect(name, my_place)) %>% 
    select(name, id, latitude, longitude) %>% 
    mutate(id = str_squish(id))
  
  return(stations)
}


#' Download and Filter out Precipitation Data from SMHI
#' @import dplyr
#' @import purrr
#' @import lubridate
#' @import stringr
#' @import ggplot2
#' @noRd
sebms_precip_data <- function(my_place = NA, year = lubridate::year(lubridate::today())-1) {
  
  if(year == lubridate::year(lubridate::today()) & lubridate::month(lubridate::today()) < 11){
    cat("THERE IS NO PRECIPITATION DATA FOR THIS YEAR YET!\n")
    cat("You have to wait until at least NOVEMBER.\n\n")
    return()
  }
  
  if(year > lubridate::year(lubridate::today())){
    cat("YOU ARE WAY AHEAD OF YOURSELF!")
    cat("Chose a year that is not in the future.")
    return()
  }
  
  if(unique(is.na(my_place))){
    stations <- sebms_default_station(my_place, tempstat = FALSE)
  }else{
    stations <- sebms_user_station(my_place)
  }
  
  all_precip <- stations %>% 
    pull(id) %>% 
    set_names() %>% # To keep the id-names of the list
    map(possibly(~read.csv2(str_squish(paste0("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/23/station/",.x,"/period/corrected-archive/data.csv")), skip = 12))) %>% 
    map(possibly(~rename_with(.x, ~c("FrDate", "ToDate", "month", "nb", "Delete", "Delete2", "Delete3")))) %>% # Set column names 
    bind_rows(.id = "id") %>% # .id = "id" keep the id of the station in the dataframe
    as_tibble() %>% 
    select(!starts_with("Delete")) %>% # Remove the columns we do not need
    filter(lubridate::year(ymd_hms(FrDate)) == year,
           # filter(lubridate::year(FrDate) == if_else(lubridate::month(lubridate::today()) < 11,lubridate::year(lubridate::today())-1, lubridate::year(lubridate::today())), ## This filter out the previous year if it is before november, otherwise it take this year. The archives have data upp until three month back, and you want the summer month of a recording year. 
           month(ymd_hms(FrDate)) %in% 4:9) %>% 
    left_join(stations, by = "id") %>% 
    transmute(name, id = as.numeric(id), latitud = latitude, longitud = longitude, month = month(ymd_hms(FrDate), label = T, abbr = T), nb = as.numeric(nb), monthnr = month(ymd_hms(FrDate)), period = "2")
  
  filt_precip <- all_precip  %>%
    mutate(name = str_remove(name, " .*|-.*")) %>% 
    group_by(name) %>% 
    distinct(id) %>% 
    slice(1) %>%
    ungroup() %>% 
    select(-name)
  
  precip <- filt_precip  %>%
    left_join(all_precip, by = "id") %>% 
    bind_rows(norm_precip %>% filter(id %in% c(filt_precip %>% pull(id)))) %>% 
    mutate(name = str_remove(name, " .*|-.*"))
  
  return(precip)
}

#' Download and Filter out Temperature Data from SMHI
#' @import dplyr 
#' @import stringr
#' @import lubridate
#' @import purrr
#' @import ggplot2
#' @noRd
sebms_temp_data <- function(my_place = NA, year = lubridate::year(lubridate::today())-1) {
  
  if(year == lubridate::year(lubridate::today()) & lubridate::month(lubridate::today()) < 11){
    cat("THERE IS NO TEMPERATURE DATA FOR THIS YEAR YET!\n")
    cat("You have to wait until at least NOVEMBER.\n\n")
    return()
  }
  if(year > lubridate::year(lubridate::today())){
    cat("YOU ARE WAY AHEAD OF YOURSELF!")
    cat("Chose a year that is not in the future.")
    return()
  }
  
  if(unique(is.na(my_place))){
    stations <- sebms_default_station(my_place, tempstat = TRUE)
  }else{
    stations <- sebms_user_station(my_place)
  }
  
  all_temp <- stations %>% 
    pull(id) %>% 
    set_names() %>% # To keep the id-names of the list
    map(~read.csv2(str_squish(paste0("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22/station/",.x,"/period/corrected-archive/data.csv")), skip = 12)) %>% 
    map(~rename_with(.x, ~c("FrDate", "ToDate", "month", "temp", "Delete", "Delete2", "Delete3"))) %>% # Set column names 
    bind_rows(.id = "id") %>% # .id = "id" keep the id of the station in the dataframe
    as_tibble() %>% 
    select(!starts_with("Delete")) %>% # Remove the columns we do not need
    filter(lubridate::year(ymd_hms(FrDate)) == year,
           #filter(lubridate::year(FrDate) == if_else(lubridate::month(lubridate::today()) < 11,lubridate::year(lubridate::today())-1, lubridate::year(lubridate::today())), ## This filter out the previous year if it is before november, otherwise it take this year. The archives have data upp until three month back, and you want the summer month of a recording year. 
           month(ymd_hms(FrDate)) %in% 4:9) %>% 
    left_join(stations, by = "id") %>%
    transmute(name, id = as.numeric(id), latitud = latitude, longitud = longitude, month = month(ymd_hms(FrDate), label = T, abbr = T), temp = as.numeric(temp), monthnr = month(ymd_hms(FrDate)), period = "2")
  
  filt_temp <- all_temp  %>%
    mutate(name = str_remove(name, " .*|-.*")) %>% 
    group_by(name) %>% 
    distinct(id) %>% 
    slice(1) %>%
    ungroup() %>% 
    select(-name)
  
  temp <- filt_temp  %>%
    left_join(all_temp, by = "id") %>% 
    bind_rows(norm_temp %>% filter(id %in% c(filt_temp %>% pull(id)))) %>% 
    mutate(name = str_remove(name, " .*|-.*"))
  
  return(temp) 
}


#' Palette used in plots
#' @return vector of color hex codes
#' @export
sebms_palette <- c("#BE4B48", "#9BBB59") #"#C0504D", 


#' Make a ggplot from Precipitation Data
#' 
#' This takes a dataframe from sebms_precip_data and makes a precipitation figure from that 
#' 
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' 
#' @noRd
sebms_precipplot <- function(precip, colours = sebms_palette) {
  
  # x_tick <- c(0, unique(nb$month)+0.5)
  # len <- length(x_tick)
  # br <- c(sort(unique(nb$month)), x_tick)
  # lab <- c(sort(unique(nb$month.name)), rep(c(""), len))
  # 
  
  plotfunc <- function(df){
    ggplot(data = df, aes(x = reorder(month, monthnr), y = nb, fill = forcats::fct_rev(period))) + 
      geom_bar(stat = "identity", position = "dodge", width = .7) + 
      facet_wrap(~ name, ncol = 1) +
      #scale_x_continuous(breaks = br, labels = lab) +
      scale_y_continuous(expand = c(0,0), limits = c(0,250)) +
      scale_fill_manual(values = rev(colours)) + 
      labs(x = NULL, y = "Nederbörd (mm)") +
      theme_sebms()
    
  }
  p <- precip %>% 
    group_by(id) %>% 
    nest() %>%  
    mutate(plots = map(data, plotfunc))
  
  p$plots
}


#' Make a ggplot from Temperature Data
#' 
#' This takes a dataframe from sebms_temp_data and makes a temperature figure from that 
#' 
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' 
#' @noRd
sebms_tempplot <- function(temp, colours = sebms_palette){
  
  plotfunct <- function(df)
  {ggplot(data = df, aes(x = reorder(month, monthnr), 
                         y = temp, group = period, linetype = period, 
                         colour = period)) + 
      geom_line(stat = "identity", linewidth = 1) +
      facet_wrap(~ name, ncol = 1) +
      scale_linetype_manual(values = c("dashed", "solid")) +
      scale_color_manual(values = colours) + 
      scale_y_continuous(expand = c(0,0), limits = c(0,25)) +
      #coord_cartesian(expand = F, ylim = c(0,25), xlim = c(0,NA)) +
      labs(x = NULL, y = "Temperatur (°C)") + 
      theme_sebms() 
  }
  
  g <- temp %>% 
    group_by(id) %>% 
    nest() %>%  
    mutate(plots = map(data, plotfunct))
  
  g$plots  
  
}


#' Saves a ggplot object as a PNG file, resizing using pixel dimensions and a text scaling factor
#' 
#' @param plot a ggplot object
#' @param filename the path to the output file
#' @param width pixel width
#' @param height pixel height
#' @param text.factor text scaling factor (default is 3)
#' @param weathervar which weather variable it shoulld put in the name; 'Temp' or 'Precip'
#' @importFrom ggplot2 ggsave
#' @export
sebms_ggsave <- function(plot, filename, width = 12.67, height = 9.25, text.factor = 3, weathervar = "Temp") 
{
  dpi <- text.factor * 100
  width.calc <- width #/ dpi
  height.calc <- height # / dpi
  ggsave(filename = paste0(filename, weathervar, ".png"), plot = plot,
         device = "png", dpi = dpi, width = width.calc, height = height.calc, units = 'cm')
}

#' Creates png Figures  of Temperature and Precipitation for each Site
#' 
#' @param year from what year you want the temp and precipitation to be
#' @param my_place the places you want weather data pngs from (default to Umeå, Stockholm, Visby, Lund)
#' @param savepng logical, should the figures be saved as pngs. They are always shown in plot window
#' @param colours add your own colours to separate the year data from the normal values (1991-2020)
#'
#' @return png files with temperature and precipitation figures
#' @import ggplot2
#' @importFrom purrr map map2
#' @import dplyr
#' @export

sebms_weather_png <- function(year = lubridate::year(lubridate::today())-1, my_place = NA, savepng = TRUE, colours = sebms_palette) {
  
  if(length(colours) != 2) {
    cat("GIVE TWO COLOURS")
    cat("For example; colours = c('#BE4B48', '#9BBB59')")
    return()
  }
  
  plotst <- sebms_temp_data(my_place = my_place, year = year) %>% 
    sebms_tempplot(colours = colours)
  plotsp <- sebms_precip_data(my_place = my_place, year = year) %>% 
    sebms_precipplot(colours = colours)
  
  if(savepng) {
    
    if(unique(is.na(my_place))) # This is the default station names
      my_place <- c("Lund","Stockholm", "Umeå","Visby")
    
    try(map2(plotst, my_place, sebms_ggsave), silent = T)
    try(map2(plotsp, my_place, sebms_ggsave, weathervar = "Precip"), silent = T)
  }
  
  list(plotst, plotsp)
}

#' Download Function for SMHI Sunhours
#' 
#' Function that download sunhour data from SMHI
#' @importFrom httr GET content
#' @importFrom glue glue
#' @details
#' This is a helper function that download data from the SMHI API on sunhours (sun seconds) for a ceratin year `year` and month `month` and bind it to a dataframe.
#' 
#' @noRd
sunHdata <- function(year, month) {
  httr::GET(glue::glue("https://opendata-download-metanalys.smhi.se/api/category/strang1g/version/1/geotype/multipoint/validtime/{year}0{month}/parameter/119/data.json?interval=monthly")) %>% 
    httr::content(encoding = "UTF-8") %>% 
    bind_rows()
}

#' Create a total Sunhours Dataframe from SMHI Sunhour data
#' 
#' Produce a data frame of the total irradiance in Sweden for the given month.
#' 
#' @param year the year to produce plot for
#' @param month numeric value of the months to summarise sun ours over (default to 4:9)
#'
#' @import dplyr
#' @import sf
#' @importFrom purrr map map2 set_names
#' 
#' @returns a sf spatial point object with the WGS84 coordinate system
#' 
#' @export
sebms_sunhours_data <- function(year = year(today())-1, month = 4:9) {
  
  allyears <- function(year, month){
    map2(year, month, sunHdata) %>% ##iterate through year plus month and send that to sunHdata, se above
      set_names(month) %>% 
      bind_rows(.id = "month") %>% 
      group_by(lat, lon) %>% 
      summarise(total_sunH = sum(value),.groups = "drop") %>% 
      mutate(total_sunH = total_sunH / 60)
  }
  
  sunlist <- map(year, ~allyears(year = .x, month = month)) %>% 
    set_names(year) %>% 
    bind_rows(.id = "Year")
  
  spatlist <- sunlist %>% 
    filter(lon > 4) %>% 
    st_as_sf(coords = c("lon", "lat")) %>% 
    st_set_crs(4326) %>% 
    st_intersection(SE)
  
  assign("spatsunlist", spatlist, envir = .GlobalEnv)
  
  return(spatlist)
}

#' Colour Palette for Sunhours
#' 
#' @noRd
suncols <- colorRampPalette(colors = c(rgb(43,131,186,maxColorValue = 255), rgb(171,221,164, maxColorValue = 255), rgb(255,255,191, maxColorValue = 255), rgb(253,174,97, maxColorValue = 255) , rgb(215,25,28,maxColorValue = 255)))


#' Create a Mean Sunhour Value over a Five Year Period
#' 
#' Creates a five year mean from the SMHI Iradiance data. This data is also stored internally in the package to avoid to much downloading
#' 
#' @import dplyr
#' @importFrom purrr map set_names
#' @noRd
sebms_sunmean_data <- function(year = 2017:2021, month = 4:9, df) {
  
  meansunH <- map(year, ~sebms_sunhours_data(.x, month = month)) %>% 
    set_names(year) %>% 
    bind_rows() %>% 
    group_by(geometry) %>% 
    mutate(mean_sunH = mean(total_sunH, na.rm = T)) %>% 
    ungroup()

    return(meansunH)
}


#' Create an Image with Sunhour Data
#'
#' This function takes a data frame from e.g. `sebms_sunhours_data()` and creates an raster image.
#' 
#' @param year the year to create the figure for. 
#' @param df optional; a dataframe created by `sebms_sunhours_data()`
#' @param sunvar the variable to calculate colours on, `total_sunH` or `mean_sunH`
#' @param month the month or month ranges to sum over if new data is downloaded with `sebms_sunhours_data()`
#' 
#' @import sf
#' @import ggplot2
#' 
#' @return a figure saved as a png with the sunhours in coloour from, high (red) to low (blue)
#' @export
#'
sebms_sunhour_plot <- function(year = year(today())-1, df, sunvar = total_sunH, month = 4:9) {
  
  if(missing(df)) {
    cat("Please be pacient...")
    cat("THIS CAN TAKE A MINUTE OR FIVE\n\n")
    cat("Downloading sunhour data from SMHI........\n")
    df <- sebms_sunhours_data(year = year, month = month)
  }
  
  sunHplot <- df %>% 
    ggplot() +
    geom_sf(aes(colour = {{ sunvar }}), show.legend = F) +
    scale_colour_gradientn(colours = suncols(5),
                           limits = c(950, 2050),
                           oob = scales::squish
                           #values = seq(750, 2600, 200)
                           #low = "#2B83BA",
                           #high = "#D7191C",
                           #mid = "#FFFFBF"
                           ) +#c(0, 0.25, 0.5, 0.74, 1)) +
    theme_void() + theme(plot.background = element_rect(fill = "white", colour = "white"))
  
  sebms_ggsave(sunHplot, glue::glue("Sweden_{year}"), width = 9.25, height = 12.67, weathervar = "Sunhours")
  return(sunHplot) 
  
}

# all_plots <- allyearlist %>%
#   mutate(year = Year) %>% 
#   group_by(Year) %>%
#   nest() %>%
#   mutate(plots = map(data, ~sebms_sunhour_plot(df = .x, year = .x$year %>% unique())))

#' Make a diff Between current Year and the 5-year Mean 
#' 
#' This function makes a plot of the difference between the current years sun hours and the 5-year mean (2017-2021)
#' 
#' @noRd
sebms_sunhour_diff <- function(year, month) {
  
  allyearlist %>% 
    st_drop_geometry() %>% 
    ggplot() +
    geom_histogram(aes(total_sunH)) +
    facet_wrap(~Year) +
    theme_sebms()
  
  ggsave("SunHourHistogram.png", width = 14, height = 8)
  
  meansunH %>% 
    ggplot() +
    geom_histogram(aes(mean_sunH)) +
    theme_sebms()
  
  ggsave("MeanSunHourHistogram.png", width = 14, height = 8)
  
 allyearlist %>% 
   st_drop_geometry() %>% 
   group_by(Year) %>% 
   summarise(minsun = min(total_sunH),
             maxsun = max(total_sunH), .groups = "drop") %>% 
  gt::gt() %>% 
 gt::gtsave(filename = "MinMax_sunHours.png")
 
 if (missing(df)) {
  
 }
} 
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
