## Below are functions that creates the weather figures for the butterfly monitoring reports

#' Extract default Station names and id
#' 
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @import stringr
#' @noRd

sebms_default_station <- function(my_place, tempstat = TRUE) {
  
  if(tempstat){ # This change stations slightly depending on if it is precipitation or tempereature that is used.
    my_place <- c("Stock.*Obs.*len$|^Lund$|Visby.*Flyg|Umeå.*Flyg")
    stations <- fromJSON("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22.json")$station %>% 
      filter(str_detect(name, my_place)) %>% 
      select(name, id, latitude, longitude) %>% 
      mutate(id = str_squish(id))
    
  }else{ # Stations for perecipitation
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
    paste0(collapse = "|") # This makes the concatenated list of stations from user to a regex spressions with the names.
  
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
    map(possibly(~read.csv2(str_squish(paste0("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/23/station/",.x,"/period/corrected-archive/data.csv")), skip = 12)), .progress = "Downloading precipitation") %>% 
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
    map(\(x) read.csv2(str_squish(paste0("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22/station/",x,"/period/corrected-archive/data.csv")), skip = 12), .progress = "Downloading temperatures") %>% 
    map(\(x) rename_with(x, ~c("FrDate", "ToDate", "month", "temp", "Delete", "Delete2", "Delete3"))) %>% # Set column names 
    bind_rows(.id = "id") %>% # .id = "id" keep the id of the station in the dataframe
    as_tibble() %>% 
    select(!starts_with("Delete")) %>% # Remove the columns we do not need
    filter(lubridate::year(ymd_hms(FrDate)) == year,
           #filter(lubridate::year(FrDate) == if_else(lubridate::month(lubridate::today()) < 11,lubridate::year(lubridate::today())-1, lubridate::year(lubridate::today())), ## This filter out the previous year if it is before november, otherwise it take this year. The archives have data upp until three month back, and you want the summer month of a recording year. 
           month(ymd_hms(FrDate)) %in% 4:9) %>% 
    left_join(stations, by = "id") %>% # Join in the names
    transmute(name, id = as.numeric(id), latitud = latitude, longitud = longitude, month = month(ymd_hms(FrDate), label = T, abbr = T, locale = "sv_SE"), temp = as.numeric(temp), monthnr = month(ymd_hms(FrDate)), period = "2") # Change name and variables I want to keep/have
  
  filt_temp <- all_temp  %>%
    mutate(name = str_remove(name, " .*|-.*")) %>% # Only keep basic name, e.g. Umeå Flygplats become Umeå
    group_by(name) %>% 
    distinct(id) %>% 
    slice(1) %>% # This take the first station id from the location name if there are several
    ungroup() %>% 
    select(-name)
  
  temp <- filt_temp  %>%
    left_join(all_temp, by = "id") %>% # Join in the data from the chosen station
    bind_rows(norm_temp %>% filter(id %in% c(filt_temp %>% pull(id)))) %>% # add in the normal temperatures from the internal data for the chosen stations
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
    nest() %>%  # Nest by name
    mutate(plots = map(data, plotfunc, .progress = "Create precip figures")) # Iterate over the station names and make a plot for each
  
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
      geom_line(stat = "identity", linewidth = 1.1) +
      facet_wrap(~ name, ncol = 1) +
      scale_linetype_manual(values = c(32, "solid")) +
      scale_color_manual(values = colours) + 
      scale_y_continuous(expand = c(0,0), limits = c(0,25)) +
      #coord_cartesian(expand = T, ylim = c(0,25), xlim = c(0.7,6.3)) +
      labs(x = NULL, y = "Temperatur (°C)") + 
      theme_sebms() 
  }
  
  g <- temp %>% 
    group_by(id) %>% 
    nest() %>%  
    mutate(plots = map(data, plotfunct, .progress = "Creating temp figures"))
  
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
sebms_ggsave <- function(plot, filename, width = 12.67, height = 9.722, text.factor = 3, weathervar = "Temp") 
{
  dpi <- text.factor * 100
  width.calc <- width #/ dpi
  height.calc <- height # / dpi
  ggsave(filename = glue("{filename}_{weathervar}.png"), plot = plot,
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
    
    try(map2(plotst, my_place, sebms_ggsave, weathervar = glue("Temp_{year}"), .progress = "Saving temp figure"), silent = T) # This iterates over plot + name and save it to file
    try(map2(plotsp, my_place, sebms_ggsave, weathervar = glue("Precip_{year}"), .progress = "Saving precip figure"), silent = T)
  }
  
  list(plotst, plotsp)
}
