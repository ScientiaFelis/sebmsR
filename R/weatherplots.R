## Below are functions that creates the weather figures for the butterfly monitoring reports

#' Extract Default Station Names and ID
#' 
#' Helper function that extract default station names from SMHI weather stations.
#' These are Lund, Visby, Stockholm and Umeå. 
#' 
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @importFrom stringr str_detect str_squish
#' @noRd
sebms_default_station <- function(tempstat = TRUE) {
  
  if(tempstat){ # This change stations slightly depending on if it is precipitation or temperature that is used.
    my_place <- c("Stock.*Obs.*len$|^Lund$|Visby.*Flyg|Umeå.*Flyg")
    stations <- jsonlite::fromJSON("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22.json")$station %>% 
      filter(str_detect(name, my_place)) %>% 
      select(name, id, latitude, longitude) %>% 
      mutate(id = str_squish(id))
    
  }else{ # Stations for precipitation
    my_place <- c("Stock.*Observ.*len$|^Lund$|Visby$|Umeå-Röbäck")
    stations <- jsonlite::fromJSON("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22.json")$station %>% 
      filter(str_detect(name, my_place)) %>% 
      select(name, id, latitude, longitude) %>% 
      mutate(id = str_squish(id))
  }
  
  return(stations)
}


#' Downloads and Exctract Stations Given by User
#'
#' This takes the given list by users and match only one station to the possible
#' with similar names
#'
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @importFrom stringr str_detect str_squish
#' 
#' @noRd
sebms_user_station <- function(my_place) {
  
  my_place <- my_place %>% 
    paste0(collapse = "|") # This makes the concatenated list of stations from user to a regex spressions with the names.
  
  stations <- jsonlite::fromJSON("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22.json")$station %>% 
    filter(str_detect(name, my_place)) %>% 
    select(name, id, latitude, longitude) %>% 
    mutate(id = str_squish(id))
  
  return(stations)
}


#' Download and Filter out Precipitation Data from SMHI
#' 
#' @import dplyr
#' @importFrom purrr set_names map compact
#' @importFrom utils read.csv2
#' @import lubridate
#' @import stringr
#' @import ggplot2
#' @noRd
sebms_precip_data <- function(year = lubridate::year(lubridate::today())-1, my_place = NA) {
  
  if(year == lubridate::year(lubridate::today()) & lubridate::month(lubridate::today()) < 11){
    warning("THERE IS NO PRECIPITATION DATA FOR THE LAST THREE MONTH YET!\nYou have to wait until at least DECEMBER.\n\n")
    return()
  }
  
  if(year > lubridate::year(lubridate::today())){
    warning("YOU ARE WAY AHEAD OF YOURSELF!")
    message("Chose a year that is not in the future.")
    return()
  }
  
  if(unique(is.na(my_place))){
    stations <- sebms_default_station(tempstat = FALSE)
  }else{
    stations <- sebms_user_station(my_place)
  }
  
  all_precip <- stations %>% 
    pull(id) %>% 
    set_names() %>% # To keep the id-names of the list
    map(possibly(~read.csv2(str_squish(paste0("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/23/station/",.x,"/period/corrected-archive/data.csv")), skip = 12)), .progress = "Downloading precipitation") %>% 
    compact() %>% 
    map(possibly(~rename_with(.x, ~c("FrDate", "ToDate", "month", "nb", "Delete", "Delete2", "Delete3")))) %>% # Set column names 
    bind_rows(.id = "id") %>% # .id = "id" keep the id of the station in the dataframe
    as_tibble() %>% 
    select(!starts_with("Delete")) %>% # Remove the columns we do not need
    filter(lubridate::year(ymd_hms(FrDate)) == year,
           month(ymd_hms(FrDate)) %in% 4:9) %>% 
    left_join(stations, by = "id") %>% 
    transmute(name,
              id = as.numeric(id),
              latitud = latitude,
              longitud = longitude,
              month = month(ymd_hms(FrDate),label = T, abbr = T, locale = "sv_SE.UTF-8"),
              nb = as.numeric(nb),
              monthnr = month(ymd_hms(FrDate)),
              period = "2") %>% 
    mutate(month = str_to_lower(month))
  
  filt_precip <- all_precip  %>%
    mutate(name = str_remove(name, " .*|-.*")) %>% 
    group_by(name) %>% 
    distinct(id) %>% 
    slice(1) %>%
    ungroup() %>% 
    select(-name)
  
  
    if(year == lubridate::year(lubridate::today())) { # If this year chosen add in the latest months from other database
      
      all_precip_latest <- stations %>% 
        pull(id) %>% 
        set_names() %>% # To keep the id-names of the list
        map(possibly(~read.csv2(str_squish(paste0("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/23/station/",.x,"/period/latest-months/data.csv")), skip = 9, header = F, na.strings = c("NA", ""))), .progress = "Downloading precipitation") %>% 
        compact() %>% 
        map(possibly(~rename_with(.x, ~c("FrDate", "ToDate", "month", "nb", "Delete", "Delete2", "Delete3")))) %>% # Set column names 
        bind_rows(.id = "id") %>% # .id = "id" keep the id of the station in the dataframe
        as_tibble() %>%  
        select(!starts_with("Delete")) %>% # Remove the columns we do not need
        filter(lubridate::year(ymd_hms(FrDate)) == year,
               month(ymd_hms(FrDate)) %in% 4:9,
               !str_detect(nb, "[Nn]eder"),
               !is.na(nb)) %>% 
        suppressWarnings() %>% 
        left_join(stations, by = "id") %>% 
        transmute(name,
                  id = as.numeric(id),
                  latitud = latitude,
                  longitud = longitude,
                  month = month(ymd_hms(FrDate), label = T, abbr = T, locale = "sv_SE.UTF-8"),
                  nb = as.numeric(nb),
                  monthnr = month(ymd_hms(FrDate)),
                  period = "2") %>% 
        mutate(month = str_to_lower(month))
      
      all_precip <- all_precip %>% 
        bind_rows(all_precip_latest) %>% # Add latest month
        distinct() # Remove potential doublets
      
  }
  
  precip <- filt_precip %>%
    left_join(all_precip, by = "id") %>% 
    bind_rows(norm_precip %>% filter(id %in% c(filt_precip %>% pull(id)))) %>% 
    mutate(name = str_remove(name, " .*|-.*")) %>%
    complete(id, monthnr, period, fill = list(nb = 0)) %>%
    fill(c(name,latitud, longitud, month), .direction = "down") 
  
  return(precip)
}

#' Download and Filter out Temperature Data from SMHI
#' 
#' @import dplyr 
#' @import stringr
#' @import lubridate
#' @importFrom purrr set_names map compact
#' @import ggplot2
#' @noRd
sebms_temp_data <- function(year = lubridate::year(lubridate::today())-1, my_place = NA) {
  
  if(year > lubridate::year(lubridate::today())){
    warning("YOU ARE WAY AHEAD OF YOURSELF!")
    message("Chose a year that is not in the future.")
    return()
  }
  
  if(unique(is.na(my_place))){
    stations <- sebms_default_station(tempstat = TRUE)
  }else{
    stations <- sebms_user_station(my_place)
  }
  
  all_temp <- stations %>% 
    pull(id) %>% 
    set_names() %>% # To keep the id-names of the list
    map(possibly(~read.csv2(str_squish(paste0("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22/station/",.x,"/period/corrected-archive/data.csv")), skip = 12)), .progress = "Downloading temperatures") %>% 
    compact() %>% 
    map(\(x) rename_with(x, ~c("FrDate", "ToDate", "month", "temp", "Delete", "Delete2", "Delete3"))) %>% # Set column names 
    bind_rows(.id = "id") %>% # .id = "id" keep the id of the station in the dataframe
    as_tibble() %>% 
    select(!starts_with("Delete")) %>% # Remove the columns we do not need
    filter(lubridate::year(ymd_hms(FrDate)) == year,
           month(ymd_hms(FrDate)) %in% 4:9) %>% 
    left_join(stations, by = "id") %>% # Join in the names
    transmute(name,
              id = as.numeric(id),
              latitud = latitude,
              longitud = longitude,
              month = month(ymd_hms(FrDate), label = T, abbr = T, locale = "sv_SE.UTF-8"),
              temp = as.numeric(temp),
              monthnr = month(ymd_hms(FrDate)),
              period = "2") %>%  # Change name and variables I want to keep/have
  mutate(month = str_to_lower(month))
  
  filt_temp <- all_temp  %>%
    mutate(name = str_remove(name, " .*|-.*")) %>% # Only keep basic name, e.g. Umeå Flygplats become Umeå
    group_by(name) %>% 
    distinct(id) %>% 
    slice(1) %>% # This take the first station id from the location name if there are several
    ungroup() %>% 
    select(-name)
  
    if(year == lubridate::year(lubridate::today())) { # If this year chosen add in the latest months from other database
      
     all_temp_latest <- stations %>% 
        pull(id) %>% 
        set_names() %>% # To keep the id-names of the list
        map(possibly(~read.csv2(str_squish(paste0("https://opendata-download-metobs.smhi.se/api/version/latest/parameter/22/station/",.x,"/period/latest-months/data.csv")), skip = 9, header = F, na.strings = c("NA", ""))), .progress = "Downloading temperatures") %>% 
       compact() %>% 
        map(possibly(~rename_with(.x, ~c("FrDate", "ToDate", "month", "temp", "Delete", "Delete2", "Delete3")))) %>% # Set column names 
       
        bind_rows(.id = "id") %>% # .id = "id" keep the id of the station in the dataframe
        as_tibble() %>% 
        select(!starts_with("Delete")) %>% # Remove the columns we do not need
        filter(lubridate::year(ymd_hms(FrDate)) == year,
               month(ymd_hms(FrDate)) %in% 4:9,
               !str_detect(temp, "[Ll]uft"),
               !is.na(temp)) %>% 
       suppressWarnings() %>% 
        left_join(stations, by = "id") %>% # Join in the names
        transmute(name,
                  id = as.numeric(id),
                  latitud = latitude,
                  longitud = longitude,
                  month = month(ymd_hms(FrDate), label = T, abbr = T, locale = "sv_SE.UTF-8"),
                  temp = as.numeric(temp),
                  monthnr = month(ymd_hms(FrDate)),
                  period = "2") %>%  # Change name and variables I want to keep/have
        mutate(month = str_to_lower(month))
     
     all_temp <- all_temp %>% 
       bind_rows(all_temp_latest) %>% # add the latest month
       distinct() # Remove potential doublets 
    }
  
  
  temp <- filt_temp  %>%
    left_join(all_temp, by = "id") %>% # Join in the data from the chosen station
    bind_rows(norm_temp %>% filter(id %in% c(filt_temp %>% pull(id)))) %>% # add in the normal temperatures from the internal data for the chosen stations
    mutate(name = str_remove(name, " .*|-.*")) %>%
    complete(id, monthnr, period, fill = list(temp = 0)) %>%
    fill(c(name,latitud, longitud, month), .direction = "down") 
  
  return(temp) 
}




#' Make a ggplot from Precipitation Data
#'
#' This takes a dataframe from sebms_precip_data and makes a precipitation
#' figure from that
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr nest
#' @importFrom forcats fct_reorder
#' @importFrom purrr map
#'
#' @noRd
sebms_precipplot <- function(precip, colours = sebms_palette) {
  
  # x_tick <- c(0, unique(nb$month)+0.5)
  # len <- length(x_tick)
  # br <- c(sort(unique(nb$month)), x_tick)
  # lab <- c(sort(unique(nb$month.name)), rep(c(""), len))
  # 
  
  plotfunc <- function(df){
    ggplot(data = df, aes(x = fct_reorder(month, monthnr), y = nb, fill = forcats::fct_rev(period))) + 
      geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = .7) + 
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
    ungroup() %>% 
    mutate(plots = map(data, plotfunc, .progress = "Create precip figures")) # Iterate over the station names and make a plot for each
  
  p$plots
}


#' Make a ggplot from Temperature Data
#'
#' This takes a dataframe from sebms_temp_data and makes a temperature figure
#' from that
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr nest
#' @importFrom purrr map
#'
#' @noRd
sebms_tempplot <- function(temp, colours = sebms_palette){
  
  plotfunct <- function(df)
  {ggplot(data = df, aes(x = fct_reorder(month, monthnr), 
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


#' Creates png Figures of Temperature and Precipitation for each Site
#'
#' Creates figures for Temperature and Precipitation for the given year with a
#' 30-year mean as comparison.
#'
#' @param year from what year you want the temp and precipitation to be
#' @param my_place the places you want weather data pngs from (default to Umeå,
#'   Stockholm, Visby, Lund)
#' @param savepng logical, should the figures be saved as pngs. They are always
#'   shown in plot window
#' @param colours add your own colours to separate the year data from the normal
#'   values (1991-2020)
#'
#' @import ggplot2
#' @importFrom purrr map2
#' @import dplyr
#' @importFrom glue glue
#' @importFrom lubridate year today
#'
#' @return png files with temperature and precipitation figures
#'
#' @export
sebms_weather_png <- function(year = lubridate::year(lubridate::today())-1, my_place = NA, savepng = TRUE, colours = sebms_palette) {
  
  if(length(colours) != 2) {
    cat("GIVE TWO COLOURS")
    cat("For example; colours = c('#BE4B48', '#9BBB59')")
    return()
  }
  
  plotst <- sebms_temp_data(year = year, my_place = my_place) %>% 
    sebms_tempplot(colours = colours)
  plotsp <- sebms_precip_data(year = year, my_place = my_place) %>% 
    sebms_precipplot(colours = colours)
  
  if(savepng) {
    
    if(unique(is.na(my_place))) # This is the default station names
      my_place <- c("Lund","Visby","Stockholm", "Umeå")
    
    try(map2(plotst, my_place, sebms_ggsave, weathervar = glue("Temp_{year}"), .progress = "Saving temp figure"), silent = T) # This iterates over plot + name and save it to file
    try(map2(plotsp, my_place, sebms_ggsave, weathervar = glue("Precip_{year}"), .progress = "Saving precip figure"), silent = T)
  }
  
  list(plotst, plotsp)
}
