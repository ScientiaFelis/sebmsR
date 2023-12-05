#' Download Sun Hour Data from SMHI
#' 
#' This is a helper function that download sunhour data from SMHI open data. 
#'
#' @param year the year to produce plot for
#' @param months numeric value of the months to summarise sun ours over (default
#'   to 4:9)
#' @param day the day of interest
#' @param per_day logical; if data should be downloaded per day
#'
#' @importFrom httr GET content
#' @importFrom glue glue
#' @importFrom polite politely
#' @importFrom dplyr bind_rows if_else lag lead mutate
#'
#' @details This is a helper function that download data from the SMHI API on
#'   sunhours (sun seconds) for a ceratin year `year` and month `months` and
#'   bind it to a dataframe.
#'
#' @noRd
sunHdata <- function(year, months, day, per_day = FALSE) {
  if (per_day) {
    polite_GET_nrt <- polite::politely(GET, verbose = FALSE, robots = FALSE) # turn off robotstxt checking
    
    polite_GET_nrt(glue("https://opendata-download-metanalys.smhi.se/api/category/strang1g/version/1/geotype/multipoint/validtime/{year}0{months}{day}/parameter/119/data.json?interval=daily")) %>% 
      httr::content(encoding = "UTF-8") %>% 
      bind_rows() %>% 
      mutate(gapvalue = if_else(value < 0,
                                "Low value",
                                "Normal value"),
             value = if_else(value < 0,
                             mean(c(lag(value), lead(value)), na.rm = T),
                             value))
  }else {
    httr::GET(glue("https://opendata-download-metanalys.smhi.se/api/category/strang1g/version/1/geotype/multipoint/validtime/{year}0{months}/parameter/119/data.json?interval=monthly")) %>% 
      httr::content(encoding = "UTF-8") %>% 
      bind_rows() %>% 
      mutate(gapvalue = if_else(value < 0,
                                "Low value",
                                "Normal value"),
             value = if_else(value < 0,
                             mean(c(lag(value), lead(value)), na.rm = T),
                             value))
  }
}


#' Calculate Total Sun Hours from SMHI Open Data
#'
#' Produce a data frame of the total sun hours in Sweden for the given month.
#'
#' @param year the year or years to produce plot for
#' @param months numeric value of the months to summarise sun ours over (default
#'   to 4:9)
#' @param per_month logical; summarise per month instead of per year
#' @param per_day logical; if data should be downloaded per day
#' @param to_env logical; also send the result to the global environment as an
#'   object called 'spatsunist_{year}. If the function is used within a plot
#'   function this is TRUE
#'
#' @import dplyr
#' @importFrom sf st_as_sf st_set_crs st_intersection
#' @importFrom purrr map map2 set_names possibly pmap_dfr
#' @importFrom lubridate year today
#' @importFrom glue glue
#'
#' @returns a sf spatial point object with the WGS84 coordinate system
#'
#' @export
sebms_sunhours_data <- function(year = lubridate::year(lubridate::today())-1, months = 4:9, per_month = FALSE, per_day = FALSE, to_env = FALSE) {
  
  if (per_month) { #This runs two different versions of the functions. Summarise per month or per year.
    
    if (per_day) {
      dayfunc <- function(year, months) {
        pmap(Day, possibly(~sunHdata(year = year, months = months, day = .x, per_day = TRUE))) %>%
          list_rbind() %>% 
          group_by(gapvalue, lat, lon) %>% 
          summarise(daysunH = sum(value), .groups = "drop")
      } # This function iterate over days (and hour combinations if wanted) in combination with the year and month.
      
      allyears <- function(year, months){ # This functions iterate over year and month (not in all combinations) and sum sunhours per location.
        map2(year, months, dayfunc) %>% ##iterate through year plus month and send that to sunHdata via dayfunc and fix_sunhour_NAs, se above
          set_names(months) %>% # set the names of month to list items
          bind_rows(.id = "month") %>% # Take the name of list items (month) and set them in a variable
          group_by(month, gapvalue, lat, lon) %>% 
          summarise(total_sunH = sum(daysunH),
                    .groups = "drop") %>%
          mutate(total_sunH = total_sunH / 60) # Convert minutes to hours
        
      }
    }else {# This function summarise per month but do not use per day values.
      
      allyears <- function(year, months){ # This functions iterate over year and month (not in all combinations) and sum sunhours per location.
        map2(year, months, possibly(sunHdata)) %>% ##iterate through year plus month and send that to sunHdata, se above
          set_names(months) %>% # set the names of month to list items
          bind_rows(.id = "month") %>% # Take the name of list items (month) and set them in a variable
          group_by(month, gapvalue, lat, lon) %>%
          summarise(total_sunH = sum(value),.groups = "drop") %>%
          mutate(total_sunH = total_sunH / 60) # Convert minutes to hours
      }
    }  
  }else { # Below functions summarise per year instead.
    
    
    # DayHour <- list(day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),# All days in a month
    #   hour = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
    # )  # All hours of the day
    
    # Day <- list(day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")) # All days in a month
    
    if (per_day) { # Take out data per day
      dayfunc <- function(year, months) {
        pmap(Day, possibly(~sunHdata(year = year, months = months, day = .x, per_day = TRUE))) %>%
          list_rbind() %>% 
          group_by(gapvalue, lat, lon) %>% 
          summarise(daysunH = sum(value), .groups = "drop")
      } # This function iterate over days (and hour combinations if wanted) in combination with the year and month.
      
      allyears <- function(year, months){ # This functions iterate over year and month (not in all combinations) and sum sunhours per location.
        map2(year, months, dayfunc) %>% ##iterate through year plus month and send that to sunHdata via dayfunc and fix_sunhour_NAs, se above
          set_names(months) %>% # set the names of month to list items
          bind_rows(.id = "month") %>% # Take the nmae of list items (month) and set them in a variable
          group_by(month, gapvalue, lat, lon) %>%
          summarise(total_sunH = sum(daysunH),
                    .groups = "drop")
        
      }
    }else { # Use monthly data from SMHI to summarise on
      
      allyears <- function(year, months){ # This functions iterate over year and month (not in all combinations) and sum sunhours per location.
        map2(year, months, possibly(sunHdata)) %>% ##iterate through year plus month and send that to sunHdata, se above
          set_names(months) %>% # set the names of month to list items
          bind_rows(.id = "month") %>% # Take the name of list items (month) and set them in a variable
          group_by(month, gapvalue, lat, lon) %>%
          summarise(total_sunH = sum(value),.groups = "drop")
      }
    }
  }
  
  if (length(months) < 6 && per_month == FALSE) {
    message("IF YOU ARE DOING A FIGURE, IT IS OPTIMIZED FOR THE SUNHOURS OVER 6 SUMMER MONTH\n")
    message("USE 'per_month = TRUE' TO GET VALUES PER MONTH")
  }
  sunlist <- map(year, ~allyears(year = .x, months = months), .progress = "Loading sun-hours") %>%  # This iterates over all years given and send each one to allyears() function
    set_names(year) %>% # set names to Year
    bind_rows(.id = "Year")
  
  # Now we take the data frame and convert it to a sf object and intersect with a sf object over Sweden.
  # We then also bind the mean values to the resulting data frame and calculate a diff from the total sun hours
  if (per_month) { #per month sunlist data
    
    lmon <- sunlist %>% distinct(month) %>% pull()
    if(length(lmon) < length(months)) {
      message("DATA FROM ONE OR SEVERAL MONTHS MISSING!\n\n 'per_day=TRUE' MIGHT WORK.")
      months <- as.integer(lmon)
    }
    
    sunlist <- sunlist %>%  # intersects with Sweden sf object to cut out only Sweden from area.
      select(Year, month, gapvalue, total_sunH, lon, lat) %>% # Put year in a column
      filter(lon > 4) %>% # removes negative W longitudes to not mess up the sf and crs
      st_as_sf(coords = c("lon", "lat")) %>%
      st_set_crs(4326) %>%
      st_intersection(SE) %>% # Filter out points for Sweden
      bind_cols(meansunH_M %>% st_drop_geometry() %>% filter(month %in% months) %>% select(-month)) %>% # Bind in the mean sun hours per month
      group_by(Year, month) %>% 
      mutate(sundiff = total_sunH - mean_sunH) %>% # Calculate difference of sunhours to mean
      ungroup() 
    
  }else { # Per year sunlist data
    
    lmon <- sunlist %>% distinct(month) %>% pull()
    
    if(length(lmon) < length(months)) {
      message("DATA FROM ONE OR SEVERAL MONTHS MISSING!\n")
      message("IF YOU ARE MAKING A FIGURE, IT WILL BE INCORRECT, TO BLUE!\n")
      message("USE 'per_day=T', WHICH MAY SOLVE THE PROBLEM BUT TAKE LONG TIME.")
    }
    
    sunlist <- sunlist %>%  # intersects with Sweden sf object to cut out only Sweden from area.
      group_by(Year, gapvalue, lat, lon) %>% 
      summarise(total_sunH = sum(total_sunH)) %>% 
      ungroup() %>% 
      mutate(total_sunH = total_sunH / 60) %>% # Convert minutes to hours
      select(Year, gapvalue, total_sunH, lon, lat) %>% # Put year in a column
      filter(lon > 4) %>% # removes negative W longitudes to not mess up the sf and crs
      st_as_sf(coords = c("lon", "lat")) %>%
      st_set_crs(4326) %>%
      st_intersection(SE) %>% # Filter out points in Sweden
      bind_cols(meansunH %>% st_drop_geometry()) %>% # bind in the mean sun hours per month
      mutate(sundiff = total_sunH - mean_sunH) # Calculate difference of sunhours to mean
  }
  
  if (to_env) {
    
    if(length(year) > 1) {
      year <- glue("{min(year)}-{max(year)}") 
    }
    
    assign(glue("SunHours_{year}"), sunlist, envir = .GlobalEnv) # Send the result to Global environment if the function is used inside a plot function. This way you do not need to download the data again if you want a diff plt to. You can just feed the spatsunlist data to the sun_diff_plot function
  }
  return(sunlist) # Also return the data frame to consol
}

#' Colour Palette for Sunhours
#' 
#' @noRd
suncols <- colorRampPalette(colors = c(rgb(43,131,186,maxColorValue = 255), rgb(171,221,164, maxColorValue = 255), rgb(255,255,191, maxColorValue = 255), rgb(253,174,97, maxColorValue = 255) , rgb(215,25,28,maxColorValue = 255)))


#' Create a Mean Sun Hour Value over a Five Year Period
#'
#' Creates a five year mean from the SMHI Iradiance data. This data is also
#' stored internally in the package to avoid to much downloading
#'
#' @inheritParams sebms_sunhours_data
#' @import dplyr
#' @importFrom purrr map set_names
#' @noRd
sebms_sunmean_data <- function(year = 2018:2022, months = 4:9, per_month = FALSE) {
  
  if (per_month) {
    meansunH_M <- sebms_sunhours_data(year, months = months, per_month = per_month, per_day = FALSE) %>% 
      group_by(month, geometry) %>% 
      summarise(mean_sunH = mean(total_sunH, na.rm = T), .groups = "drop") %>% 
      select(month, mean_sunH, geometry)
    
    return(meansunH_M)
  }else {
    meansunH <- sebms_sunhours_data(year, months = months, per_month = per_month, per_day = FALSE) %>% 
      group_by(geometry) %>% 
      summarise(mean_sunH = mean(total_sunH, na.rm = T), .groups = "drop") %>% 
      select(mean_sunH, geometry)
    
    return(meansunH)
  }
  
}


#' Create an Image with Sun Hour Data
#'
#' Create a data frame from e.g. [sebms_sunhours_data()] and creates an raster
#' image.
#'
#' @inheritParams sebms_sunhours_data
#' @param df optional; a dataframe created by [sebms_sunhours_data()]
#' @param sunvar the variable to calculate colours on, `total_sunH` or
#'   `mean_sunH`
#' @param legends logical; if you want a legend to the figures (default: FALSE)
#'
#' @importFrom lubridate year today
#' @import ggplot2
#' @importFrom tidyr nest
#' @importFrom purrr map map2
#' @importFrom glue glue
#' @importFrom scales squish
#'
#' @return a figure saved as a png with the sunhours in coloour from, high (red)
#'   to low (blue)
#' @export
sebms_sunhour_plot <- function(year = lubridate::year(lubridate::today())-1, df, sunvar = total_sunH, months = 4:9, per_month = FALSE, per_day = FALSE, legends = FALSE) {
  
  if(missing(df)) {
    cat("Please be pacient...")
    cat("THIS CAN TAKE A MINUTE OR FIVE\n\n")
    cat("Downloading sunhour data from SMHI........\n")
    df <- sebms_sunhours_data(year = year, months = months, per_month = per_month, per_day = per_day, to_env = TRUE)
  }
  
  if (per_month) { # Figures per month
    #FIXME: check actual min and max for each month for years 2017:2022
    ## This makes limits specific for each month
    jan = c(60, 200) 
    feb = c(65, 230)
    mar = c(70, 300)
    apr = c(80, 350)
    maj = c(95, 440)
    jun = c(90, 440)
    jul = c(90, 460)
    aug = c(75, 400) #FIXME: Check august values 2018. Min is 0.99!!
    sep = c(75, 300) #FIXME: Very low min here too
    okt = c(65, 250)
    nov = c(60, 200)
    dec = c(60, 200)
    #FIXME: The legend is way to big
    sunHplot <- function(df, months) {  
      
      ggplot(data = df) +
        geom_sf(aes(colour = {{ sunvar }}), size = 0.01, show.legend = legends) +
        scale_colour_gradientn(colours = suncols(5), # Use the 5 colours of suncols, blue to red.
                               limits = switch(months, "1" = jan, "2"=feb, "3"=mar, "4"=apr, "5"=maj, "6"=jun, "7"=jul, "8"=aug, "9"=sep, "10"=okt, "11"=nov, "12"=dec), # Te switch take the month and return the corresponding limits from above. # These limits are set from a bit above and below the min and max values of sunhours
                               oob = scales::squish # This makes all values under min lim to blue, and all above max lim to red.
        ) +
        coord_sf(expand = F) +
        labs(colour = "Sun hours") +
        theme_void() + theme(plot.background = element_rect(fill = "white", colour = "white"),
                             legend.key.size = unit(4, "mm"),
                             legend.title = element_text(size = 9),
                             legend.text = element_text(size = 6),
                             legend.position = c(0.8, 0.45)
        )
    }
    
  }else { # Figures per year
    sunHplot <- function(df) {  
      ggplot(data = df) +
        geom_sf(aes(colour = {{ sunvar }}), size = 0.01, show.legend = legends) +
        scale_colour_gradientn(colours = suncols(5), # Use the 5 colours of suncols, blue to red.
                               limits = c(950, 2050), # These limits are set from a bit above and below the min and max values of sunhours
                               oob = scales::squish # This makes all values under min lim to blue, and all above max lim to red.
        ) +
        coord_sf(expand = F) +
        labs(colour = "Sun hours") +
        theme_void() + theme(plot.background = element_rect(fill = "white", colour = "white"),
                             legend.key.size = unit(4, "mm"),
                             legend.title = element_text(size = 9),
                             legend.text = element_text(size = 6),
                             legend.position = c(0.8, 0.45)
        )
    }
  }
  
  cat("\nMaking plots........\n")
  if (per_month) { # make a figure per month
    
    lmon <- df %>% st_drop_geometry() %>% distinct(month) %>% pull()
    
    if(length(lmon) < length(months)) {
      message("DATA FROM ONE OR SEVERAL MONTHS MISSING!\n\n'per_day = TUE' MIGHT BE A WORK AROUND BUT TAKE LONG TIME.")
      months <- as.integer(lmon)
    }
    
    ggs <- df %>% 
      group_by(month) %>% 
      nest() %>% 
      ungroup() %>% 
      mutate(plots = map2(data, months, ~sunHplot(df = .x, months = .y), .progress = "Create sunhour figures")) # The map2 use data for eachmonth and also give the sunHplot function the month which is used in the switch to give a specific limits for eachmonth.
    
    map2(ggs$plots, ggs$month, ~sebms_ggsave(.x, "Sweden", width = 6, height = 12.67, weathervar = glue("Sunhours_{year}-{.y}")))
    
    return(ggs$plots) 
    
  }else { # make a figure per year
    
    if (length(months) < 6 && per_month == FALSE) {
      message("THIS FIGURE IF OPTIMIZED FOR THE SUM OF SUNHOURS OVER 6 SUMMER MONTH\n")
      message("IT MIGHT LOOK VERY BLUE (LOW NR HOURS) OR RED (HIGH NR HOURS) IF FEWER OR MORE MONTH IS USED\n\n")
      message("USE 'per_month = TRUE' TO GET VALUES PER MONTH")
    }
    
    ggs <- df %>% 
      group_by(Year) %>% 
      nest() %>% 
      ungroup() %>% 
      mutate(plots = map(data, sunHplot, .progress = "Create sunhour figures"))
    
    map2(ggs$plots, ggs$Year, ~sebms_ggsave(.x, "Sweden", width = 6, height = 12.67, weathervar = glue("Sunhours_{.y}")))
    
    return(ggs$plots) 
  }
}

# all_plots <- allyearlist %>%
#   mutate(year = Year) %>% 
#   group_by(Year) %>%
#   nest() %>%
#   mutate(plots = map(data, ~sebms_sunhour_plot(df = .x, year = .x$year %>% unique())))


#' Create a Figure that Shows the Difference in Sun Hours
#'
#' Produce a plot that shows differences in sun hours between a given year and a
#' 5-year mean.
#'
#' @param df optinal; dataframe from the [sebms_sunhour_diff()]
#' @inheritParams sebms_sunhour_plot
#'
#' @importFrom lubridate year today
#' @import ggplot2
#' @importFrom glue glue
#' @importFrom tidyr nest
#' @importFrom purrr map map2
#' @importFrom scales squish
#'
#' @return a figure that shows diffeence in sunhours
#' @export
sebms_sundiff_plot <- function(year = lubridate::year(lubridate::today())-1, df, months = 4:9, per_month = FALSE, legends = FALSE, per_day = FALSE) {
  
  if(missing(df)) {
    message("Please be pacient...")
    message("THIS CAN TAKE A MINUTE OR FIVE\n\n")
    message("Downloading sunhour data from SMHI........\n")
    dff <- sebms_sunhours_data(year = year, months = months, per_month = per_month, per_day = per_day, to_env = TRUE)
  }#else{
  #  dff <- df %>% sebms_sunhours_data(year = year, months = months, per_month = per_month)
  #}
  
  if (per_month) {
    #FIXME: check actual min and max for each month for years 2017:2022
    ## This makes limits specific for each month
    jan = c(60, 200) 
    feb = c(65, 230)
    mar = c(70, 300)
    apr = c(-80, 50)
    maj = c(-150, 40)
    jun = c(-100, 80)
    jul = c(-120, 50)
    aug = c(-65, 100) #FIXME: Check august values 2018. Min is 0.99!!
    sep = c(-75, 80) #FIXME: Very low min here too
    okt = c(65, 250)
    nov = c(60, 200)
    dec = c(60, 200)
    
    #FIXME: The legend is way to big
    sunDiffplot <- function(dff, months) { 
      ggplot(data = dff) +
        geom_sf(aes(colour = sundiff), size = 0.01, show.legend = legends) +
        scale_colour_gradientn(colours = suncols(5),
                               limits = switch(months, "1" = jan, "2"=feb, "3"=mar, "4"=apr, "5"=maj, "6"=jun, "7"=jul, "8"=aug, "9"=sep, "10"=okt, "11"=nov, "12"=dec),
                               oob = scales::squish
        ) +
        coord_sf(expand = F) +
        labs(colour = "Sun hour diff") +
        theme_void() + theme(plot.background = element_rect(fill = "white", colour = "white"),
                             legend.key.size = unit(4, "mm"),
                             legend.title = element_text(size = 9),
                             legend.text = element_text(size = 6),
                             legend.position = c(0.8, 0.45)
        )
    }
    
    
  }else {
    
    sunDiffplot <- function(dff) { 
      ggplot(data = dff) +
        geom_sf(aes(colour = sundiff), size = 0.01, show.legend = legends) +
        scale_colour_gradientn(colours = suncols(5),
                               limits = c(-600, 600),
                               oob = scales::squish
        ) +
        coord_sf(expand = F) +
        labs(colour = "Sun hour diff") +
        theme_void() + theme(plot.background = element_rect(fill = "white", colour = "white"),
                             legend.key.size = unit(4, "mm"),
                             legend.title = element_text(size = 9),
                             legend.text = element_text(size = 6),
                             legend.position = c(0.8, 0.45)
        )
    }
    
  }
  
  cat("\n Making plots.....\n")
  
  if (per_month) {
    ggs <- dff %>% 
      group_by(month) %>% 
      nest() %>% 
      ungroup() %>% 
      mutate(plots = map2(data, months, ~sunDiffplot(dff = .x, months = .y), .progress = "Create sunhour diff figures"))
    
    
    map2(ggs$plots, ggs$month,  ~sebms_ggsave(.x, "Sweden", width = 6, height = 12.67, weathervar = glue("SunhourDiff_{year}-{.y}")))
    
    return(ggs$plots)
  }else {
    
    ggs <- dff %>% 
      group_by(Year) %>% 
      nest() %>% 
      ungroup() %>% 
      mutate(plots = map(data, sunDiffplot, .progress = "Create sunhour diff figures"))
    
    
    map2(ggs$plots, ggs$Year,  ~sebms_ggsave(.x, "Sweden", width = 6, height = 12.67, weathervar = glue("SunhourDiff_{.y}")))
    
    return(ggs$plots)
  }
}

#' Maximum and Minimum Sun Hours for Given Years
#'
#' Give the maximum and minimum sun hour per year and the city or village
#' closest to that location
#'
#' @param df a sf object with `year` and `total_sunhour` created by
#'   [sebms_sunhous_data()]
#' @param sunvar which variable to calculate the min and max on, can be
#'   `total_sunH` or `sundiff`
#' @inheritParams sebms_sunhours_data
#'
#' @importFrom sf st_drop_geometry st_coordinates
#' @import dplyr
#'
#' @return a data frame with the max and min of total sun hours per year and the
#'   mean and diff from mean at that location. It also gives the name of the
#'   nearest city or village for that location.
#' @export
sebms_minmax_sunhour <- function(df, year = 2017:2022, months = 4:9, sunvar = total_sunH, per_month = FALSE, per_day = FALSE) {
  
  if(missing(df)) {
    
    df <- sebms_sunhours_data(year = year, months = months, per_month = per_month, per_day = per_day) 
  }
  
    if (per_month) { # Set min max sun hour per month
      df <- df %>% 
        group_by(Year, month) %>% 
        mutate(max = max({{ sunvar }}),
               min = min({{ sunvar }})) %>% 
        ungroup() %>% 
        filter({{ sunvar }} == max | {{ sunvar }} == min)
    }else { # Set min-max sun hour per year
      df <- df %>% 
        group_by(Year) %>% 
        mutate(max = max({{ sunvar }}),
               min = min({{ sunvar }})) %>% 
        ungroup() %>% 
        filter({{ sunvar }} == max | {{ sunvar }} == min)
    }

  minmaxsun <- df %>%
    get_nearby_SunHour(sunvar = {{ sunvar }}, per_month = per_month) %>%
    #select(Year, lon, lat, name, MaxMin = {{ sunvar }}) %>% 
    rename(MaxMin = {{ sunvar }}) %>% 
    relocate(MaxMin, .after = last_col()) %>% 
    relocate(lon, lat, .before = name)
  
  if (per_month) {
    minmaxsun <- minmaxsun %>% 
      arrange(Year, month, desc(MaxMin))
  }else {
    minmaxsun <- minmaxsun %>% 
      arrange(Year, desc(MaxMin))
  }
  
  return(minmaxsun)
}
