#' 
#' Function that download hourly sunhour data from SMHI
#'
#' @param year the year to produce plot for
#' @param months numeric value of the months to summarise sun ours over (default to 4:9)
#' @param day the day of interest
#' @param hour the hour of interest
#' @param per_day logical; if data should be downloaded per day
#' 
#' @importFrom httr GET content
#' @importFrom glue glue
#' @importFrom polite politely
#' @importFrom dplyr bind_rows
#' 
#' @details
#' This is a helper function that download data from the SMHI API on sunhours (sun seconds) for a ceratin year `year` and month `months` and bind it to a dataframe.
#' 
#' @noRd
sunHdata <- function(year, months, day, per_day = FALSE) {
  if (per_day) {
    polite_GET_nrt <- politely(GET, verbose = FALSE, robots = FALSE) # turn off robotstxt checking
    
    polite_GET_nrt(glue("https://opendata-download-metanalys.smhi.se/api/category/strang1g/version/1/geotype/multipoint/validtime/{year}0{months}{day}/parameter/119/data.json?interval=daily")) %>% 
      content(encoding = "UTF-8") %>% 
      bind_rows()
  }else {
    GET(glue("https://opendata-download-metanalys.smhi.se/api/category/strang1g/version/1/geotype/multipoint/validtime/{year}0{months}/parameter/119/data.json?interval=monthly")) %>% 
      content(encoding = "UTF-8") %>% 
      bind_rows()
  }
}

#' Replace Falting Sunhour Values with mean of Surrounding Values
#' 
#' This function look for gaps in sunhour values (set to -999) and replace them with the mean of the before and following value.
#'
#' @inheritParams sunHdata
#' @importFrom dplyr mutate if_else lag lead
#' 
#' @return a dataframe with no gaps in sunhour values.
#' @noRd
fix_sunhour_NAs <- function(year, months, day, per_day = FALSE) {
  
  fixna <- sunHdata(year=year, months=months, day=day, per_day = per_day) %>% 
    mutate(gapvalue = if_else(value < 0,
                              "Low value",
                              "Normal value"),
           value = if_else(value < 0,
                           mean(c(lag(value), lead(value)), na.rm = T),
                           value))
  return(fixna)
  
}
#' Create a total Sunhours Dataframe from SMHI Sunhour data
#' 
#' Produce a data frame of the total sunhours in Sweden for the given month.
#' 
#' @param year the year to produce plot for
#' @param months numeric value of the months to summarise sun ours over (default to 4:9)
#' @param day the day of interest
#' @param hour the hour of interest
#' @param per_month logical; summarise per month instead of per year
#' @param per_day logical; if data should be downloaded per day
#' @param to_env logical; also send the result to the global environment as an object called 'spatsunist_{year}. If the function is used within a plot function this is TRUE
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
sebms_sunhours_data <- function(year = year(today())-1, months = 4:9, per_month = FALSE, per_day = FALSE, to_env = FALSE) {
  
  if (per_month) { #This runs two different versions of the functions. Summarise per month or per year.
    
    if (per_day) {
      dayfunc <- function(year, months) {
        pmap_dfr(Day, possibly(~fix_sunhour_NAs(year = year, months = months, day = .x, per_day = TRUE))) %>%
          bind_rows() %>% 
          group_by(month, gapvalue, lat, lon) %>% 
          summarise(daysunH = sum(value), .groups = "drop")
      } # This function iterate over days (and hour combinations if wanted) in combination with the year and month.
      
      allyears <- function(year, months){ # This functions iterate over year and month (not in all combinations) and sum sunhours per location.
        map2(year, months, dayfunc) %>% ##iterate through year plus month and send that to sunHdata via dayfunc and fix_sunhour_NAs, se above
          set_names(months) %>% # set the names of month to list items
          bind_rows(.id = "month") %>% # Take the nmae of list items (month) and set them in a variable
          group_by(month, gapvalue, lat, lon) %>%
          summarise(total_sunH = sum(daysunH),
                    .groups = "drop") %>%
          mutate(total_sunH = total_sunH / 60) # Convert minutes to hours
        
      }
    }else {
      
      allyears <- function(year, months){ # This functions iterate over year and month (not in all combinations) and sum sunhours per location.
        map2(year, months, possibly(fix_sunhour_NAs)) %>% ##iterate through year plus month and send that to sunHdata, se above
          set_names(months) %>% # set the names of month to list items
          bind_rows(.id = "month") %>% # Take the name of list items (month) and set them in a variable
          group_by(month, gapvalue, lat, lon) %>%
          summarise(total_sunH = sum(value),.groups = "drop") %>%
          mutate(total_sunH = total_sunH / 60) # Convert minutes to hours
      }
    }  
  }else { # Below functions summarise per year instead.
    
    
    # 
    # DayHour <- list(day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),# All days in a month
    #   hour = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
    # )  # All hours of the day
    # Day <- list(day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")) # All hours of the day
    # # 
    #Days <- list(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "31")) # To test with specific days
    
    if (per_day) {
      dayfunc <- function(year, months) {
        pmap_dfr(Day, possibly(~fix_sunhour_NAs(year = year, months = months, day = .x, per_day = TRUE))) %>%
          bind_rows() %>% 
          group_by(gapvalue, lat, lon) %>% 
          summarise(daysunH = sum(value), .groups = "drop")
      } # This function iterate over days (and hour combinations if wanted) in combination with the year and month.
      
      allyears <- function(year, months){ # This functions iterate over year and month (not in all combinations) and sum sunhours per location.
        map2(year, months, dayfunc) %>% ##iterate through year plus month and send that to sunHdata via dayfunc and fix_sunhour_NAs, se above
          set_names(months) %>% # set the names of month to list items
          bind_rows(.id = "month") %>% # Take the nmae of list items (month) and set them in a variable
          group_by(gapvalue, lat, lon) %>%
          summarise(total_sunH = sum(daysunH),
                    .groups = "drop") %>%
          mutate(total_sunH = total_sunH / 60) # Convert minutes to hours
        
      }
    }else {
      
      allyears <- function(year, months){ # This functions iterate over year and month (not in all combinations) and sum sunhours per location.
        map2(year, months, possibly(fix_sunhour_NAs)) %>% ##iterate through year plus month and send that to sunHdata, se above
          set_names(months) %>% # set the names of month to list items
          bind_rows(.id = "month") %>% # Take the name of list items (month) and set them in a variable
          group_by(gapvalue, lat, lon) %>%
          summarise(total_sunH = sum(value),.groups = "drop") %>%
          mutate(total_sunH = total_sunH / 60) # Convert minutes to hours
      }
    }
  }
  sunlist <- map(year, ~allyears(year = .x, months = months), .progress = "Loading sun-hours") %>%  # This iterates over all years given and send each one to allyears() function
    set_names(year) %>% # set names to Year
    bind_rows(.id = "Year")
  
  if (per_month) {
    sunlist <- sunlist %>%  # intersects with Sweden sf object to cut out only Sweden from area.
      select(Year, month, gapvalue, total_sunH, lon, lat) %>% # Put year in a column
      filter(lon > 4) %>% # removes negative W longitudes to not mess up the sf and crs
      st_as_sf(coords = c("lon", "lat")) %>%
      st_set_crs(4326) %>%
      st_intersection(SE)
  }else {
    sunlist <- sunlist %>%  # intersects with Sweden sf object to cut out only Sweden from area.
      select(Year, gapvalue, total_sunH, lon, lat) %>% # Put year in a column
      filter(lon > 4) %>% # removes negative W longitudes to not mess up the sf and crs
      st_as_sf(coords = c("lon", "lat")) %>%
      st_set_crs(4326) %>%
      st_intersection(SE)
  }
  
  if (to_env) {
    
    if(length(year) > 1) {
      Year <- glue("{min(year)}-{max(year)}") 
    }else {
      Year <- glue("{year}")
    }
    
    assign(glue("spatsunlist_{Year}"), sunlist, envir = .GlobalEnv) # Send the result to Global environment if the function is used inside a plot function. This way you do not need to download the data again if you want a diff plt to. You can just feed the spatsunlist data to the sun_diff_plot function
  }
  return(sunlist) # Also return the data frame to consol
}

#' Colour Palette for Sunhours
#' 
#' @noRd
suncols <- colorRampPalette(colors = c(rgb(43,131,186,maxColorValue = 255), rgb(171,221,164, maxColorValue = 255), rgb(255,255,191, maxColorValue = 255), rgb(253,174,97, maxColorValue = 255) , rgb(215,25,28,maxColorValue = 255)))


#' Create a Mean Sunhour Value over a Five Year Period
#' 
#' Creates a five year mean from the SMHI Iradiance data. This data is also stored internally in the package to avoid to much downloading
#' 
#' @inheritParams sebms_sunhours_data
#' @import dplyr
#' @importFrom purrr map set_names
#' @noRd
sebms_sunmean_data <- function(year = 2017:2021, months = 4:9, per_month = FALSE) {
  
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


#' Create an Image with Sunhour Data
#'
#' This function takes a data frame from e.g. `sebms_sunhours_data()` and creates an raster image.
#' 
#' @inheritParams sebms_sunhours_data
#' @param df optional; a dataframe created by `sebms_sunhours_data()`
#' @param sunvar the variable to calculate colours on, `total_sunH` or `mean_sunH`
#' @param legends logical; if you want a legend to the figures (default: FALSE)
#' 
#' @importFrom lubridate year today
#' @import ggplot2
#' @importFrom tidyr nest
#' @importFrom purrr map map2
#' @importFrom glue glue
#' 
#' @return a figure saved as a png with the sunhours in coloour from, high (red) to low (blue)
#' @export
sebms_sunhour_plot <- function(year = year(today())-1, df, sunvar = total_sunH, months = 4:9, per_month = FALSE, legends = FALSE) {
  
  if(missing(df)) {
    cat("Please be pacient...")
    cat("THIS CAN TAKE A MINUTE OR FIVE\n\n")
    cat("Downloading sunhour data from SMHI........\n")
    df <- sebms_sunhours_data(year = year, months = months, per_month = per_month, to_env = TRUE)
  }
  
  if (per_month) {
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
    
  }else {
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
  if (per_month) {
    ggs <- df %>% 
      group_by(month) %>% 
      nest() %>% 
      mutate(plots = map2(data, months, ~sunHplot(df = .x, months = .y), .progress = "Create sunhour figures")) # The map2 use data for eachmonth and also give the sunHplot function the month which is used in the switch to give a specific limits for eachmonth.
    
    map2(ggs$plots, ggs$month, ~sebms_ggsave(.x, "Sweden", width = 6, height = 12.67, weathervar = glue("Sunhours_{year}-{.y}")))
    
    return(ggs$plots) 
    
  }else {
    if (length(months) < 6 && per_month == FALSE) {
      cat("THIS FIGURE IF OPTIMIZED FOR THE SUM OF SUNHOURS OVER 6 SUMMER MONTH\n")
      cat("IT MIGHT LOOK VERY BLUE (LOW NR HOURS) OR RED (HIGH NR HOURS) IF FEWER OR MORE MONTH IS USED\n\n")
      cat("USE 'per_month = TRUE' TO GET VALUES PER MONTH")
    }
    ggs <- df %>% 
      group_by(Year) %>% 
      nest() %>% 
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

#' Make a diff Between current Year and the 5-year Mean 
#' 
#' This function makes a plot of the difference between the current years sun hours and the 5-year mean (2017-2021)
#' 
#' @inheritParams sebms_sunhours_data
#'  
#' @import dplyr
#' @importFrom sf st_drop_geometry st_as_sf
#' @importFrom lubridate year today
#'
#' @noRd
sebms_sunhour_diff <- function(df, year = year(today())-1, months = 4:9, per_month = FALSE) {
  
  if (missing(df)) {
    df <- sebms_sunhours_data(year = year, months = months, per_month = per_month, to_env = TRUE)
  }
  
  if (per_month) {
    sundiff <- df %>% 
      st_drop_geometry() %>%  
      select(Year, total_sunH) %>% # Take only Year and the sunhour var here as I am binding with the meansunH_M that contains what we need otherwise and is in the same order.  
      bind_cols(meansunH_M %>% filter(month %in% months)) %>% 
      #inner_join(meansunH_M, by = c("month", "geometry")) %>% 
      group_by(month) %>% 
      mutate(diffsun = total_sunH - mean_sunH) %>% 
      ungroup() %>% 
      st_as_sf()
    
  }else {
    sundiff <- meansunH %>%  
      bind_cols(df %>% st_drop_geometry()) %>% 
      mutate(diffsun = total_sunH - mean_sunH) %>% 
      st_as_sf()
  }
  
  return(sundiff)
} 

#' Create a Figure that Shows the Difference in Sunhours
#' 
#' Produce a plot that shows differences in sun hours between a given year and mean.
#'
#' @param df optinal; dataframe from the `sebms_sunhour_diff()`
#' @inheritParams sebms_sunhour_plot
#'
#' @importFrom lubridate year today
#' @import ggplot2
#' @importFrom glue glue
#' @importFrom tidyr nest
#' @importFrom purrr map map2
#' 
#' @return a figure that shows diffeence in sunhours
#' @export
sebms_sundiff_plot <- function(year = year(today())-1, df, months = 4:9, per_month = FALSE, legends = FALSE) {
  
  if(missing(df)) {
    cat("Please be pacient...")
    cat("THIS CAN TAKE A MINUTE OR FIVE\n\n")
    cat("Downloading sunhour data from SMHI........\n")
    dff <- sebms_sunhour_diff(year = year, months = months, per_month = per_month)
  }else{
    dff <- df %>% sebms_sunhour_diff(year = year, months = months, per_month = per_month)
  }
  
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
        geom_sf(aes(colour = diffsun), size = 0.01, show.legend = legends) +
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
        geom_sf(aes(colour = diffsun), size = 0.01, show.legend = legends) +
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
      mutate(plots = map2(data, months, ~sunDiffplot(dff = .x, months = .y), .progress = "Create sunhour diff figures"))
    
    
    map2(ggs$plots, ggs$month,  ~sebms_ggsave(.x, "Sweden", width = 6, height = 12.67, weathervar = glue("SunhourDiff_{year}-{.y}")))
    
    return(ggs$plots)
  }else {
    
    ggs <- dff %>% 
      group_by(Year) %>% 
      nest() %>% 
      mutate(plots = map(data, sunDiffplot, .progress = "Create sunhour diff figures"))
    
    
    map2(ggs$plots, ggs$Year,  ~sebms_ggsave(.x, "Sweden", width = 6, height = 12.67, weathervar = glue("SunhourDiff_{.y}")))
    
    return(ggs$plots)
  }
}

#' Maximum and Minimum Sun-hours Given Years
#'
#' GIve the maximm and minimum sunhour per year and the city or village closest to that location
#'
#' @param df a sf object with `year` and `total_sunhour` created by `sebms_sunhour_data()`
#' @param sunvar which variable to calculate the min and max on, can 
#' be `total_sunH` or `diffsun`
#' @inheritParams sebms_sunhours_data
#' 
#' @importFrom sf st_drop_geometry st_coordinates
#' @import dplyr
#'
#' @return a data frame with the max and min of total sunhours per year and the mean and diff from mean at that lokation. It also gives the name of the nearest city or village for that location.
#' @export
sebms_minmax_sunhour <- function(df, years = 2017:2022, months = 4:9, sunvar = total_sunH) {
  
  if(missing(df)) {
    
    df <- sebms_sunhours_data(year = years, months = months)
  }
  
  df %>%
    st_drop_geometry()  %>%
    bind_cols(df %>% st_coordinates() %>% as_tibble() %>% rename(lat = Y, lon = X)) %>%
    filter(Year %in% years) %>%
    group_by(Year) %>%
    mutate(max = max({{ sunvar }}), min = min({{ sunvar }})) %>%
    ungroup() %>%
    filter({{ sunvar }} == max| {{ sunvar }} == min) %>%
    get_nearby(sunvar = {{ sunvar }})
}

# 
# allyearlist_total_mean_diff %>%
#   st_drop_geometry() %>%
#   group_by(Year) %>%
#   summarise(minsun = min(total_sunH),
#             maxsun = max(total_sunH), .groups = "drop") %>%
#  gt::gt() %>%
#  gt::gtsave(filename = "MinMax_sunHours.png")
# #
# allyearlist_total_mean_diff %>%
#   st_drop_geometry() %>%
#   ggplot() +
#   geom_histogram(aes(diffsun)) +
#   facet_wrap(~Year) +
#   theme_sebms()
# 
# ggsave("DiffsunH_histogram.png", width = 14, height = 8)
# 
# allyearlist_total_mean_diff %>%
#   st_drop_geometry() %>%
#   group_by(Year) %>%
#   summarise(minsun = min(diffsun),
#             maxsun = max(diffsun),
#             meansun = mean(diffsun),
#             .groups = "drop") %>%
#   gt::gt() %>%
#   gt::gtsave(filename = "MinMax_DiffsunHours.png")
