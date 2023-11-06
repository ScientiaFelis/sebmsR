
#' Get Nearby Places to Given Coordinates
#'
#' Search for a city, village or municipality nearby a coordinate depending on
#' what is available and the number of inhabitants.
#'
#' @param df data frame with coordinates, or sf object
#' @param radius the radius to search for nearby places
#' @param top how many of the top results to save
#' @param limited logical; if you want only the names of the resulting sites
#' @param population_limit the smallest amount of poeple that should be in the
#'   locations
#'
#' @importFrom geonames GNfindNearbyPlaceName
#' @import dplyr
#' @importFrom tidyr nest unnest
#' @importFrom purrr map map2 possibly
#' @importFrom sf st_coordinates st_coordinates
#'
#' @return a data frame with location names nearby your coordinates
#' @export
#' 
get_nearby <- function(df, radius = 50, top = 1, limited = TRUE, population_limit = 0){
  #TODO: Make it use only Swedish locals
  options(geonamesUsername = "sebms") 
  
  find_near <- possibly(function(df, radius = radius, top = top, limited = limited, pupulation_limit = population_limit) {
    
    lat <- df %>% select(matches("lat")) %>% pull()
    lon <- df %>% select(matches("lon")) %>% pull()
      GNfindNearbyPlaceName(lat = lat , lng = lon, radius = radius, maxRows = "100", style = "MEDIUM") %>%
        as_tibble() %>% 
        transmute(name = toponymName, distance = as.numeric(distance), population = as.numeric(population)) %>%
        filter(population > population_limit) %>%
        slice_min(distance, n = top) %>%  
        { if(limited) select(., name)  else select_all(.) }
  }
  )
#  find_near <- possibly(find_near, otherwise = "Empty df")

    if(class(df) %>% first() %in% "sf") {
    df <- df %>% 
      st_coordinates() %>% 
      as_tibble() %>%
      rename(lat = Y, lon = X)
    
  }

  locations <- df %>%
    mutate(ID = row_number()) %>%
    group_by(ID) %>%
    nest() %>%
    ungroup() %>%
    mutate(loc = map(data, ~find_near(.x, radius = radius, top = top, limited = limited, pupulation_limit = population_limit))) %>% 
    unnest(loc) %>%
    unnest(data) %>%
    select(lon = matches("lon"), lat = matches("lat"), name)
  
  return(locations)
}




#' Get Nearby Places to SunHour Station Coordinates
#'
#' Search for a city, village or municipality nearby a Sun Hour station
#' depending on what is available and the number of inhabitants.
#'
#' @inheritParams get_nearby
#' @param sunvar dataframe with sunvar data, e.g. from `sebms_sunhours_data()``
#'
#' @importFrom geonames GNfindNearbyPlaceName
#' @import dplyr
#' @importFrom tidyr nest unnest
#' @importFrom purrr map map2 possibly
#' @importFrom sf st_coordinates st_coordinates
#'
#' @return a data frame with location names nearby your coordinates
#' @export
#' 
get_nearby_SunHour <- function(df, radius = 50, top = 1, limited = TRUE, population_limit = 0, sunvar = total_sunH){
  #TODO: Make it use only Swedish locals
  options(geonamesUsername = "sebms") 
  
  find_near <- possibly(function(df, radius = radius, top = top, limited = limited, pupulation_limit = population_limit) {
    
    lat <- df %>% select(matches("lat")) %>% pull()
    lon <- df %>% select(matches("lon")) %>% pull()
      GNfindNearbyPlaceName(lat = lat , lng = lon, radius = radius, maxRows = "100", style = "MEDIUM") %>%
        as_tibble() %>% 
        transmute(name = toponymName, distance = as.numeric(distance), population = as.numeric(population)) %>%
        filter(population > population_limit) %>%
        slice_min(distance, n = top) %>%  
        { if(limited) select(., name)  else select_all(.) }
  }
  )
#  find_near <- possibly(find_near, otherwise = "Empty df")

    if(class(df) %>% first() %in% "sf") {
    df <- df %>% 
      st_coordinates() %>% 
      as_tibble() %>%
      rename(lat = Y, lon = X) %>% 
      bind_cols(df %>% 
                  st_drop_geometry() %>% 
                  select(Year, {{ sunvar }})
                )
    
  }

  locations <- df %>%
    mutate(ID = row_number()) %>%
    group_by(ID) %>%
    nest() %>%
    ungroup() %>%
    mutate(loc = map(data, ~find_near(.x, radius = radius, top = top, limited = limited, pupulation_limit = population_limit))) %>% 
    unnest(loc) %>%
    unnest(data) %>% 
    transmute(Year, lon, lat, name, Max_Min = {{ sunvar }}) %>% 
    arrange(Year, name, desc(Max_Min))
  
  return(locations)
}
