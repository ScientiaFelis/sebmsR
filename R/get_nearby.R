
#' Get Nearby Places to Given Coordinates
#'
#' Takes a coordinate and search for a nearby city, village or municiplaity depending on what is available and the number of inhabitants.
#' @param df data frame with coordinates, or sf object 
#' @param radius the radius to search for nearby places
#' @param top how many of the top results to save
#' @param limited logical; if you want only the names of the resulting sites
#' @param population_limit the smallest amount of poeple that should be in the locations
#'
#' @import geonames
#' @import dplyr
#' @importFrom purrr map map2
#' @importFrom sf st_coordinates
#' @return a dataframe with names nearby you coordinates
#' @export
#'
get_nearby <- function(df, radius = 50, top = 1, limited = TRUE, population_limit = 0){
  
  options(geonamesUsername = "sebms") 
  
  find_near <- possibly(function(df, radius = radius, top = top, limited = limited, pupulation_limit = population_limit) {
    
    lat <- df$lat
    lon <- df$lon
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
    transmute(Year, lon, lat, name, Max_Min = total_sunH)
  
  return(locations)
}
