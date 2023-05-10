
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
get_nearby <- function(df, radius=50, top=1, limited=TRUE, population_limit=0){
  
  options(geonamesUsername="sebms") 
  
  find_near <- function(lat = lat, lon = lon, radius = radius, top = top, limited = limited, pupulation_limit = population_limit) {
    
    GNfindNearbyPlaceName(lat = lat , lng = lon, radius = radius, maxRows = "100", style = "MEDIUM") %>%
      transmute(name = toponymName, distance = as.numeric(distance), population = as.numeric(population)) %>%
      filter(population > population_limit) %>%
      slice_min(distance, n = top) %>%  
      { if(limited) select(., name)  else select_all(.) }
  }
  
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
    mutate(loc = map(data, find_near)) %>% 
    unnest(loc)
  
  return(locations)
}

# tomte <- data.frame (sit_uid=c(11,12),lat=c(56,60),lon=c(16,16))
# sites <- read_csv("sites_0915.csv")
# names(sites) <- c("sit_uid","sit_nearby","lat","lon")
# 
# s2 <- filter(sites, sit_uid<=100)
# 
# s3 <- get_nearby(s2, limited = F, top = 10,population_limit = 1000)
# 
# s4<-merge(s3, s2,  by="sit_uid") 
# 



