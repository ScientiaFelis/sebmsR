
#' Find Nearest Locales to XY-Coordinates
#'
#' Find the nearest locales to the xy-coordinates. This is a help function to the
#' get_nearby- functions.
#'
#' @param df data frame with coordinates
#' @param radius the radius to search for nearby places
#' @param top how many of the top results to save
#' @param limited logical; if you want only the names of the resulting sites
#' @param population_limit the smallest amount of poeple that should be in the locations
#'
#' @importFrom geonames GNfindNearbyPlaceName
#' @import dplyr
#'
#' @returns a tibble with the names (and distance and population if 'limited = FALSE') of
#'   the nearest locales.
#' @noRd
find_near <- function(df,  radius = 50, top = 1, limited = TRUE, population_limit = 0) {

  lat <- df %>% select(matches("lat")) %>% pull()
  lon <- df %>% select(matches("lon")) %>% pull()
  geonames::GNfindNearbyPlaceName(lat = lat , lng = lon, radius = radius, maxRows = "100", style = "MEDIUM") %>%
    as_tibble() %>%
    transmute(name = toponymName, distance = as.numeric(distance), population = as.numeric(population)) %>%
    filter(population > population_limit) %>% # Filter on min population size
    slice_min(distance, n = top) %>% # take the nearest 'top' locals
    { if(limited) select(., name)  else select_all(.) } # select only name var if limited = TRUE, otherwise all.
}



#' Get Nearby Places to Given Coordinates
#'
#' Search for a city, village or municipality nearby a coordinate depending on
#' what is available and the number of inhabitants.
#'
#' @param df data frame with coordinates, or sf object from e.g. [sebms_sunhours_data()]
#' @inheritParams find_near
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

  if(inherits(df, "sf")) { # If df is a sf object from another function, create a tibble with x&y coordinates
    df <- df %>%
      st_coordinates() %>%
      as_tibble() %>%
      rename(lat = Y, lon = X)
  }

  locations <- df %>%
    mutate(ID = row_number()) %>% # Create an ID for each xy-coordinate
    group_by(ID) %>%
    nest() %>%
    ungroup() %>%
    mutate(loc = map(.x = data, .f = possibly(~find_near(.x, radius = radius, top = top, limited = limited, population_limit = population_limit)), .progress = "Finding nearest locations")) %>%
    unnest(loc) %>%
    unnest(data) %>%
    select(lon = matches("lon"), lat = matches("lat"), name)

  return(locations)
}


#' Get Nearby Places to Sunhour Stations
#'
#' Search for a city, village or municipality nearby the Sun Hour stations and
#' get the Sun hour data. Optional is to get the only stations where there where
#' most and least number of sun hours. This function also depends on what is
#' available and the number of inhabitants.
#'
#' @inheritParams get_nearby
#' @param sunvar variable with sun hour data, can be `total_sunH` or `sundiff`
#' @param per_month logical; summarise per month instead of per year
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
get_nearby_SunHour <- function(df, radius = 50, top = 1, limited = TRUE, population_limit = 0, sunvar = total_sunH, per_month = F){
  #TODO: Make it use only Swedish locals
  options(geonamesUsername = "sebms")


  if(inherits(df, "sf")) { # If 'df' is a sf object
    df1 <- df %>%
      st_coordinates() %>% # Take only XY coordinate
      as_tibble() %>%
      rename(lat = Y, lon = X)
    if (per_month) { # For per month data frame
      df <- df1 %>%
        bind_cols(df %>% # Bind in variables to coordinates
                    st_drop_geometry() %>%
                    select(Year, month, {{ sunvar }})
        )
    }else { # for per year data frame
      df <- df1 %>%
        bind_cols(df %>%
                    st_drop_geometry() %>%
                    select(Year, {{ sunvar }})
        )
    }
  }

  locations <- df %>%
    mutate(ID = row_number()) %>%
    group_by(ID) %>%
    nest() %>%
    ungroup() %>%
    mutate(loc = map(.x = data, .f = possibly(~find_near(.x, radius = radius, top = top, limited = limited, population_limit = population_limit)), .progress = "Finding nearest location")) %>% # Find nearest per row
    unnest(loc) %>%
    unnest(data) %>%
    select(-ID)


  return(locations)
}
