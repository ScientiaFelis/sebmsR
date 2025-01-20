#' Swedish Map of SeBMS Sites
#'
#' Producing map for all visited sites on Swedish grid separating transects and
#' points.
#'
#' @import ggplot2
#' @import sf
#' @importFrom terra ext ext<- rast rasterize crs crs<- coltab project values
#' @importFrom ggnewscale new_scale_fill
#' @import dplyr
#' @importFrom lubridate year today
#' @importFrom tidyr nest
#' @importFrom purrr map map2
#' @importFrom glue glue
#'
#' @inheritParams sebms_abundance_per_species_plot
#' @param occ_sp SpatialPoints with occurrence data
#' @param width the plot width, default 12 inches
#' @param height the plot height, default 18 inches
#' @param maptype what survey type to produce map on, can be 'Transect', 'Point'
#'   or 'both', #'   default to 'both'. The 'Transect' and 'Point' can be
#'   abbreviated to 'T' and #'   'P'.
#' @param print logical; should the plots be printed in window, default FALSE
#'
#' @return Figures in png for points, and transects the given year
#' @export
sebms_sites_map <- function(year = lubridate::year(lubridate::today())-1, occ_sp, Län = ".", Landskap = ".", Kommun = ".", width = 12, height = 18, maptype = "both", print = FALSE, source = c(54,55,56,63,64,66,67,84)) {
  
  if (missing(occ_sp)) { #Load in data for all species from given year
    occ_sp <- sebms_occurances_distribution(year = year, Län = Län, Landskap = Landskap, Kommun = Kommun, source = source) %>%
      transmute(sitetype, speuid, lokalnamn, lat, lon) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = "espg:3006") %>% 
      st_set_crs(3006) %>% 
      st_transform(3021)
  }
  
  
  SweLandGrid <- st_read(system.file("extdata", "SweLandGrid.shp", package = "sebmsR"), quiet = TRUE) %>% 
    st_set_crs(3021)
  
  ## Sweden map
  tiff1 <- terra::rast(system.file("extdata", "MapSweden_RGB.png", 
                                   package = "sebmsR", mustWork = TRUE)) %>% 
    terra::flip() %>% 
    suppressWarnings()
  
  terra::ext(tiff1) <- c(1179998, 1948697, 6129692, 7679610)
  crs(tiff1) <- "epsg:3021"  
  
  tiff <- tiff1 %>%   
    terra::as.data.frame(xy=T) %>% 
    rename_with(.fn = ~c("x", "y","Red", "Green", "Blue", "Max")) %>% 
    filter(Red != 0)
  
  # Creating the plotting function
  speplot <- function(spda, spid) {
    
    # Create a grid for all the visited survey grids the given year and site type
    bf <- apply(st_intersects(SweLandGrid, occ_sp %>% filter(sitetype %in% spid) %>% distinct(lokalnamn, .keep_all = T), sparse = FALSE), 2, function(col) { SweLandGrid[which(col), ]}) %>% 
      bind_rows() %>% 
      st_as_sf() %>% 
      st_set_crs(3021)
    
    # Make the plot
    ggplot() +
      geom_raster(data = tiff, aes(x = x, y = y,fill = rgb(r = Red, g = Green, b = Blue, maxColorValue = 255)), show.legend = FALSE) + # The Swedish map
      scale_fill_identity() + # This keep the correct original colours of map
      geom_sf(data = bf, alpha = 0, linewidth = 0.3, colour = rgb(128,128,128, maxColorValue = 255), inherit.aes = F) + # Visited survey grids the given year
      geom_sf(data = spda, colour = rgb(255,0,0,maxColorValue = 255), size = 0.1, inherit.aes = F) + # Species occurrences
      coord_sf(expand = F) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            plot.margin = margin(t = 1,r = 0,b = 1,l = 0, unit = "mm"),
            legend.position = "inside",
            legend.position.inside = c(0.1,0.91),
            legend.spacing.y = unit(2, units = "mm"),
            legend.key.size = unit(3, units = "mm")) +
      guides(fill = guide_legend(byrow = TRUE))
    
  }
  
  ggs <- occ_sp %>%
    distinct(sitetype, lokalnamn, .keep_all = T) %>% 
    group_by(sitetype) %>% 
    nest() %>% # Nest per species to save one png per species
    ungroup() %>% 
    mutate(plots = map2(data, sitetype, speplot, .progress = "Making plots:"))
  
  if (str_detect(maptype, "[Pp]oi?nt|[Pp]$")) {
    ggs <- ggs %>% 
      filter(sitetype == "P")
  }
  
  if (str_detect(maptype, "[Tt]ransect|[Tt]$")) {
    ggs <- ggs %>% 
      filter(sitetype == "T")
  }
  
  
  map2(ggs$plots, ggs$sitetype, ~sebms_ggsave(.x, .y, width = width, height = height, weathervar = glue("{year}")), .progress = "Saving plots:")
  
  if (print) {
    return(ggs$plots)
  }
  
}


#' Swedish Map of SeBMS Distribution Data
#'
#' Producing distribution map for species on the Swedish grid.
#'
#' @import ggplot2
#' @import sf
#' @importFrom terra ext ext<- rast rasterize crs crs<- coltab project values
#' @importFrom ggnewscale new_scale_fill
#' @import dplyr
#' @importFrom lubridate year today
#' @importFrom tidyr nest
#' @importFrom purrr map map2
#' @importFrom glue glue
#'
#' @inheritParams sebms_sites_map
#' @param Art the species of interest as a species id
#' 
#' @return ggplot object of map with grid coloured by local density and with
#'   species occurrence points.

#' @export
sebms_distribution_map <- function(year = lubridate::year(lubridate::today())-1, occ_sp, Art = 1:200, Län = ".", Landskap = ".", Kommun = ".", width=9, height=18, print = FALSE, source = c(54,55,56,63,64,66,67,84)) {
  
  if (missing(occ_sp)) { # Load in data for all species from given year,
    # without species restriction to get all sites visited
    occ_sp <- sebms_occurances_distribution(year = year, Län = Län, Landskap = Landskap, Kommun = Kommun, source = source) %>%
      transmute(speuid, art, lokalnamn, lat, lon, maxobs = as.numeric(max)) %>% 
      mutate(art = str_replace_all(art, "/", "-")) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = "espg:3006") %>% 
      st_set_crs(3006) %>% 
      st_transform(3021)
  }
  
  SweLandGrid <- st_read(system.file("extdata", "SweLandGrid.shp", package = "sebmsR"), quiet = TRUE) %>% 
    st_set_crs(3021)
  
  # Make a raster of all grid cells covering Sweden
  grid <- sebms_swe_grid %>% 
    st_as_sf() %>%
    st_set_crs(3021) %>% 
    suppressWarnings() %>% 
    st_transform(3021)
  
  rs <- rast(ext(grid), nrows = 62, ncols = 28, 
             crs = crs(grid))
  
  
  ## Sweden map
  tiff1 <- terra::rast(system.file("extdata", "MapSweden_RGB.png", 
                                   package = "sebmsR", mustWork = TRUE)) %>% 
    terra::flip() %>% 
    suppressWarnings()
  
  terra::ext(tiff1) <- c(1179998, 1948697, 6129692, 7679610)
  crs(tiff1) <- "epsg:3021"  
  
  tiff <- tiff1 %>%   
    terra::as.data.frame(xy=T) %>% 
    rename_with(.fn = ~c("x", "y","Red", "Green", "Blue", "Max")) %>% 
    filter(Red != 0)
  
  
  # Create a grid for all the visited survey sites the given year and region
  bf <- apply(st_intersects(SweLandGrid, occ_sp %>% distinct(lokalnamn, .keep_all = T), sparse = FALSE), 2, function(col) { SweLandGrid[which(col), ]}) %>% 
    bind_rows() %>% 
    st_as_sf() %>% 
    st_set_crs(3021)
  
  
  
  
  # Creating a colour scale for the occurrences fill
  
  # pal_orig <- c("0" = NA_integer_, "1" = rgb(234,173,68, alpha = 96, maxColorValue = 255), "2" = rgb(203,141,53, alpha = 96, maxColorValue = 255), "3" = rgb(171,109,37, alpha = 96, maxColorValue = 255), "4" = rgb(148,77,21, alpha = 96, maxColorValue = 255), "5" = rgb(92,69,4, alpha = 96, maxColorValue = 255))
  #pal_orig <- c("#EAAD44","#CB8D35","#AB6D25","#944D15","#5C4504")
  
  
  # Creating the plotting function
  speplot <- function(spda, spid) {
    
    # Create data frame to construct the fill colour for the given species
    rl <- occ_sp %>%
      filter(speuid %in% c(spid)) %>%
      arrange(maxobs) %>% 
      rasterize(rs, field = "maxobs")
    
    rl[rl>4] <- 5 # Every max obs value over 5 should be 5
    
    df <- as.data.frame(rl, xy = T)
    colnames(df) <- c("x", "y", "value")
    
    df <- df %>%
      bind_rows(data.frame(x = NA, y = NA, value = 0:5)) %>% # Fill in all possible values to make legend always show all values 0-5
      # Make a colour variable to make scale_fill work
      mutate(colour = case_when(value == 0 ~ "NA",
                                value == 1 ~ rgb(234,173,68, alpha = 96, maxColorValue = 255),
                                value == 2 ~ rgb(203,141,53, alpha = 96, maxColorValue = 255),
                                value == 3 ~ rgb(171,109,37, alpha = 96, maxColorValue = 255),
                                value == 4 ~ rgb(148,77,21, alpha = 96, maxColorValue = 255),
                                value == 5 ~ rgb(92,69,4, alpha = 96, maxColorValue = 255),
                                TRUE ~ "white"),
             colour = fct_rev(colour)) # reverse to make legend in right order
    
    # Make the plot
    ggplot() +
      geom_raster(data = tiff, aes(x = x, y = y,fill = rgb(r = Red, g = Green, b = Blue, maxColorValue = 255)), show.legend = FALSE) + # The Swedish map
      scale_fill_identity() + # This keep the correct original colours of map
      new_scale_fill() + # Start new scale
      geom_tile(data = df, aes(x, y, fill = colour, height = 28000, width = 28000), colour = rgb(128,128,128, maxColorValue = 255), inherit.aes = FALSE, alpha = 0.3, size = 0.2) + # Tiles/raster with occurrence data with values of the max observation of individuals per day 0-5+
      geom_sf(data = bf, alpha = 0, linewidth = 0.3, colour = rgb(128,128,128, maxColorValue = 255), inherit.aes = F) + # Visited survey grids the given year
      geom_sf(data = spda, colour = rgb(255,0,0,maxColorValue = 255), size = 0.1, inherit.aes = F) + # Species occurrences
      scale_fill_identity(name = NULL,
                          guide = "legend",
                          labels = c("0", "1", "2", "3", "4", "5+")
      ) +
      coord_sf(expand = F) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            plot.margin = margin(t = 1,r = 0,b = 1,l = 0, unit = "mm"),
            legend.position = c(0.1,0.91),
            legend.spacing.y = unit(1, units = "mm"),
            legend.key.size = unit(3, units = "mm")) +
      guides(fill = guide_legend(byrow = TRUE))
    
  }
  
  # Create the plots
  ggs <- occ_sp %>% 
    filter(maxobs > 0, speuid %in% Art) %>% # Now filter out the wanted species
    group_by(speuid, art) %>% 
    nest() %>% # Nest per species to save one png per species
    ungroup() %>% 
    mutate(plots = map2(data, speuid, speplot, .progress = "Making plots:"))
  
  map2(ggs$plots, ggs$art, ~sebms_ggsave(.x, .y, width = width, height = height, weathervar = glue("{year}")), .progress = "Saving plots:")
  
  if (print) {
    return(ggs$plots)
  }
  
}



#' Create Local Maps with Transect
#' 
#' Creates a map of the County or Municipality with the transect marked.
#'
#' @inheritParams sebms_sites_map
#' @import leaflet
#' @importFrom mapview mapshot2
#' @import webshot2
#'
#' @returns a png file with a map of the chosen area with slingor or transects marked.
#' @export

sebms_local_transekt_map <- function(year = lubridate::year(lubridate::today())-1, occ_sp, Län = ".", Landskap = ".", Kommun = ".", width = 12, height = 18, maptype = "both", print = FALSE, source = c(54,55,56,63,64,66,67,84)) {
  
  if (missing(occ_sp)) { # Load in data for all species from given year,
    # without species restriction to get all sites visited
    occ_sp <- sebms_occurances_distribution(year = year, Län = Län, Landskap = Landskap, Kommun = Kommun, source = source) %>%
      select(sitetype, lokalnamn, lat, lon) %>% 
      mutate(colour = if_else(sitetype == "T", "blue", "red")) 
    
    occ_sp <- occ_sp %>% 
      st_as_sf(coords = c("lon", "lat"), crs = "espg:3006") %>% 
      st_set_crs(3006) %>% 
      st_transform(4326) %>% st_coordinates() %>% bind_cols(occ_sp) %>% transmute(lokalnamn, sitetype, Kommun, lon = X, lat = Y, colour)
    
  }
  
  wms_topo_nedtonad <- "https://hades.slu.se/lm/topowebb/wms/v1"
  
  locplot <- function(data, sitetype) {
    leaflet(data) %>%
      setView(lng = 13.29, lat = 55.7, zoom = 11) %>%
      addWMSTiles(
        wms_topo_nedtonad,
        layers = "topowebbkartan_nedtonad",
        options = WMSTileOptions(format = "image/png", transparent = TRUE)
      ) %>%
      addCircleMarkers(lng = data$lon, lat = data$lat,
                       radius = 5,
                       color = NA,
                       fill = TRUE,
                       fillColor = data$colour,
                       fillOpacity = 1,
                       opacity = 1,
                       label = data$lokalnamn) 
    
  }
  
  ggs <- occ_sp %>%
    distinct(sitetype, lokalnamn, .keep_all = T) %>% 
    group_by(sitetype) %>% 
    nest() %>% # Nest per species to save one png per species
    ungroup() %>% 
    mutate(plots = map2(data, sitetype, locplot, .progress = "Making plots:"))
  
  if (str_detect(maptype, "[Pp]oi?nt|[Pp]$")) {
    ggs <- ggs %>% 
      filter(sitetype == "P")
  }
  
  if (str_detect(maptype, "[Tt]ransect|[Tt]$")) {
    ggs <- ggs %>% 
      filter(sitetype == "T")
  }
  
  walk2(ggs$plots, ggs$sitetype, ~mapshot2(.x, file = glue("{Kommun}_sitetype-{.y}.png")), .progress = "Saving plots:")
  
  if (print) {
    return(ggs$plots)
  }
  
  
}