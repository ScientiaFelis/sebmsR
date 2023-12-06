#' Swedish Map of SeBMS Sites
#' 
#' Producing map for all sites on Swedish grid.
#' 
#' @import grid
#' @import magick
#' @import ggthemes
#' @import ggplot2
#' @import sf
#' @importFrom terra ext ext<- rast rasterize crs crs<- project values
#' @importFrom dplyr mutate group_by ungroup
#' @importFrom tidyr nest
#' @importFrom purrr map map2
#' @importFrom glue glue
#' 
#' @param year the year of interest
#' @param width the plot width, default 12 inches
#' @param species the species of interest as a species id
#' @param height the plot height, default 18 inches
#' @param occ_sp SpatialPoints with occurrence data
#' @param print logical; should the plots be printed in window, default FALSE
#' @return ggplot object
#' @export
sebms_sites_map <- function(year=2021, species = 118, width = 12, height = 18, occ_sp, print = FALSE) {
  
  if (missing(occ_sp)) {
    occ_sp <- sebms_occurances_distribution(year = year, Art = species) %>%
      select(art, lokalnamn, lat, lon) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = "espg:3006") %>% 
      st_set_crs(3006) %>% 
      st_transform(3021)
  }
  
  
  SweLandGrid <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "SweLandGrid", quiet = TRUE) %>% 
    st_set_crs(3021)
  
  alla <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "alla_2010-2014_sites", quiet = TRUE) %>% 
    st_set_crs(3021)
  
  bf <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "ButterflySquares0423", quiet = TRUE) %>% 
    st_set_crs(3021)
  
  #a = rast("data-raw/figures/MapDistribution-data/R_files_for_similar_map/MapSweden.tif")
  
  grid <- sebms_swe_grid %>% 
    st_as_sf() %>% 
    st_set_crs(3021) %>% 
    st_transform(3021)
  
  bg <- sebms_swe_borders %>%
    st_as_sf() %>% 
    st_set_crs(3021) %>% 
    st_transform(3021)
  
  tiff1 <- terra::rast(system.file("extdata", "MapSweden_RGB.png", 
                                   package = "sebmsR", mustWork = TRUE))
  
  ext(tiff1) <- c(1179998, 1948697, 6129692, 7679610)
  crs(tiff1) <- "epsg:3021"  
  
  tiff <- tiff1 %>%   
    terra::as.data.frame(xy=T) %>% 
    rename_with(.fn = ~c("x", "y","Red", "Green", "Blue", "Max")) %>% 
    filter(Red != 0) %>% 
    select(-Max)
  
  #terra::rasterize(as.matrix(tiff[1:2]), y= tiff1)
  
  speplot <- function(spocc) {
    ggplot(data = tiff, aes(x = x, y = y)) +
      geom_raster(aes(fill = rgb(r = Red, g = Green, b = Blue, maxColorValue = 255)), show.legend = FALSE) +
      geom_sf(data = bf, inherit.aes = F, alpha = 0, linewidth = 0.3, colour = "black") +
      geom_sf(data = spocc, colour = "red", alpha = 0.2, inherit.aes = F) +
      scale_fill_identity() +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            #legend.position = "left",
            legend.position = c(0.2,0.8))
  }
  
  ggs <- occ_sp %>% 
    group_by(art) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(plots = map(data, speplot))
  
  map2(ggs$plots, ggs$art, ~sebms_ggsave(plot = .x, "Butterfly_sites", weathervar = .y, width = width, height = height))
  
  if (print) {
    return(ggs$plots)
  }
}


#' Swedish Map of SeBMS Distribution Data
#'
#' Producing distribution map for species on the Swedish grid.
#'
#' @import grid
#' @import magick
#' @import ggthemes
#' @import ggplot2
#' @import sf
#' @importFrom terra ext ext<- rast rasterize crs crs<- coltab project values
#' @importFrom ggnewscale new_scale_fill
#' @importFrom dplyr mutate group_by ungroup
#' @importFrom tidyr nest
#' @importFrom purrr map map2
#' @importFrom glue glue
#'
#' @inheritParams sebms_sites_map
#' 
#' @return ggplot object of map with grid coloured by local density and with
#'   species occurence points.

#' @export
sebms_distribution_map <- function(year=2022, species = 118, width=12, height=18, occ_sp, print = FALSE) {
  
  if (missing(occ_sp)) {
    occ_sp <- sebms_occurances_distribution(year = year) %>%
      transmute(speuid, art, lokalnamn, lat, lon, maxobs = as.numeric(max)) %>% 
      mutate(art = str_replace_all(art, "/", "-")) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = "espg:3006") %>% 
      st_set_crs(3006) %>% 
      st_transform(3021)
  }
  
  SweLandGrid <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "SweLandGrid", quiet = TRUE) %>% 
    st_set_crs(3021)
  
  
  # bf <- occ_sp %>% st_join(SweLandGrid, .) %>%  # Filter out grids which have been visited
  #   distinct(lokalnamn, .keep_all = T)
  
  # Create a grid for all the visited survey grids the given year
  bf <- apply(st_intersects(SweLandGrid, occ_sp %>% distinct(lokalnamn, .keep_all = T), sparse = FALSE), 2, function(col) { SweLandGrid[which(col), ]}) %>% 
    list_rbind() %>% 
    st_as_sf() %>% 
    st_set_crs(3021)
  
  
  grid <- sebms_swe_grid %>% 
    st_as_sf() %>% 
    st_set_crs(3021) %>% 
    suppressWarnings() %>% 
    st_transform(3021)
  
  
  # n_points_in_cell <- function(x, na.rm = TRUE){ 
  #   if (na.rm) length(na.omit(x)) else (length(x))
  # }
  # 
  
  # Make a raster of all grid cells covering Sweden
  rs <- rast(ext(grid), nrows = 62, ncols = 28, 
             crs = crs(grid))
  
  
  
  ## Sweden map
  tiff1 <- terra::rast(system.file("extdata", "MapSweden_RGB.png", 
                                   package = "sebmsR", mustWork = TRUE)) %>% 
    suppressWarnings()
  
  terra::ext(tiff1) <- c(1179998, 1948697, 6129692, 7679610)
  crs(tiff1) <- "epsg:3021"  
  
  tiff <- tiff1 %>%   
    terra::as.data.frame(xy=T) %>% 
    rename_with(.fn = ~c("x", "y","Red", "Green", "Blue", "Max")) %>% 
    filter(Red != 0)
  
  
  # Creating a colour scale for the occurrences fill
  
  pal_orig <- c(NA, rgb(234,173,68, alpha = 96, maxColorValue = 255),rgb(203,141,53, alpha = 96, maxColorValue = 255),rgb(171,109,37, alpha = 96, maxColorValue = 255), rgb(148,77,21, alpha = 96, maxColorValue = 255),rgb(92,69,4, alpha = 96, maxColorValue = 255))
  #pal_orig <- c("#EAAD44","#CB8D35","#AB6D25","#944D15","#5C4504")
  
  
  # Creating the plotting function
  speplot <- function(spda, spid) {
    
    # Create data frame to construct the fill colour for the given species + zeroes
    rl <- occ_sp %>%
      filter(speuid %in% c(spid, 135)) %>%
      rasterize(rs, field = "maxobs")
    
    rl[rl>4] <- 5 # Every max obs value over 5 should be 5
    
    df <- as.data.frame(rl, xy = T)
    colnames(df) <- c("x", "y", "value")
    
    # Make the plot
    ggplot() +
      geom_raster(data = tiff, aes(x = x, y = y,fill = rgb(r = Red, g = Green, b = Blue, maxColorValue = 255)), show.legend = FALSE) + # The Swedish map
      scale_fill_identity() + # This keep the correct original colours of map
      #coord_sf(expand = F) +
      new_scale_fill() + # Start new scale
      geom_sf(data = bf, alpha = 0, linewidth = 0.3, colour = rgb(128,128,128, maxColorValue = 255), inherit.aes = F) + # Visited survey grids the given year
      geom_tile(data = df, aes(x, y, fill = as.factor(value)), colour = rgb(128,128,128, maxColorValue = 255), inherit.aes = FALSE, alpha = 0.5, size = 0.2) + # Tiles/raster with occurrence data with values of the max observation of individuals per day 0-5+
      geom_sf(data = spda, colour = rgb(255,0,0,maxColorValue = 255), size = 0.5, inherit.aes = F) + # Species occurrences
      scale_fill_manual(name = NULL,
                        breaks = 0:5,
                        labels = c(0:4, "5+"),
                        values = pal_orig,
                        guide = "legend",
                        na.value = "transparent"
      ) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            legend.position = c(0.2,0.8),
            legend.spacing.y = unit(2, units = "mm"),
            legend.key.size = unit(5, units = "mm")) +
      guides(fill = guide_legend(byrow = TRUE))
    
    #scale_fill_gradient2(name = "Lokaler (n)", labels = c(1:4, ">= 5"), 
    # guide = "legend", na.value = "transparent", 
    # low = pal_orig[1], mid = pal_orig[3], high = pal_orig[5], 
    # midpoint = mean(df$value)) +
  }
  
  ggs <- occ_sp %>% 
    filter(maxobs > 0, speuid %in% species) %>% 
    group_by(speuid, art) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(plots = map2(data, speuid, speplot, .progress = "Making plots:"))
  
  map2(ggs$plots, ggs$art, ~sebms_ggsave(.x, .y, width = width, height = height, weathervar = glue("{year}")), .progress = "Saving plots:")
  
  if (print) {
    return(ggs$plots)
  }
  
}


