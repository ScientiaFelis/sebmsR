#' Swedish Map of SeBMS Sites
#'
#' Producing map for all visited sites on Swedish grid separating transects and
#' points.
#' 
#' @import ggplot2
#' @import sf
#' @importFrom terra ext ext<- rast rasterize crs crs<- coltab project values
#' @importFrom ggnewscale new_scale_fill
#' @importFrom dplyr mutate group_by ungroup
#' @importFrom tidyr nest
#' @importFrom purrr map map2
#' @importFrom glue glue
#'
#' @param year the year of interest
#' @param width the plot width, default 12 inches
#' @param height the plot height, default 18 inches
#' @param occ_sp SpatialPoints with occurrence data
#' @param print logical; should the plots be printed in window, default FALSE
#' 
#' @return Figures in png for points, and transects the given year
#' @export
sebms_sites_map <- function(year=2021, width = 12, height = 18, occ_sp, print = FALSE) {
  
 if (missing(occ_sp)) { #Load in data for all species from given year
    occ_sp <- sebms_occurances_distribution(year = year) %>%
      transmute(sitetype, speuid, lokalnamn, lat, lon) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = "espg:3006") %>% 
      st_set_crs(3006) %>% 
      st_transform(3021)
  }
  
  SweLandGrid <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "SweLandGrid", quiet = TRUE) %>% 
    st_set_crs(3021)

  
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
  
  # Creating the plotting function
  speplot <- function(spda, spid) {
    
    # Create a grid for all the visited survey grids the given year and site type
    bf <- apply(st_intersects(SweLandGrid, occ_sp %>% filter(sitetype %in% spid) %>% distinct(lokalnamn, .keep_all = T), sparse = FALSE), 2, function(col) { SweLandGrid[which(col), ]}) %>% 
      list_rbind() %>% 
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
            legend.position = c(0.1,0.91),
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
  
  map2(ggs$plots, ggs$sitetype, ~sebms_ggsave(.x, .y, width = width, height = height, weathervar = glue("{year}")), .progress = "Saving plots:")
  
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
sebms_distribution_map <- function(year=2023, Art = 118, width=9, height=18, occ_sp, print = FALSE) {
  
  if (missing(occ_sp)) { #Load in data for all species from given year
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
    suppressWarnings()
  
  terra::ext(tiff1) <- c(1179998, 1948697, 6129692, 7679610)
  crs(tiff1) <- "epsg:3021"  
  
  tiff <- tiff1 %>%   
    terra::as.data.frame(xy=T) %>% 
    rename_with(.fn = ~c("x", "y","Red", "Green", "Blue", "Max")) %>% 
    filter(Red != 0)
  
  
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
             colour = fct_rev(colour))
    
    # Make the plot
    ggplot() +
      geom_raster(data = tiff, aes(x = x, y = y,fill = rgb(r = Red, g = Green, b = Blue, maxColorValue = 255)), show.legend = FALSE) + # The Swedish map
      scale_fill_identity() + # This keep the correct original colours of map
      new_scale_fill() + # Start new scale
      geom_sf(data = bf, alpha = 0, linewidth = 0.3, colour = rgb(128,128,128, maxColorValue = 255), inherit.aes = F) + # Visited survey grids the given year
      geom_tile(data = df, aes(x, y, fill = colour), colour = rgb(128,128,128, maxColorValue = 255), inherit.aes = FALSE, alpha = 0.3, size = 0.2) + # Tiles/raster with occurrence data with values of the max observation of individuals per day 0-5+
      geom_sf(data = spda, colour = rgb(255,0,0,maxColorValue = 255), size = 0.1, inherit.aes = F) + # Species occurrences
      # scale_fill_manual(name = NULL,
      #                   breaks = c("0", "1", "2", "3", "4", "5"),
      #                   labels = c("0", "1", "2", "3", "4", "5+"),
      #                   values = pal_orig,
      #                   guide = "legend",
      #                   drop = F,
      #                   na.value = "transparent"
      # ) +
      scale_fill_identity(name = NULL,
                          guide = "legend",
                          labels = c("0", "1", "2", "3", "4", "5+")
      ) +
      coord_sf(expand = F) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            plot.margin = margin(t = 1,r = 0,b = 1,l = 0, unit = "mm"),
            legend.position = c(0.1,0.91),
            legend.spacing.y = unit(2, units = "mm"),
            legend.key.size = unit(3, units = "mm")) +
      guides(fill = guide_legend(byrow = TRUE))
    
  }
  
  ggs <- occ_sp %>% 
    filter(maxobs > 0, speuid %in% Art) %>% 
    group_by(speuid, art) %>% 
    nest() %>% # Nest per species to save one png per species
    ungroup() %>% 
    mutate(plots = map2(data, speuid, speplot, .progress = "Making plots:"))
  
  map2(ggs$plots, ggs$art, ~sebms_ggsave(.x, .y, width = width, height = height, weathervar = glue("{year}")), .progress = "Saving plots:")
  
  if (print) {
    return(ggs$plots)
  }
  
}


