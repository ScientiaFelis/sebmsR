#' Swedish Map of SeBMS Sites
#' 
#' Producing map for all sites on Swedish grid.
#' 
#' @import grid
#' @import magick
#' @import ggthemes
#' @import ggplot2
#' @import sf
#' @importFrom rasterVis gplot
#' @importFrom terra ext ext<- rast rasterize crs crs<- coltab project values
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sp SpatialPixelsDataFrame
#' @importFrom cowplot ggdraw draw_grob 
#' 
#' @param year the year of interest
#' @param width the plot width, default 5 inches
#' @param height the plot height, default 3.5 inches
#' @return ggplot object

#' @export
sebms_sites_map <- function(year=2021, width=5, height=3.5) {
  
  SweLandGrid <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "SweLandGrid") %>% 
    st_set_crs(3021)
  
  alla <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "alla_2010-2014_sites") %>% 
    st_set_crs(3021)
  
  bf <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "ButterflySquares0423") %>% 
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
  #FIXME: make alla as raster too
  
  ggplot(data = tiff, aes(x = x, y = y)) +                   #plot map
    geom_raster(aes(fill = rgb(r = Red, g = Green, b = Blue, maxColorValue = 255)), show.legend = FALSE) +
    geom_sf(data = bf, inherit.aes = F, alpha = 0, linewidth = 0.3, colour = "black") +
    geom_sf(data = alla, colour = "red", alpha = 0.2, inherit.aes = F) +
    scale_fill_identity() +
    theme_void()
  
  ggsave("Butterfly_sites.png", width = 7, height = 15, units = "cm")
  
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
#'
#' @inheritParams sebms_sites_map
#' @param occ_sp SpatialPoints with occurrence data
#' @param species the species of interest as a species id
#' @param print logical; should the plots be printed in window, default FALSE
#' @return ggplot object of map with grid coloured by local density and with
#'   species occurence points.

#' @export
sebms_distribution_map <- function(year=2021, species = 118, width=12, height=18, occ_sp, print = FALSE) {
  
  if (missing(occ_sp)) {
    occ_sp <- sebms_occurances_distribution(year = year, Art = species) %>%
      select(art, lokalnamn, lat, lon) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = "espg:3006") %>% 
      st_set_crs(3006) %>% 
      st_transform(3021)
  }
  
  SweLandGrid <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "SweLandGrid") %>% 
    st_set_crs(3021)
  
  alla <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "alla_2010-2014_sites") %>% 
    st_set_crs(3021)
  
  bf <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "ButterflySquares0423") %>% 
    st_set_crs(3021)
  
  #a = rast("data-raw/figures/MapDistribution-data/R_files_for_similar_map/MapSweden.tif")
  
  grid <- sebms_swe_grid %>% 
    st_as_sf() %>% 
    st_set_crs(3021) %>% 
    suppressWarnings() %>% 
    st_transform(3021)
  
  bg <- sebms_swe_borders %>%
    st_as_sf() %>% 
    st_set_crs(3857) %>% 
    st_transform(3021)
  
  
  ## Species raster
  n_points_in_cell <- function(x, na.rm = TRUE){ 
    if (na.rm) length(na.omit(x)) else (length(x))
  }
  
  rs <- rast(ext(grid), nrows = 62, ncols = 28, 
             crs = crs(grid))
  
  rl <- rasterize(occ_sp, rs, fun = n_points_in_cell)
  idx_n_large <- which(values(rl) >= 5)
  rl[][idx_n_large] <- 5
  
  
  df <- as.data.frame(rl, xy = T)
  colnames(df) <- c("x", "y", "value")
  
  
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
  
  pal_orig <- c("#EAAD44","#CB8D35","#AB6D25","#944D15","#5C4504")
  pals <- brewer.pal(7, "OrRd")[c(1, 4, 7)]
  
  # QUESTION: Is it possible to add a colour to each df grid value and let scale_fill identity use that with fill = colour in geom_tile()?
  speplot <- function(spda) {
    ggplot() +
      geom_raster(data = tiff, aes(x = x, y = y,fill = rgb(r = Red, g = Green, b = Blue, maxColorValue = 255)), show.legend = FALSE) +
      geom_sf(data = spda, colour = "red", size = 0.8, inherit.aes = F) +
      geom_sf(data = bf, alpha = 0, linewidth = 0.3, colour = "black", inherit.aes = F) +
      scale_fill_identity() +
      #coord_sf(expand = F) +
      new_scale_fill() +
      geom_tile(aes(x, y, fill = as.factor(value)), data = df, inherit.aes = FALSE, alpha = 0.5, size = 0.2) +
      # scale_fill_gradient2(name = "Lokaler (n)",
      #                     breaks = 1:5,
      #                     labels = c(1:4, ">= 5"),
      #                     guide = "legend",
      #                     na.value = "transparent",
      #                     low = pal_orig[1], mid = pal_orig[3], high = pal_orig[5],
      #                     midpoint = mean(df$value)) +
      scale_fill_manual(name = "Lokaler (n)",
                        breaks = 1:5,
                        labels = c(1:4, ">= 5"),
                        values = pal_orig,
                        guide = "legend",
                        na.value = "transparent",
                        #low = pal_orig[1], mid = pal_orig[3], high = pal_orig[5],
                        #midpoint = mean(df$value)
      ) +
      theme_void() +
      theme(plot.background = element_rect(fill = "white", colour = "white"),
            #legend.position = "left",
            legend.position = c(0.2,0.8))
    
    #scale_fill_gradient2(name = "Lokaler (n)", labels = c(1:4, ">= 5"), 
    # guide = "legend", na.value = "transparent", 
    # low = pal_orig[1], mid = pal_orig[3], high = pal_orig[5], 
    # midpoint = mean(df$value)) +
  }
  
  ggs <- occ_sp %>% 
    group_by(art) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(plots = map(data, speplot))
  
  map2(ggs$plots, ggs$art, ~sebms_ggsave(.x, .y, width = width, height = height, weathervar = glue("{year}")))
  
   if (print) {
    return(ggs$plots)
  }
}


