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
#' @importFrom terra ext rast rasterize crs crs<- coltab project values
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sp SpatialPixelsDataFrame
#' @importFrom cowplot ggdraw draw_grob 
#' 
#' @param year the year of interest
#' @param width the plot width, default 5 inches
#' @param height the plot height, default 3.5 inches
#' @return ggplot object

#' @export
#
sebms_sites_map <- function(year=2021, width=5, height=3.5) {
  
  SweLandGrid <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "SweLandGrid") %>% 
    st_set_crs(3021)
  
  alla <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "alla_2010-2014_sites") %>% 
    st_set_crs(3021) %>% 
    st_transform(3006)
  
  bf <- st_read("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "ButterflySquares0423") %>% 
    st_set_crs(3021) %>% 
    st_transform(3006)
  
  #a = rast("data-raw/figures/MapDistribution-data/R_files_for_similar_map/MapSweden.tif")
  
  grid <- sebms_swe_grid %>% 
    st_as_sf() %>% 
    st_set_crs(3021) %>% 
    st_transform(3021)
  
  bg <- sebms_swe_borders %>%
    st_as_sf() %>% 
    st_set_crs(3021) %>% 
    st_transform(3021)
  
  ## FIXME: colours of map not good. Is there a map colour guide.
  # TODO: Check out ggRGB in RStoolbox package
  tiff <- terra::rast(system.file("extdata", "MapSweden.tif", 
                           package = "sebmsR", mustWork = TRUE))
  crs(tiff) <- "epsg:3006"
  
 tiff <-  raster::stack(system.file("extdata", "MapSweden.tif", 
                            package = "sebmsR", mustWork = TRUE)) %>% 
  raster::as.data.frame(xy=T)
 
 
 
  tifdf <- raster::brick(x = system.file("extdata", "MapSweden_RGB.tif",
                                         package = "sebmsR", mustWork = TRUE), crs = "epsg:3006") %>%
    as("SpatialPixelsDataFrame") %>% 
    raster::as.data.frame(xy=T)
  
  col_map <- function(rl) {
    cm <- coltab(rl) %>% 
      bind_rows() %>% 
      mutate(hex = rgb(red, green, blue, maxColorValue = alpha),
             values = paste0(value+1," = ", hex)) %>% 
      pull()
    #names(cm) <- 0:(length(cm) - 1)
    cm
  }
  
  
  gplot(tiff, maxpixels = 1e6) +
    geom_raster(aes(x = x, y = y, fill = value), show.legend = F) +
    #geom_sf(data = bg) +
    geom_sf(data = bf, inherit.aes = F, alpha = 0, linewidth = 0.3, colour = "black") +
    geom_sf(data = alla, colour = "red", alpha = 0.2, inherit.aes = F) +
    #scale_fill_distiller(type = "div", palette = "RdYlGn") +
    # scale_fill_identity() +
    scale_fill_gradientn(colours = colmap, guide = FALSE) +
    theme_void()# + theme(panel.background = element_rect(fill = "white"))
  
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
#' @importFrom rasterVis gplot
#' @importFrom terra ext rast rasterize crs crs<- coltab project values
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sp SpatialPixelsDataFrame
#' @importFrom cowplot ggdraw draw_grob 
#'
#' @inheritParams sebms_sites_map 
#' @param occ_sp SpatialPoints with occurrence data
#' @param species the species of interest as a species id
#' @return ggplot object

#' @export
#
sebms_distribution_map <- function(occ_sp, year=2021, species = 80, width= 5, height=3.5) {
  
  
  # TODO: add in species data from sebms database.
  
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
  
  ## FIXME: colours of map not good. Is there a map colour guide.
  tiff <- rast(system.file("extdata", "MapSweden.tif", 
                           package = "sebmsR", mustWork = TRUE)) 
  
  
  col_map <- function(rl) {
    cm <- coltab(rl) %>% 
      bind_rows() %>% 
      mutate(hex = rgb(red, green, blue, maxColorValue = alpha),
             values = paste0(value," = ", hex)) %>% 
      pull()
    #names(cm) <- 0:(length(cm) - 1)
    cm
  }
  
  terrain.colors(25)
  
  gplot(tiff, maxpixels = 1e6) +
    geom_raster(aes(x = x, y = y, fill = factor(value)), show.legend = F) +
    #geom_sf(data = bg) +
    geom_sf(data = bf, inherit.aes = F, alpha = 0, linewidth = 0.3, colour = "black") +
    geom_sf(data = alla, colour = "red", alpha = 0.2, inherit.aes = F) +
    #scale_fill_distiller(type = "div", palette = "RdYlGn") +
    scale_fill_manual(values = col_map(tiff), guide = "none") +
    theme_void()# + theme(panel.background = element_rect(fill = "white"))
  
}


