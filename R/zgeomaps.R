#' Swedish Map of SeBMS Site Data
#' 
#' Making use of raster grid and point data
#' 
#' @param occ_sp SpatialPoints with occurrence data
#' @param width the plot width, default 5 inches
#' @param height the plot height, default 3.5 inches
#' @return ggplot object
#' @import grid
#' @import magick
#' @import ggthemes
#' @import ggplot2
#' @importFrom rasterVis gplot
#' @importFrom terra ext rast rasterize crs crs<- coltab project values
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sf st_transform
#' @importFrom sp SpatialPixelsDataFrame
#' @importFrom cowplot ggdraw draw_grob
#' @export
#' 
sebms_species_site_plot <- function(occ_sp, 
  width = 5, height = 3.5) {
  
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
    

  rs <- rast(ext(grid), nrows = 62, ncols = 28, 
      #crs = terra::project(x=grid, y="espg:4326"),
      crs = "espg:3021")
  
  n_points_in_cell <- function(x, na.rm = TRUE) {
    if (na.rm) length(na.omit(x)) else (length(x))
  }
  
  p <- occ_sp 
  p <- st_transform(alla, crs("epsg:3021"))
  
  rl <- rasterize(alla, rs, fun = n_points_in_cell)
  idx_n_large <- which(values(rl) >= 5)
  rl[][idx_n_large] <- 5
  
  #pal_orig <- c("#EAAD44","#CB8D35","#AB6D25","#944D15","#5C4504")
  pal <- brewer.pal(7, "OrRd")[c(1, 4, 7)]
  
  #spdf <- as(rl, "SpatialPixelsDataFrame")
  spdf <- st_as_sf(rl)
  df <- as.data.frame(rl, xy=T)
  colnames(df) <- c("value", "x", "y")

  pdf <- as.data.frame(alla, xy=T)
  colnames(pdf) <- c("x", "y")

  #bg <- as.data.frame(as(a, "SpatialPixelsDataFrame"))
  #colnames(bg) <- c("value", "x", "y")
  
  bg <- sebms_swe_borders %>%
    st_as_sf() %>% 
    st_set_crs(3021)
  
#  tiff <- as(a, "SpatialPixelsDataFrame")
#  tiff <- spTransform(tiff, crs(grid))
#  tiff <- as.data.frame(tiff)
#  colnames(tiff) <- c("value", "x", "y")

  tiff <- rast(system.file("extdata", "MapSweden.tif", 
    package = "sebmsR", mustWork = TRUE))
  
  #raster::crs(tiff) <-  sp::CRS("+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs")
  
  sebms_data_swetiff <- tiff

    
  col_map <- function(rl) {
    cm <- colortable(rl)
    names(cm) <- 0:(length(cm) - 1)
    cm
  }
  
  layer1 <- 
    gplot(sebms_data_swetiff, maxpixels = 1e6) + 
    geom_raster(aes(x = x, y = y, fill = factor(value))) +
    #scale_fill_manual(values = col_map(sebms_data_swetiff), guide = "none") +
    geom_polygon(data = bg, 
      aes(x = LON, y = LAT, group = REGION), 
      fill = NA, color = "transparent", linewidth = 0.4) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none")
  
  p <- 
    ggplot() +
    #layer1 + 
    geom_tile(inherit.aes = FALSE, data = df, mapping = aes(x = y, y = x, fill = value), 
      alpha = 0.4, colour = "grey50", size = 0.2) + 
  #  geom_raster(inherit.aes = FALSE, data = df, aes(x = x, y = y, fill = value), 
  #    alpha = 0.4, color = "black") + 
    scale_fill_gradient2(name = "Lokaler (n)", labels = c(1:4, ">= 5"), 
      guide = "legend", na.value = "transparent", 
      low = pal[1], mid = pal[2], high = pal[3], 
        midpoint = mean(df$value)) + 
    geom_polygon(data = bg, 
      aes(x = LON, y = LAT, group = REGION), 
      fill = NA, color = "transparent", size = 0.4) +
    geom_point(data = pdf, aes(x = y, y = x), 
      size = 0.05, alpha = 0.4, 
      color = "darkred", fill = "darkred") +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none")
  
  p_legend <- p + 
    theme(legend.position = "right")
    
  gg_legend <- function(p) { 
    tmp <- ggplot_gtable(ggplot_build(p)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    legend
  } 
  
  legend <- gg_legend(p_legend)

  save_pplot <- function(g, fn, w = width, h = height) {
    ggsave(
      filename = fn, 
      plot = g, device = "png", 
      width = w, height = h)
  }
  
  fn1 <- tempfile("01-", fileext = ".png")
  fn2 <- tempfile("02-", fileext = ".png")
  fn3 <- tempfile("03-", fileext = ".png")
  
  layer1 %>% save_pplot(fn = fn1, h=3.5, w = 5)
  p %>% save_pplot(fn = fn2, h=3.5, w = 5)
  
  l_plot <- cowplot::ggdraw() + cowplot::draw_grob(legend) 
  l_plot %>% save_pplot(fn = fn3, h=3.5, w = 5)
  
  i1 <- image_read(fn1)%>% image_transparent("white")
  i2 <- image_read(fn2) %>% image_transparent("white")
  i3 <- image_read(fn3) %>% image_transparent("white")
  i <- image_composite(i1, i2, operator = "Over") %>% image_trim()
  legend <- i3 %>% image_trim()
  
  res <- list(plot = i, legend = legend)
  unlink(c(fn1, fn2, fn3))
  return (res)
}
