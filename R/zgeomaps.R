#' Swedish map of SeBMS site/location data, making use of raster grid and point data
#' @param occ_sp SpatialPoints with occurrence data
#' @param width the plot width, default 5 inches
#' @param height the plot height, default 3.5 inches
#' @return ggplot object
#' @import grid
#' @import magick
#' @import ggthemes
#' @import ggplot2
#' @importFrom rasterVis gplot
#' @importFrom raster extent raster rasterize crs crs<- colortable projection values
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sp SpatialPixelsDataFrame spTransform
#' @importFrom cowplot ggdraw draw_grob
#' @export
#' 
sebms_species_site_plot <- function(occ_sp, 
  width = 5, height = 3.5) {
  
  grid <- sebms_swe_grid

  rs <- raster(extent(grid), nrows = 62, ncols = 28, 
      crs = projection(grid))
  
  n_points_in_cell <- function(x, na.rm = TRUE) 
    if (na.rm) length(na.omit(x)) else (length(x))
  
  p <- occ_sp #spTransform(alla2, crs("+init=epsg:3857"))
  
  rl <- rasterize(p, rs, fun = n_points_in_cell)
  idx_n_large <- which(values(rl) >= 5)
  rl[][idx_n_large] <- 5
  
  #pal_orig <- c("#EAAD44","#CB8D35","#AB6D25","#944D15","#5C4504")
  pal <- brewer.pal(7, "OrRd")[c(1, 4, 7)]
  
  spdf <- as(rl, "SpatialPixelsDataFrame")
  df <- as.data.frame(spdf)
  colnames(df) <- c("value", "x", "y")

  pdf <- as.data.frame(occ_sp)
  colnames(pdf) <- c("x", "y")

  #bg <- as.data.frame(as(a, "SpatialPixelsDataFrame"))
  #colnames(bg) <- c("value", "x", "y")
  
  bg <- spTransform(sebms_swe_borders, crs(grid))
  
#  tiff <- as(a, "SpatialPixelsDataFrame")
#  tiff <- spTransform(tiff, crs(grid))
#  tiff <- as.data.frame(tiff)
#  colnames(tiff) <- c("value", "x", "y")

  tiff <- raster(system.file("extdata", "MapSweden.tif", 
    package = "sebmsR", mustWork = TRUE))
  
  raster::crs(tiff) <-  sp::CRS("+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs")
  
  sebms_data_swetiff <- tiff

    
  col_map <- function(rl) {
    cm <- colortable(rl)
    names(cm) <- 0:(length(cm) - 1)
    cm
  }
  
  layer1 <- 
    gplot(sebms_data_swetiff, maxpixels = 1e6) + 
    geom_raster(aes(x = x, y = y, fill = factor(value))) +
    scale_fill_manual(values = col_map(sebms_data_swetiff), guide = "none") +
    geom_polygon(data = bg, 
      aes(x = long, y = lat, group = group), 
      fill = NA, color = "transparent", size = 0.4) +
    coord_fixed() +
    theme_map() +
    theme(legend.position = "none")
  
  p <- 
    ggplot() +
    #layer1 + 
    geom_tile(inherit.aes = FALSE, data = df, mapping = aes(x = x, y = y, fill = value), 
      alpha = 0.4, colour = "grey50", size = 0.2) + 
  #  geom_raster(inherit.aes = FALSE, data = df, aes(x = x, y = y, fill = value), 
  #    alpha = 0.4, color = "black") + 
    scale_fill_gradient2(name = "Lokaler (n)", labels = c(1:4, ">= 5"), 
      guide = "legend", na.value = "transparent", 
      low = pal[1], mid = pal[2], high = pal[3], 
        midpoint = mean(df$value)) + 
    geom_polygon(data = bg, 
      aes(x = long, y = lat, group = group), 
      fill = NA, color = "transparent", size = 0.4) +
    geom_point(data = pdf, aes(x = x, y = y), 
      size = 0.05, alpha = 0.4, 
      color = "darkred", fill = "darkred") +
    coord_fixed() +
    theme_map() +
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
  
  layer1 %>% sebms_ggsave(filename = fn1)
  p %>% sebms_ggsave(filename = fn2)
  
  l_plot <- cowplot::ggdraw() + cowplot::draw_grob(legend) 
  l_plot %>% sebms_ggsave(filename = fn3)
  
  i1 <- image_read(fn1)%>% image_transparent("white")
  i2 <- image_read(fn2) %>% image_transparent("white")
  i3 <- image_read(fn3) %>% image_transparent("white")
  i <- image_composite(i1, i2, operator = "Over") %>% image_trim()
  legend <- i3 %>% image_trim()
  
  res <- list(plot = i, legend = legend)
  unlink(c(fn1, fn2, fn3))
  return (res)
}

#' Plot of sunhours using Leaflet
#' @param rl raster layer, if missing default is to use smhi_sunhours_rb$X2016
#' @return leaflet map
#' @importFrom leaflet colorNumeric
#' @importFrom raster values
#' @export
sebms_sunhours_leaflet <- function(rl, ...) {
  
  if (missing(rl)) {
    message("missing rl param, using default raster layer for 2016")
    rl <- smhi_sunhours_rb$X2016
  }

  pal <- leaflet::colorNumeric("RdYlBu", raster::values(rl), 
    reverse = TRUE, na.color = "transparent")

  leaflet(...) %>% 
#    addTiles(attribution = NULL, layerId = NULL, group = NULL) %>%
#    addTiles("//server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}", attribution = NULL, layerId = NULL, group = NULL) %>%
#    addTiles("//stamen-tiles-{s}.a.ssl.fastly.net/watercolor/{z}/{x}/{y}.png", attribution = NULL, layerId = NULL, group = NULL) %>%
    addRasterImage(rl, opacity = 0.8, colors = pal)
  
}

#' Plot of sunhours data using ggplot
#' @param rl raster layer, if missing default is to use smhi_sunhours_rb$X2016
#' @param show_legend boolean to indicate if legend should be included
#' @return ggplot object
#' @import ggplot2
#' @importFrom raster raster
#' @importFrom sp SpatialPixelsDataFrame
#' @importFrom RColorBrewer brewer.pal
#' @export
sebms_sunhours_plot <- function(rl, show_legend = FALSE) {
  
  if (missing(rl)) {
    message("missing rl param, using default raster layer")
    rl <- smhi_sunhours_rb$X2016
  }

  spdf <- as(rl, "SpatialPixelsDataFrame")
  df <- as.data.frame(spdf)
  colnames(df) <- c("value", "x", "y")

  # from http://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=7
  pal <- brewer.pal(11, "RdYlBu")[c(2, 6, 10)]
  
  #pal <- leaflet::colorNumeric("RdYlBu", raster::values(rl), 
  #  reverse = TRUE, na.color = "transparent")

  p <- 
    ggplot() +  
    geom_raster(data = df, aes(x = x, y = y, fill = value), 
      alpha = 1) + 
    scale_fill_gradient2(name = "Soltimmar (h)", 
      guide = "legend", 
      na.value = "transparent", 
      low = pal[3], mid = pal[2], high = pal[1],
      midpoint = mean(df$value)) + 
    geom_polygon(data = sebms_swe_borders, 
      aes(x = long, y = lat, group = group), 
      fill = NA, color = "grey65", size = 0.4) +
    coord_equal() +
    theme_map() +
    theme(legend.position = "none", panel.grid.major = element_blank())
  
  if (show_legend) 
    p + theme(legend.position = "right")
  else
    p
}
