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
#' @param Region character or reg ex; which region do you want. Possible values are:
#'   'SGot', 'OGot', 'VGotSve', 'OSve', 'NSveNor', 'NNor'.
#' @param occ_sp SpatialPoints with occurrence data
#' @param width the plot width, default 12 inches
#' @param height the plot height, default 18 inches
#' @param maptype what survey type to produce map on, can be 'Transect', 'Point', 'Transekt', 'Punkt',
#'   or 'both' (default). The 'Transect' and 'Point' can be
#'   abbreviated to 'T' and 'P'.
#' @param print logical; should the plots be printed in window, default FALSE
#' @param write logical; should the file be written to a png, default to TRUE
#'
#' @return Figures in png for points, and transects the given year
#' @export
sebms_sites_map <- function(year = lubridate::year(lubridate::today())-1, occ_sp, Län = ".", Region = ".", Landskap = ".", Kommun = ".", width = 12, height = 18, maptype = "both", filepath = getwd(), tag = NULL, print = FALSE, write = TRUE, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84)) {
  
  if (missing(occ_sp)) { #Load in data for all species from given year
    occ_sp <- sebms_occurances_distribution(year = year, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
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
  
  if (str_detect(maptype, "[Pp][ou]i?nk?t|[Pp]$")) {
    occ_sp <- occ_sp %>% 
      filter(sitetype == "P")
  }
  
  if (str_detect(maptype, "[Tt]ranse[ck]t|[Tt]$")) {
    occ_sp <- occ_sp %>% 
      filter(sitetype == "T")
  }
  
  
  ggs <- occ_sp %>%
    distinct(sitetype, lokalnamn, .keep_all = T) %>% 
    group_by(sitetype) %>% 
    nest() %>% # Nest per species to save one png per species
    ungroup() %>% 
    mutate(plots = map2(data, sitetype, speplot, .progress = "Making plots:"))
  
  
  #Set year name  
  if (length(year)>1) {
    yearname <- paste0(min(year),"-",max(year))  
  }else {
    yearname <- year
  }
  
  #set tag
  if (is.null(tag)) {
    tag = ""
  }else {
    tag = glue("_{tag}")
  }
  #set filepath
  filepath <- normalizePath(filepath)
  
  if (write) {
    map2(ggs$plots, ggs$sitetype, ~sebms_ggsave(.x, glue("{filepath}/{.y}"), width = width, height = height, weathervar = glue("{yearname}{tag}")), .progress = "Saving plots:")
  }
  
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
sebms_distribution_map <- function(year = lubridate::year(lubridate::today())-1, occ_sp, Art = 1:200, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filepath = getwd(), tag = NULL, width=9, height=18, print = FALSE, write = TRUE,  verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84)) {
  
  if (missing(occ_sp)) { # Load in data for all species from given year,
    # without species restriction to get all sites visited
    occ_sp <- sebms_occurances_distribution(year = year, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) %>%
      transmute(speuid, art, lokalnamn, lat, lon, maxobs = as.numeric(max)) %>% 
      mutate(art = str_replace_all(art, "/", "-")) %>% 
      st_as_sf(coords = c("lon", "lat"), crs = "espg:3006") %>% 
      st_set_crs(3006) %>% 
      st_transform(3021)
  }
  

  
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
  
  
  if (write) {
    
    # set year variable  
    if (length(year)>1) {
      yearname <- paste0(min(year),"-",max(year))  
    }else {
      yearname <- year
    }
    
    #set tag
    if (is.null(tag)) {
      tag = ""
    }else {
      tag = glue("_{tag}")
    }
    #set filepath
    filepath <- normalizePath(filepath)
    
    
    map2(ggs$plots, ggs$art, ~sebms_ggsave(.x, glue("{filepath}/{.y}"), width = width, height = height, weathervar = glue("{yearname}{tag}")), .progress = "Saving plots:")
    
  }
  
  if (print) {
    return(ggs$plots)
  }
  
}



#' Create Local Maps with Transect and Point Locals
#'
#' Creates a map of the County or Municipality with the transect and point data marked.You
#' need to be on a Lund university or SLU network, or LU/SLU VPN to get the map as it is
#' created from the SLU WMS topomap.
#'
#' @inheritParams sebms_sites_map
#'
#' @param zoomlevel the level of zoom on map. This is set automatically but this argument
#'   allow for changing this if wanted.
#' @param showgrid logical; should the grid show, defaults to FALSE
#' @param gridtype which grid to show on the map, can take the value of 'square5',
#'   'square10', or 'hex', can be shortened to '5','10', and 'h'. Ignored if showgrid =
#'   FALSE
#' @param active_site_cutoff the year where the site is considered old, and get a smaller
#'   point size on the map (default to no year, all are active).
#' @param showsite logical; whether to show the sites on the map
#' @param onemap logical; wheter to make one map with all sites or ekse two maps with
#'   points and transects separately.
#' @import leaflet
#' @importFrom mapview mapshot2
#' @import webshot2
#' @import ggplot2
#' @importFrom sf st_intersection st_as_sf st_transform st_coordinates
#' @import dplyr
#' @importFrom lubridate year today
#' @importFrom tidyr nest
#' @importFrom purrr walk2 map2
#' @importFrom glue glue
#' @returns one or two png files with a map of the chosen area with points or transects
#'   marked.
#' @export

sebms_regional_site_map <- function(year = lubridate::year(lubridate::today())-1, occ_sp, Län = ".", Region = ".", Landskap = ".", Kommun = ".", filepath = getwd(), tag = NULL, active_site_cutoff = NULL, width = 12, height = 18, zoomlevel = NULL, maptype = "both", showsite = T, showgrid = F, gridtype = "10", onemap = F, print = FALSE, write = TRUE, verification = c(109,110,111), source = c(54,55,56,63,64,66,67,84)) {
  
  message("Make sure to be on Lund university network or LU VPN to get the map to work!")
  
  if (missing(occ_sp)) { # Load in data for all species from given year,
    # without species restriction to get all sites visited
    occ_sp <- sebms_occurances_distribution(year = year, Län = Län, Region = Region, Landskap = Landskap, Kommun = Kommun, verification = verification, source = source) 
    #drop_na() %>% 
  }
  
  occ_sp <- occ_sp %>% 
    transmute(sitetype, lokalnamn, lat, lon, year = year(obsdag)) %>% 
    mutate(colour = if_else(sitetype == "T", "#1F78B4", "#CE2D30"), # Set colours depending on sitetype
           radius = 6.5) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = "espg:3006") %>% 
    st_set_crs(3006) %>% 
    st_transform(4326)
  # fix size of locale circles based on age
  if (!is.null(active_site_cutoff)) {
    occ_sp <- occ_sp %>% 
      mutate(radius = if_else(year <= active_site_cutoff, 4,6.5))
  }
  
  
  # Picking out the borders
  #Län <- paste0(Län, collapse = "|")
  if (Län != ".") {
    border <- Counties %>% 
      filter(str_detect(NAME_1, Län)) #Filter out the right borders
    
    if (showgrid) {
      if (str_detect(gridtype, "5")) {
        segrid <- sebms_5_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      } else if (str_detect(gridtype, "10")) {
        segrid <- sebms_10_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      } else if (str_detect(gridtype, "h")) {
        segrid <- sebms_hex_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      }
      
    }
    
    
    Region <- Län # to use when setting name for the saved png
    
    CPK <- centerPL %>% 
      filter(str_detect(LNNAMN, Län)) # This loads in the center point and automatic zoom level.
    
  }
  
  if (Region != ".") {
    border <- Regions %>% 
      filter(str_detect(RegionName, Region))  #Filter out the right borders
    
    if (showgrid) {
      if (str_detect(gridtype, "5")) {
        segrid <- sebms_5_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      } else if (str_detect(gridtype, "10")) {
        segrid <- sebms_10_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      } else if (str_detect(gridtype, "h")) {
        segrid <- sebms_hex_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      }
      
    }

    #Region <- Län # to use when setting name for the saved png
    
    CPK <- centerPR %>% 
      filter(str_detect(RegionName, Region)) # This loads in the center point and automatic zoom level.
    
  }
  
  if (Kommun != ".") {
    border <- Kommuner %>% 
      filter(str_detect(NAME_2, Kommun)) 
    
    if (showgrid) {
      if (str_detect(gridtype, "5")) {
        segrid <- sebms_5_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      } else if (str_detect(gridtype, "10")) {
        segrid <- sebms_10_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      } else if (str_detect(gridtype, "h")) {
        segrid <- sebms_hex_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      }
      
    }
    
    Region <- Kommun
    
    CPK <- centerPK %>% 
      filter(str_detect(KnNamn, Kommun))
  }
  
  if (Landskap != ".") {
    border <- Landskapen %>% 
      filter(str_detect(reg_name, Landskap))   
    
    if (showgrid) {
      if (str_detect(gridtype, "5")) {
        segrid <- sebms_5_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      } else if (str_detect(gridtype, "10")) {
        segrid <- sebms_10_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      } else if (str_detect(gridtype, "h")) {
        segrid <- sebms_hex_grid %>% st_intersection(border) # crop the grid to just the actual area
        
      }
      
    }
    
    Region <- Landskap
    
    CPK <- centerPLsk %>% 
      filter(str_detect(reg_name, Landskap))
  }
  
  
  wms_topo_nedtonad <- "https://hades.slu.se/lm/topowebb/wms/v1" # wms topografic map from SLU
  
  #TODO: Optional: region2: smalare linje 0,66 mm röd #CE2D30, 60% opacity, longdash
  
  # Function to create the map
  
  # Add automatic zoom level if not set
  if (is.null(zoomlevel)) {
    zoomlevel = CPK$zoom
  }
  
  
  locplot <- function(data, sittyp) {
    lpl <- leaflet(data) %>%
      setView(lng = CPK$X, lat = CPK$Y, zoom = zoomlevel) %>% # Set center and zoom
      addWMSTiles(
        wms_topo_nedtonad,
        layers = "topowebbkartan_nedtonad",
        options = WMSTileOptions(format = "image/png", transparent = TRUE)
      ) 
    
    # Add grid if wanted 
    # This is set here to be below the borders and points
    if (showgrid) {
      if (str_detect(gridtype, "h")) {
        
        segrid <- segrid %>%
          mutate(fillc = if_else(map_lgl(st_contains(segrid, occ_sp %>% filter(str_detect(sitetype, sittyp))), is_empty), "#ffffff", "#C06020"))
        lpl <- lpl %>% 
          addPolygons(data = segrid, weight = 1, fill = T, fillColor = segrid$fillc)
      } else{
        
        lpl <- lpl %>% 
          addPolygons(data = segrid, weight = 1, fill = F)
      }
    } 
    # Add the rest of the features (border lines and points showing localities)
    #TODO: Fix so you can show only filled hex grid
    lpl <- lpl %>% 
      addPolylines(data = border, # add border lines
                   color = "#CE2D30",
                   weight = 5,
                   opacity = 1,
                   dashArray = c("21","9", "21"))
    
    if (showsite) { # Should you show the sites or not in the map
      
      if (onemap) {
        lpl <- lpl %>% 
          addCircleMarkers(data = data,
                           radius = 4,
                           color = "black",
                           weight = 0.6,
                           fill = TRUE,
                           fillColor = "#000000", # fill depending on site type
                           fillOpacity = 1,
                           opacity = 1,
                           label = data$lokalnamn)
      } else {
        lpl <- lpl %>% 
          addCircleMarkers(data = data,
                           radius = data$radius,
                           color = "black",
                           weight = 0.6,
                           fill = TRUE,
                           fillColor = data$colour, # fill depending on site type
                           fillOpacity = 1,
                           opacity = 1,
                           label = data$lokalnamn) 
      }
    }
    
    lpl <- lpl %>% 
      addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 150, imperial = F)) # this does not work with simple webshot save
    
    return(lpl)
  }
  
  
  
  #set tag
  if (is.null(tag)) {
    tag = ""
  }else {
    tag = glue("_{tag}")
  }
  #set filepath
  filepath <- normalizePath(filepath)
  
  
  # Save a plot with both transect and point in one map.
  if (onemap) { # This is if you want only one map with both points and transects
    ggs <- list() # Make a ggs data frame to be able to store the plot in a variable ('plots'), for the print to work.
    ggs$plots <- occ_sp %>%
      distinct(lokalnamn, .keep_all = T) %>% 
      locplot(sittyp = "T|P") 
    
    if (write){
      ggs$plots %>% 
        mapshot2(file = glue("{filepath}/{Region}{tag}.png"), remove_controls = c("zoomControl", "layersControl", "homeButton",  "drawToolbar", "easyButton", "control"))
    }
    
  } else { # This is of you want separate point and transect maps (default)
    
    
    
    # Save a plot per site type as png
    
    if (str_detect(maptype, "[Pp][ou]i?nk?t|[Pp]$")) {
      occ_sp <- occ_sp %>% 
        filter(sitetype == "P") # filter out if only point locales are wanted
    }
    
    if (str_detect(maptype, "[Tt]ranse[ck]t|[Tt]$")) {
      occ_sp <- occ_sp %>% 
        filter(sitetype == "T") # filter out if only transect locales are wanted
    }
    
    ggs <- occ_sp %>%
      distinct(sitetype, lokalnamn, .keep_all = T) %>% 
      group_by(sitetype) %>%  ## Create a map per site type
      nest() %>% # QUESTION: Nest per species to save one png per species?
      ungroup() %>% 
      mutate(plots = map2(data, sitetype, locplot, .progress = "Making plots:"))
    
    if (write){ #should you write the png file
      walk2(ggs$plots, ggs$sitetype, ~mapshot2(.x, file = glue("{filepath}/{Region}_sitetype-{.y}{tag}.png"), remove_controls = c("zoomControl", "layersControl", "homeButton",  "drawToolbar", "easyButton", "control")), .progress = "Saving plots:")
      
    }
  }
  
  if (print) {
    return(ggs$plots)
  }
  
  
}


# KomList <- Kommuner %>% 
#   pull(KnNamn)
# 
# walk(KomList, purrr::possibly(~sebms_local_transect_map(2009:2023, Kommun = .x, maptype = "T")))

