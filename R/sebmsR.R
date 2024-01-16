#' sebmsR - Make Figures for the SEBMS Project
#' 
#' This package contains functions to make figures used in e.g. the reports of the SEBMS project.
#' 
#' @name sebmsR
#' 
#' @section Included primary functions:
#' 
#' \tabular{l}{These are the primary functions to use in the package.}
#' 
#' 
#' @section Weather plots: 
#'
#' \tabular{ll}{
#'  \code{\link{sebms_weather_png}} is used to produce figures with precipitation
#'  and temperatures for given cities with weather stations in Sweden.
#'  The normal temperatures and precipitation for the last five years
#'  is also shown in figure. \cr
#'  ------------------------------------------------------------------------\cr
#'  }
#' @section Sun hour plots: 
#'  
#'  \tabular{ll}{
#'  \code{\link{sebms_sunhours_data}} download data on sun hours in Sweden
#'  in given year/s and month/s. \cr
#'  
#'  \code{\link{sebms_sunhour_plot}} produce a raster inmage of the 
#'  sun hours in Sweden the given year/s and month/s. It save the output from
#'  \code{sebms_sunhours_data} to a object in the Global environment with
#'  the name *spatsunlist_`Year`*. \cr
#'  
#'  \code{\link{sebms_sundiff_plot}} create a figure that shows the 
#'  difference in sun hours between a given year and the five year mean. \cr
#'  
#'  \code{\link{sebms_minmax_sunhour}} give the maximum and minimum
#'   sunhour per year and the city or village closest to that location. \cr
#'   
#'   -----------------------------------------------------------------------\cr
#'   }
#'   
#' @section Species figures: 
#'   
#'  \tabular{ll}{
#'  \code{\link{sebms_abundance_per_species_plot}} produce a bar plot with
#'  number of individuals per species a given year. \cr
#'  
#'  \code{\link{sebms_abundance_year_compare_plot}} produce a bar plot with 
#'  the number of found butterflies per week, compared between two years. \cr
#'  
#'  \code{\link{sebms_species_abundance_plot}} produce a bar plot with
#'  the number of individuals per week of a given species and year. \cr
#'  
#'  \code{\link{sebms_species_per_sitetype_plot}} produce a bar plot with
#'  the number of sites within a range of species richness found at the site.
#'  Also show the mean number of species per site in each site type. \cr
#'  
#'  ------------------------------------------------------------------------\cr
#'   }
#'
#' @section Data retrieval functions: 
#' 
#'  \tabular{ll}{
#'  
#'  \code{\link{sebms_species_site_count_filtered}} return a data frame with 
#'  species abundance per site, site type, and date. \cr
#'  
#'  \code{\link{sebms_species_count_filtered}} return a data frame with
#'  species abundance per date. \cr
#'  
#'  \code{\link{sebms_species_per_year_filtered}} return a data frame with
#'  abundance data per species and year. \cr
#'  
#'  ------------------------------------------------------------------------\cr
#'  }
#'  
#' 
#' @section Trim Index functions: 
#'  
#'  \tabular{ll}{
#'   
#'  \code{\link{get_trimInfile}} get the data frame for making the trim index. \cr
#'  
#'  \code{\link{get_nearby_trimIndex}} get indices for species through time. \cr
#'  
#'  \code{\link{get_trimPlots}} produce a figure with the trend and CI through
#'  given years. \cr
#'
#'  \code{\link{get_imputedList}} get a list of imputed indices. \cr
#'    
#'  \code{\link{get_trimComparedPlots}} produce a figure with the trends for 
#'  Sweden and a chosen region for given years. \cr
#'  
#'  \code{\link{get_IndicatorAnalysis}} get the indices for a group of 
#'  indicator species. Defaults to '20 most common', 'Grassland',
#'   'Forest', Agricultural'. It is possible to make own indices. \cr
#'  
#'  \code{\link{get_indicatorPlots}} produce a figure with the trend and CI 
#'  for the indicator species groups through given years. \cr
#'  
#'  ------------------------------------------------------------------------\cr
#'   }
#' 
#'  
#' @section Miscellaneous functions: 
#'  
#'  \tabular{ll}{
#'   
#'  \code{\link{get_nearby}} get the nearby places to given coordinates. \cr
#'  
#'  \code{\link{get_nearby_SunHour}} get the nearby places to sun hour stations
#'    with sun hour data fo each place. \cr
#'  
#'  \code{\link{sebms_ggsave}} save the figures produced as png.
#'  This is used within the plot functions. \cr
#'  
#'  ------------------------------------------------------------------------\cr
#'   }
#'   
#'   
#' 
#' @section ggplot2() Themes: 
#' 
#'  \tabular{ll}{
#' 
#'  \code{\link{theme_sebms}} ggplot2 theme used for figures in sebms project. \cr
#'  
#'  \code{\link{theme_sebms_species}} ggplot2 theme used specificly for
#'  the species abundance plots. \cr
#' 
#' }
#' 
#' @docType package
#' @keywords package

# needed for use of . in magrittr pipelines
utils::globalVariables(c("."))
NULL
