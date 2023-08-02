library(magick)
library(devtools)
library(readr)
library(dplyr)

message("Package logos")

logo <- 
  "inst/shiny-apps/www/svensk-dagfjarilsovervakning-logo.png" %>%
  image_read() %>%
  image_convert("png") %>%
  image_transparent(color = "white", fuzz = 20)

logo %>%
  image_write("inst/shiny-apps/www/svensk-dagfjarilsovervakning-logo.png")

logo_small <- 
  paste0("http://www.dagfjarilar.lu.se/sites/default/files/public/",
    "img/nassel_cropped_150.jpg") %>% 
  image_read() %>%
  image_convert("png") %>%
  image_resize("30x") %>% 
  image_transparent("white", fuzz = 20)

logo_small %>% image_write("inst/shiny-apps/www/logo.png")

message("Package temperature and precipitation 2015 data")

read_csv_sb <- function(x, ct = "cicici") {
  read_csv(x, col_types = ct, 
    locale = locale(encoding = "ISO-8859-1"))
}

sebms_data_precip_2015 <- bind_rows(
  read_csv_sb("data-raw/figures/ClimateRainfall-data/nb.avg.csv"),
  read_csv_sb("data-raw/figures/ClimateRainfall-data/nb.2015.csv")
)

use_data(sebms_data_precip_2015, overwrite = TRUE)

sebms_data_temp_2015 <- 
  bind_rows(
    read_csv_sb(ct = "cicicd", 
      "data-raw/figures/ClimateTemperature-data/temp.avg.csv"),
    read_csv_sb(ct = "cicicd", 
      "data-raw/figures/ClimateTemperature-data/temp.2015.csv")
  )

use_data(sebms_data_temp_2015, overwrite = TRUE)

message("Package species data")

sebms_data_specieslist_cum <- 
  read_delim(delim = "|", col_types = "ici",
    "data-raw/figures/CumulativeSpeciesList-data/total_list.csv"
  ) %>% 
  filter(!name %in% c("Nollobservation"))

use_data(sebms_data_specieslist_cum, overwrite = TRUE)

message("Package species histogram data")

sebms_data_species_histo <- 
  read_delim(delim = "|", col_types = "icicii",
    file = "data-raw/figures/HistogramSpecies-data/histogram_one_species.csv"
  )

use_data(sebms_data_species_histo, overwrite = TRUE)

message("Package species data per site and sitetype")

sebms_data_species_per_site_sitetype <- 
  read_delim(delim = "|", col_types = "icci",
    file = "data-raw/figures/HistogramSpeciesPerSite-data/species_list_per_site_and_sitetype.csv"
  )

use_data(sebms_data_species_per_site_sitetype, overwrite = TRUE)

# h2 <- 
#   sebms_data_species_per_site_sitetype %>%
#   group_by(id, sitetype) %>%
#   mutate(bin = ntile(species, n = 10))
# 
# library(classInt)
# 
# intervals <- function(x) classInt::classIntervals(
#   var = x, 
#   n = 10,
#   style = "fixed",
#   intervalClosure = "right",
#   fixedBreaks = seq(1, 50, by = 5)
# )
# 
# res <- 
#   sebms_data_species_per_site_sitetype %>%
#   filter(sitetype == "T") %>%
#   .$species %>% intervals() %>%
#   .$var

sebms_persons <- 
  read_delim(delim = "|", col_types = "icccccccii",
    file = "data-raw/figures/TablePeopleList-data/address_and_site_list.csv"
  ) %>% 
  select(-per_uid) %>%
  setNames(c("Efternamn", "Förnamn", "Adress", "Postnr", 
           "Postort", "Landskap", "Lokalnamn", "N", "E"))

library(DT)

#detach("package:raster")
#DT::datatable(sbm_persons)
#knitr::kable(sbm_persons)

message("Package site per year and province data")

library(tidyr)

sebms_data_sites_per_year_province <- 
  read_delim(delim = "|", col_types = "icci",
    file = "data-raw/figures/TableSitesPerProvince-data/sites_per_year_and_province.csv"
  ) %>% 
  spread(sitetype, sites)

use_data(sebms_data_sites_per_year_province, overwrite = TRUE)


message("Package site data for Naturum sites")

sebms_data_sites_naturum <- read_csv("data-raw/naturum.txt", 
  col_types = "cdd", locale = locale(encoding = "ISO-8859-1"))

use_data(sebms_data_sites_naturum, overwrite = TRUE)


message("Package sunhours data")

library(httr)
library(dplyr)
library(stringr)
library(lubridate)

sunhours_ymd <- function(y, m, d) {
  
  api <- paste0(
    "http://strang.smhi.se/extraction/getfield.php",
    "?par=119&y1=%i&m1=%i&d1=%i&lev=1")
  
  req <- GET(sprintf(api, y, m, d))
  if (req$status_code == 200) {
    res <- 
      content(req) %>% 
      str_trim %>% 
      str_split("\\s+") %>% 
      unlist %>% 
      as.double
  } else {
    warning("Call to ", api, " failed with ", req$status_code)
    res <- rep(NA, 65928)
  }

  return (res)
  
}

sunhours_ym <- function(y, m) {
  
  api <- paste0(
    "http://strang.smhi.se/extraction/getfield.php",
    "?par=119&y1=%i&m1=%i&lev=2")
  
  req <- GET(sprintf(api, y, m))
  if (req$status_code == 200) {
    res <- 
      content(req) %>% 
      str_trim %>% 
      str_split("\\s+") %>% 
      unlist %>% 
      as.double
  } else {
    warning("Call to ", api, " failed with ", req$status_code)
    res <- rep(NA, 65928)
  }

  return (res)
  
}


library(purrr)

p_sunhours_ymd <- possibly(
  otherwise = rep(NA, 65928), 
  sunhours_ymd)

p_sunhours_ym <- possibly(
  otherwise = rep(NA, 65928), 
  sunhours_ym)

library(readr)
library(dplyr)
la11 <- scan("data-raw/la11.dat")
lo11 <- scan("data-raw/lo11.dat")

#tibble(lat = la11, lon = lo11)

sunhours_monthly <- function(year_range = 2007:2016, month_range = 4:9) {
  api <- function(x, y) sprintf("p_sunhours_ym(%i, %i)", x, y)
  ys <- as.vector(outer(year_range, month_range, function(x, y) x))
  ms <- as.vector(outer(year_range, month_range, function(x, y) y))
  api_calls <- as.character(outer(year_range, month_range, api))
  pb <- progress_estimated(length(api_calls))
  progressively_api <- function(x, .pb = NULL) {
    if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) 
      .pb$tick()$print()
    eval(parse(text = x))
  }
  res <- map(api_calls, progressively_api, .pb = pb)
  names(res) <- paste0(ys, "-", ms)
  list(res = res, api = api_calls)
}



sunhours_daily <- function(year, month) {
  days <- 1:lubridate::days_in_month(month)
  api_calls <- sprintf("p_sunhours_ymd(%i, %i, %i)", year, month, days)
  pb <- progress_estimated(length(api_calls))
  progressively_api <- function(x, .pb = NULL) {
    if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) 
      .pb$tick()$print()
    eval(parse(text = x))
  }
  res <- map(api_calls, progressively_api, .pb = pb)
  names(res) <- ymd(paste0(year, "-", month, "-", days))
  res
}

april <- sunhours_daily(2016, 4)
may <- sunhours_daily(2016, 5)

idx <- which(map_lgl(april, function(x) length(x) == 0))
april[[idx]] <- rep(NA, 65928)
april_d_missing <- idx
#Filter(function(x) length(x) == 0, april)
idx <- which(map_lgl(may, function(x) length(x) == 0))
may_d_missing <- idx
may[[idx]] <- rep(NA, 65928)

april_11 <- 
  bind_cols(april) %>% 
  select(t1 = "2016-04-10", t3 = "2016-04-12") %>%
  mutate(t2 = pmap_dbl(., function(t1, t3, ...) mean(c(t1, t3)))) %>%
  select(`2016-04-11` = t2)

may_8 <- 
  bind_cols(may) %>% 
  select(t1 = "2016-05-07", t3 = "2016-05-09") %>%
  mutate(t2 = pmap_dbl(., function(t1, t3, ...) mean(c(t1, t3)))) %>%
  select(`2016-05-08` = t2)

may_df <- bind_rows(may)
may_df$`2016-05-08` <- may_8$`2016-05-08`

april_df <- bind_rows(april)
april_df$`2016-04-11` <- april_11$`2016-04-11`

months <- sunhours_monthly()

months_y <- str_extract(months[[2]], "\\d{4}")
months_m <- months$api %>% str_extract(" \\d{1}") %>% trimws()
names(months$res) <- paste0(months_y, "-", months_m)

# TODO continue here
#save(months, april_df, may_df, file = "temp-save")
#load("temp-save")

#map_df(months[[1]], length) %>% transpose() %>% unlist
april_sum <- 
  april_df %>% 
  mutate(sum_month = pmap_dbl(., sum)) %>%
  .$sum_month

may_sum <- 
  may_df %>% 
  mutate(sum_month = pmap_dbl(., sum)) %>%
  .$sum_month

months$res$`2016-4` <-  april_sum
months$res$`2016-5` <-  may_sum

months_all <- 
  bind_cols(months$res)

# monthz_mean <- 
#   monthz %>% 
#   select()
#   rowMeans()
# 
# monthz_sum <- 
#   monthz %>%
#   rowSums()
# 
# monthzz <- 
#   monthz %>% mutate(la11, lo11)
# 
# library((dplyr))
# library(purrr)
# library(tidyr)
# 
# bind_rows(april) %>% 
# #  bind_cols(la11, lo11) %>%
#   gather(date, sunhours) %>%
#   separate(date, c("year", "month", "day"), sep = "-") %>%
#   group_by(year, month, day) %>%
#   summarize(mean(sunhours)) %>%
#   filter(day == 11)
# 
# bind_rows(may) %>% 
#   gather(date, sunhours) %>%
#   separate(date, c("year", "month", "day"), sep = "-") %>%
#   group_by(year, month) %>%
#   summarize(mean(sunhours, na.rm = TRUE))
# 
# 
# 
# monthly_sums <- 
#   months_all %>%
#   gather(date, sunhours) %>%
#   separate(date, c("year", "month"), sep = "-") %>%
#   group_by(year, month) %>%
#   summarize(monthly_s = sum(sunhours)) %>%
#   spread(year, monthly_s)
# 
# 
#   months_all %>%
#   gather(date, sunhours) %>%
#   separate(date, c("year", "month"), sep = "-") %>%
#   group_by(month) %>%
#   transmute(monthly_s = sum(sunhours)) %>%
#   spread(month, monthly_s)
# 
# period_mean <- monthly_sums %>% select(-month) %>% rowMeans
# 
# bind_cols(
#   tibble(la11, lo11), 
#   months_all, 
#   period_mean = period_mean
# )

# form totals per months summing for all years
months_sum <- function(x)
  months_all %>% select(matches(paste0("\\d{4}-", x))) %>% rowSums()

ms <- map(as.character(4:9), months_sum)
names(ms) <- as.character(4:9)
ms <- as_tibble(ms)

# form totals per year (summing across all months in that year)
years_sum <- function(x)
  months_all %>% select(matches(x)) %>% rowSums()

ys <- map(as.character(2007:2016), years_sum)
names(ys) <- as.character(2007:2016)
ys <- as_tibble(ys)

period_means <- ys %>% select(-`2016`) %>% rowMeans()
diff_latest <- ys$`2016` - period_means

sunhours <- 
  tibble(la11, lo11, period_means, diff_latest) %>% bind_cols(ys)


# assuming data la11 and lo11 uses projection in SwedenForSunHours.geojson
# https://osgeo-org.atlassian.net/browse/GEOT-1710 says 
# urn:ogc:def:crs:OGC:1.3:CRS84 is epsg:4326 with lon, lat ordering

library(sp)
library(dplyr)
library(raster)
library(tibble)
library(leaflet)
library(rgdal)

smhi_sunhours_spdf <- sp::SpatialPointsDataFrame(
  coords = tibble(lo11, la11),
  data = as.data.frame(sunhours %>% dplyr::select(-c(la11, lo11))), 
  proj4string = sp::CRS("+init=epsg:4326")
)

spdf <- # leaflet uses epsg:3857 internally
  spTransform(smhi_sunhours_spdf, CRS("+init=epsg:3857"))

map_dir <- "data-raw/figures/MapDistribution-data/R_files_for_similar_map/"
crs_grid <- as.character(CRS("+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs"))

grid <- 
  readOGR(map_dir, "grid", p4s = crs_grid) %>% 
  spTransform(CRS("+init=epsg:3857"))

rs <- # use the swedish grid used elsewhere in SeBMS
  raster(extent(grid), nrows = 62 * 1.5, ncols = 28 * 1.5, 
    crs = projection(spdf))

#rs[] <- 1:ncell(rs)
rb <- rasterize(spdf, rs, fun = mean)

#plot(rb$diff_latest)
#plot(rb$X2016)

swe_borders <- 
  readOGR("data-raw/SwedenForSunHours.geojson", 
    p4s = as.character(crs("+init=epsg:4326")))

swe_borders <- spTransform(swe_borders, CRS("+init=epsg:3857"))

plot(swe_borders)
r <- mask(rb, swe_borders)

#extent(r) <- extent(swe_borders)

library(devtools)
message("Saving raster brick with sunhours data")
smhi_sunhours_rb <- r
use_data(smhi_sunhours_rb, overwrite = TRUE)

message("Saving Swedish borders with Leaflet projection")
sebms_swe_borders <- swe_borders
use_data(sebms_swe_borders, overwrite = TRUE)

library(rgdal)
library(devtools)

swe_grid <-
  readOGR("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "grid", 
    p4s = "+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs")

sebms_swe_grid <- swe_grid
message("Saving Swedish grid")
use_data(sebms_swe_grid, overwrite = TRUE)

message("Saving Swedish SeBMS sites 2010-2014")

alla <- readOGR("data-raw/figures/MapDistribution-data/R_files_for_similar_map/", "alla_2010-2014_sites", 
  p4s = "+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs")

sebms_data_sites <- as_tibble(as(alla, "SpatialPoints")@coords)
use_data(sebms_data_sites, overwrite = TRUE)

message("Saving Swedish TIFF raster")
# https://stackoverflow.com/questions/24495487/r-error-thrown-while-using-rgdal-and-raster-packages
tiff <- raster::raster(system.file("extdata", "MapSweden.tif", package = "sebmsR", mustWork = TRUE))
#tiff <- raster::raster(tif)
raster::crs(tiff) <-  sp::CRS("+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs")
sebms_data_swetiff <- tiff
#devtools::use_data(sebms_data_swetiff, overwrite = TRUE)

devtools::use_data

#  sebms_data_swetiff@file@name <- "C:\\Users\\IEUser\\Documents\\GitHub\\swedishbutterflies\\data-raw\\figures\\MapDistribution-data\\R_files_for_similar_map\\MapSweden.tif"
