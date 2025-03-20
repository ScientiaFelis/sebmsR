## code to prepare `Internal_sysdata` dataset goes here


## DISTRIBUTION MAP DATA
SE <- st_read("data-raw/Internal_data_rawfiles/hiresborder/high_res_Sverige_SWEREF99TM.shp") %>%
  select(NAME_ENGLI) %>%
  st_transform(4326)

SweLandGrid <- st_read("data-raw/Internal_data_rawfiles/grids_for_sebmsr/SweLandGrid.shp") %>%
  st_set_crs(3021)

## Counties, regions, municipalities etc.
#FIXME: try st_simplify() on the regions to make borders more straight.
Counties <- st_read("data-raw/Internal_data_rawfiles/hiresborder/highres_lan_SWEREF99TM.shp") %>%
  st_transform(4326) %>%
  select(ID_1, NAME_1, TYPE_1) %>%
  group_by(ID_1, NAME_1, TYPE_1) %>%
  nest() %>%
  ungroup() %>%
  mutate(simp = map(data, possibly(~st_simplify(.x, dTolerance = 5000)))) %>%
  unnest(simp) %>%
  select(-data) %>%
  st_as_sf()

Bioreg <- st_read("data-raw/Internal_data_rawfiles/hiresborder/biogeografiska_regioner_SWEREF99TM_clean.shp") %>%
  st_transform(4326)

Regions <- st_read("data-raw/Internal_data_rawfiles/hiresborder/highres_regioner_SWEREF99TM.shp") %>%
  st_transform(4326) %>%
  select(RegNr = Region, RegionName) %>%
  st_simplify(preserveTopology = F, dTolerance = 5000)

Kommuner <- st_read("data-raw/Internal_data_rawfiles/hiresborder/highres_kommuner_SWEREF99TM_clean.shp") %>%
  st_transform(4326) %>%
  select(ID_1, ID_2, NAME_2, TYPE_2) %>%
  group_by(ID_1, ID_2, NAME_2, TYPE_2) %>%
  nest() %>%
  ungroup() %>%
  mutate(simp = map(data, possibly(~st_simplify(.x, dTolerance = 5000)))) %>%
  unnest(simp) %>%
  select(-data) %>%
  st_as_sf()

Landskapen <- st_read("data-raw/Internal_data_rawfiles/hiresborder/biogeografiska_landskap_SWEREF99TM_clean.shp") %>%
  st_transform(4326) %>%
  group_by(reg_uid, reg_name, reg_code) %>%
  nest() %>%
  ungroup() %>%
  mutate(simp = map(data, possibly(~st_simplify(.x, dTolerance = 5000)))) %>%
  unnest(simp) %>%
  select(-data) %>%
  st_as_sf()

# Create the raw Centrum points
st_centroid(Regions) %>% # Make a Centrumpoint file for each type of locale, and manually fill in the zoom
  st_coordinates() %>%
  bind_cols(Regions %>% st_drop_geometry(),.) %>%
  write_csv2("data-raw/Internal_data_rawfiles/CentrumpointsReg.csv")

st_centroid(Counties) %>% # Make a Centrumpoint file for each type of locale, and manually fill in the zoom
  st_coordinates() %>%
  bind_cols(Counties %>% st_drop_geometry(),.) %>%
  write_csv2("data-raw/Internal_data_rawfiles/CentrumpointsLän.csv")

st_centroid(Landskapen) %>% # Make a Centrumpoint file for each type of locale, and manually fill in the zoom
  st_coordinates() %>%
  bind_cols(Landskapen %>% st_drop_geometry(),.) %>%
  write_csv2("data-raw/Internal_data_rawfiles/CentrumpointsLsk.csv")

st_centroid(Kommuner) %>% # Make a Centrumpoint file for each type of locale, and manually fill in the zoom
  st_coordinates() %>%
  bind_cols(Kommuner %>% st_drop_geometry(),.) %>%
  write_csv2("data-raw/Internal_data_rawfiles/Centrumpoints.csv")


## Read in the centerpoints after fixing the zoom level manually
centerPK <- readr::read_tsv("data-raw/Internal_data_rawfiles/Centrumpoints.csv", locale = readr::locale(decimal_mark = "."))

centerPL <- readr::read_tsv("data-raw/Internal_data_rawfiles/CentrumpointsLän.csv", locale = readr::locale(decimal_mark = "."))

centerPR <- readr::read_tsv("data-raw/Internal_data_rawfiles/CentrumpointsReg.csv", locale = readr::locale(decimal_mark = ".")) %>%
  select(RegNr = Region, RegionName, X,Y, zoom)

centerPLsk <- readr::read_tsv("data-raw/Internal_data_rawfiles/CentrumpointsLsk.csv", locale = readr::locale(decimal_mark = "."))


# Reading in the grids
sebms_hex_grid <- st_read("data-raw/Internal_data_rawfiles/grids_for_sebmsr/swe_index_hexagonal_50km.shp") %>%
  st_transform(4326)

sebms_10_grid <- st_read("data-raw/Internal_data_rawfiles/grids_for_sebmsr/swe_10km_ekorutor_sweref.shp") %>%
  st_transform(4326)

sebms_5_grid <- st_read("data-raw/Internal_data_rawfiles/grids_for_sebmsr/swe_5km_ekorutor_sweref.shp") %>%
  st_transform(4326)



# Making regID
regID2 <- dbGetQuery(sebms_pool, "SELECT * FROM reg_region") %>%
  mutate(reg_name = if_else(reg_group == "R", reg_code, reg_name),
         reg_name = str_to_lower(reg_name)) %>%
  select(reg_name, reg_uid)


## SUNHOUR DATA

# Day hours and day in month
DayHour <- list(day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),# All days in a month
                hour = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
)  # All hours of the day

Day <- list(day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")) # All days in a month


# Normal temp and precipitation
# This is to create the internal normal temperature and precipitation
norm_temp <- readxl::read_xlsx("data-raw/Internal_data_rawfiles/smhi/Normal-temp-1991-2020.xlsx", sheet = 2, skip = 3) %>%
  select(name = Station, id = Klimatnr, latitud = Latitud, longitud = Longitud, jan:dec) %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "temp") %>%
  mutate(monthnr = month(mdy(paste0(month,"01/22"))), period = "1") %>%
  filter(monthnr %in% 4:9)
norm_precip <- readxl:: read_xlsx("data-raw/Internal_data_rawfiles/smhi/Normal-nbd-1991-2020.xlsx", sheet = 2, skip = 3) %>%
  select(name = Station, id = Klimatnr, latitud = Latitud, longitud = Longitud, jan:dec) %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "nb") %>%
  mutate(monthnr = month(mdy(paste0(month,"01/22"))), period = "1") %>%
  filter(monthnr %in% 4:9)

meansunH <- sebms_sunmean_data()

meansunH_M <- sebms_sunmean_data(per_month = T)

## TRIM DATA
indicatorlist <- list(
  grassland = c(67, 19, 26, 117, 40, 50, 70, 8, 119, 55, 110, 101),
  agricultural = c(110, 17, 92, 29, 30, 28, 19, 70, 91, 40, 119, 26, 118, 93),
  forest = c(19, 71, 120, 46, 105, 109, 118, 38, 95, 115),
  common20 = c(118, 119, 38, 30, 92, 17, 91, 71, 117, 28, 29, 70, 105, 19, 90, 77, 46, 73, 89, 115)
)



## Saving the data objecs as internal data
use_data(Bioreg, centerPK, centerPL, centerPR, centerPLsk, Day, DayHour, indicatorlist, Counties, Regions, Kommuner, Landskapen, meansunH, meansunH_M, norm_precip, norm_temp, regID, regID2, SE, SweLandGrid, sebms_swe_grid, sebms_hex_grid, sebms_10_grid, sebms_5_grid, internal = T, overwrite = T, compress = "xz", version = 3)
