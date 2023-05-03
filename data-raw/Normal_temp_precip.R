## code to prepare `Normal_temp_precip` dataset goes here
# This is to create the internal normal temperature and precipitation
norm_temp <-   read_xlsx("data-raw/smhi/Normal-temp-1991-2020.xlsx", sheet = 2, skip = 3) %>%
    select(name = Station, id = Klimatnr, latitud = Latitud, longitud = Longitud, jan:dec) %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "temp") %>%
  mutate(monthnr = month(mdy(paste0(month,"01/22"))), period = "1") %>%
  filter(monthnr %in% 4:9)
norm_precip <-   read_xlsx("data-raw/smhi/Normal-nbd-1991-2020.xlsx", sheet = 2, skip = 3) %>%
    select(name = Station, id = Klimatnr, latitud = Latitud, longitud = Longitud, jan:dec) %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "nb") %>%
  mutate(monthnr = month(mdy(paste0(month,"01/22"))), period = "1") %>%
  filter(monthnr %in% 4:9)

usethis::use_data(norm_temp, norm_precip, internal = TRUE, overwrite = T)



