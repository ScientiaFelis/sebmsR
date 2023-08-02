#!/usr/bin/Rscript

library(plumber)
library(sebmsR)

router <- file.path(system.file("bin", "api.R", package = "sebmsR"))

if (!file.exists(router)) {
    stop(sprintf("file not found: '%s' ", router))
}

message("SeBMS service with Swagger docs starting at http://localhost:8000/__swagger__/")
message("Try these calls for example plot and data:")

message("firefox http://localhost:8000/sebms_precip_temp_2015_plot")
message("firefox http://localhost:8000/sebms_data_sites")
message("firefox http://localhost:8000/sebms_species_per_site_sitetype_plot")

plumber <- plumb(router)
plumber$registerHook("exit", function(){
  print("Bye bye from SeBMS plumber API!")
})

plumber$run(host = "0.0.0.0", port = 8000, swagger = TRUE)
