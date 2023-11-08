context("sebmsR")

test_that("data retrieval works", {
  skip_on_travis()
  res <- sebms_species_per_year_filtered()
  expect_true(nrow(res) > 0)
})

# test_that("figure w precip and temp 2015 works", {
#   library(magick)
#   img <- sebms_precip_temp_2015_plot()
#   i <- image_info(img)
#   expect_equal(i$format, "PNG")
#   expect_equal(i$width, 3000)
# })

test_that("dual plots w specieslist counts works", {
  plots <- sebms_abundance_per_species_plot(print = T)
  expect_equal(nrow(plots$p1$data), 49)
  expect_equal(nrow(plots$p2$data), 49)
})

test_that("plot w specieslist histo works", {
  p <- sebms_species_abundance_plot(Art = c(4,118), print = T)
  expect_equal(length(p), 2)
})

test_that("plot w species per site works", {
  p <- sebms_species_per_sitetype_plot(print = T)
  expect_equal(nrow(p$data), 20)
})

test_that("plot to compare years work", {
  p <- sebms_abundance_year_compare_plot(print = T)
  expect_equal(nrow(p$data), 53)
})

# test_that("naturum climate data works", {
#   df <- sebms_naturum_climate()
#   expect_gt(nrow(df), 0)
# })

test_that("path to config file does not contain '/' on win", {
  
  if (rappdirs:::get_os() != "win") skip("Not on win OS, skipping.")
  
  cfgfile <- file.path(rappdirs::app_dir("sebms")$config(), "config.yml")
  cfgfile <- normalizePath(cfgfile)
  
  has_nonwinslash <- grepl(cfgfile, "/", fixed = TRUE)
  
  expect_true(!has_nonwinslash)
  
})
