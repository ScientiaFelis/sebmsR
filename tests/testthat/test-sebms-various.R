context("sebmsR")

test_that("data retrieval works", {
  skip_on_travis()
  res <- sebms_species_per_year()
  expect_true(nrow(res) > 0)
})

test_that("figure w precip and temp 2015 works", {
  library(magick)
  img <- sebms_precip_temp_2015_plot()
  i <- image_info(img)
  expect_equal(i$format, "PNG")
  expect_equal(i$width, 3000)
})

test_that("dual plots w specieslist counts works", {
  plots <- sebms_specieslist_cum_plots()
  expect_equal(nrow(plots$p1$data), 48)
  expect_equal(nrow(plots$p2$data), 64)
})

test_that("plot w specieslist histo works", {
  p <- sebms_species_histo_plot()
  expect_equal(nrow(p$data), 7)
})

test_that("plot w specieslist histo works", {
  p <- sebms_species_per_site_sitetype_plot()
  expect_equal(nrow(p$data), 17)
})

test_that("naturum climate data works", {
  df <- sebms_naturum_climate()
  expect_gt(nrow(df), 0)
})

test_that("path to config file does not contain '/' on win", {
  
  if (rappdirs:::get_os() != "win") skip("Not on win OS, skipping.")
  
  cfgfile <- file.path(rappdirs::app_dir("sebms")$config(), "config.yml")
  cfgfile <- normalizePath(cfgfile)
  
  has_nonwinslash <- grepl(cfgfile, "/", fixed = TRUE)
  
  expect_true(!has_nonwinslash)
    
})
