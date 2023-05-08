library(dplyr)
library(pool)
library(sebmsR)

message("loading global.R")
pool <- sebmsR:::sebms_connect()
sebmsR:::sebms_assert_connection(pool)
#onStop(function() {
#  poolClose(pool)
#})

enableBookmarking(store = "url")

