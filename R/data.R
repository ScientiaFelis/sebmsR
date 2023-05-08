#' @importFrom RPostgres Postgres
#' @importFrom pool dbPool
#' @importFrom rstudioapi askForPassword
#' @importFrom config get
#' @importFrom rappdirs app_dir
sebms_connect <- function() {
  #  sebms_pool <<- sebms_connect()
  #t <- try(config <- config::get("sebms", file = cfgfile))
  #if (inherits(t, "try-error")) alternativeFunction()
  cfgfile <- file.path(rappdirs::app_dir("sebms")$config(), "config.yml")
  cfgfile <- normalizePath(cfgfile)

  tryCatch(config <- config::get(NULL, "sebms", file = cfgfile), 
    error = function(e) {
      if (!dir.exists(dirname(cfgfile))) 
        dir.create(dirname(cfgfile), recursive = TRUE)
      template <- 
        system.file("extdata", "config.yml", package = "sebmsR")
      if (template != "") 
        file.copy(template, cfgfile) 
      else 
        file.copy("extdata/config.yml", cfgfile, overwrite = TRUE)
    }, finally = {
      config <- config::get(NULL, "sebms", file = cfgfile)
    })
  
  pool <- NULL
  
  tryCatch(
    pool <- pool::dbPool(
      drv = config$sebms$driver, 
      dbname =config$sebms$database,
      host = config$sebms$server, 
      port = config$sebms$port,
      user = config$sebms$dbuser,
      password = config$sebms$dbpass), # some bug - uses present working dir instead of the password
  error = function(e) {
    message("Cannot connect to SeBMS db, are credentials invalid and/or do you need an ssh tunnel?")
    #e$message <- paste("Error connecting to SeBMS ", e, sep = " ")
    #warning(e)
    message("Config file used: ", cfgfile, ", timestamp: ", Sys.time())
    message("Using dbuser: ", config$sebms$dbuser, 
      " with more connection details in the config file")
    message("Now proceeding without valid db connection...")
  })
  
  return (pool)
}

#' Connection pool used for db connections
#' @noMd
#sebms_pool <<- sebms_connect()

sebms_assert_connection <- function(pool) {
  if (!missing(pool)) return(pool)
  if (is.null(sebms_pool)) {
    message("Attempting reconnect to db...")
    if (exists("sebms_pool")) {
      sebms_pool <- base::get("sebms_pool")
      rm(sebms_pool)
    }
    sebms_pool <- sebms_connect()
    if (is.null(sebms_pool))
      warning("No connection. Please check connection settings in config.yml...")
    else
      message("Connected!")
  }
}

#' Get user data
#' 
#' @import dplyr
#' @export
sebms_users <- function(my_username = NULL) {

  sebms_assert_connection()
    
  res <- 
    tbl(sebms_pool, "usr_user") %>%
    select_all(funs(gsub("usr_", "", .))) %>%
    select(-password)
  
  if (!missing(my_username) && !is.null(my_username))
    res <- res %>% filter(username == my_username)
 
  res %>% collect()

}

#' Update person data in database - setting new modified time and
#' database user 
#' 
#' @param my_usr_uid Internal usr_user id in SeBMS database; use param sebms_users("nrm_msk")$uid 
#' @param target_per_uid Internal per_person id in SeBMS database
#' @import dplyr
#' @importFrom pool poolWithTransaction
#' @importFrom DBI dbSendQuery dbClearResult
#' @export
sebms_per_update_modified <- function(my_usr_uid=1,target_per_uid=0) {

  sebms_assert_connection()
  
  s <- "UPDATE per_person 
    SET per_modifiedtime = CURRENT_TIMESTAMP, 
    per_usr_modifiedbyid = $1
    WHERE per_uid IN ($2);"
  
  poolWithTransaction(sebms_pool, function(conn) {
    res <- dbSendQuery(conn, s, params = list(my_usr_uid, target_per_uid))
    DBI::dbClearResult(res)
  })
  
}

#' Retrieve species list per year
#' 
#' @import tibble
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_per_year <- function() {
  
  q <- "
    SELECT
      spe.spe_uid AS id,
      spe.spe_semainname As name,
      SUM(obs.obs_count) AS count
    FROM obs_observation AS obs
    INNER JOIN vis_visit AS vis ON 
      obs.obs_vis_visitid = vis.vis_uid
    INNER JOIN spe_species AS spe ON 
      obs.obs_spe_speciesid = spe.spe_uid
    INNER JOIN seg_segment AS seg ON 
      obs.obs_seg_segmentid = seg.seg_uid
    INNER JOIN sit_site AS sit ON 
      seg.seg_sit_siteid = sit.sit_uid
    WHERE
    sit.sit_reg_countyid = (SELECT reg_uid FROM reg_region WHERE reg_code = '08' AND reg_group = 'C') AND
    date_trunc('YEAR', vis_begintime) =(DATE '2014-01-01')
    GROUP BY
      spe.spe_uid
    ORDER BY
    count DESC;"
  
  sebms_assert_connection()
  res <- DBI::dbGetQuery(sebms_pool, q)
  as_tibble(res)
}
 

#' Retrieve species list per year, filter for approved ones
#' 
#' @import tibble
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_per_year_filtered <- function() {
  
  q <- "SELECT
  spe.spe_uid AS id,
  spe.spe_semainname As name,
  SUM(obs.obs_count) AS count
FROM obs_observation AS obs
INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
INNER JOIN  spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid     
WHERE
sit.sit_reg_countyid = (SELECT reg_uid FROM reg_region WHERE reg_code = '08' AND reg_group = 'C')
AND date_trunc('YEAR', vis_begintime) =(DATE '2014-01-01')
AND spv.spv_istrim=TRUE      -- new
GROUP BY
  spe.spe_uid
ORDER BY
count DESC;"
  
  sebms_assert_connection()
  res <- dbGetQuery(sebms_pool, q)
  as_tibble(res)
  
} 

#' Count the number of species per year and site 
#' and filter for approved ones 
#' @import tibble
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_per_year_site_filtered <- function() {
  
  q <- "SELECT
  sit.sit_uid AS id,
  sit.sit_name AS site,
  sit.sit_type AS sitetype,
  COUNT(DISTINCT spe.spe_uid) as species
FROM obs_observation AS obs
INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
INNER JOIN  spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid     
WHERE
sit.sit_reg_countyid = (SELECT reg_uid FROM reg_region WHERE reg_code = '12' AND reg_group = 'C')
AND date_trunc('YEAR', vis_begintime) =(DATE '2014-01-01')
AND spv.spv_istrim=TRUE      -- new
GROUP BY
  sit.sit_uid
ORDER BY
species DESC;"

  sebms_assert_connection()
  res <- dbGetQuery(sebms_pool, q)
  as_tibble(res)
}


#' List the species per year & site, add total counts 
#' and filter for approved species 
#' @import tibble
#' @importFrom DBI dbGetQuery
#' @export
sebms_species_per_year_site_counts_filtered <- function() {

  q <- "SELECT
  sit.sit_uid AS id,
  sit.sit_name AS site,
  sit.sit_type AS sitetype,
  spe.spe_uid AS species,
  SUM(obs.obs_count) as speciesno
FROM obs_observation AS obs
INNER JOIN vis_visit AS vis ON obs.obs_vis_visitid = vis.vis_uid
INNER JOIN spe_species AS spe ON obs.obs_spe_speciesid = spe.spe_uid
INNER JOIN seg_segment AS seg ON obs.obs_seg_segmentid = seg.seg_uid
INNER JOIN sit_site AS sit ON seg.seg_sit_siteid = sit.sit_uid
INNER JOIN  spv_speciesvalidation AS spv ON spe.spe_uid = spv_spe_speciesid     
WHERE
sit.sit_reg_countyid = (SELECT reg_uid FROM reg_region WHERE reg_code = '12' AND reg_group = 'C')
AND date_trunc('YEAR', vis_begintime) =(DATE '2014-01-01')
AND spv.spv_istrim=TRUE      -- new
GROUP BY
  sit.sit_uid, spe.spe_uid
ORDER BY
id , speciesno DESC;"
  
  sebms_assert_connection()
  res <- dbGetQuery(sebms_pool, q)
  as_tibble(res)
}

#' Climate data for Naturum sites from SMHI
#' @return data frame with climate data
#' @import purrr
#' @import dplyr
#' @import tidyr 
#' @import tibble 
#' @importFrom lubridate ymd_hms
#' @importFrom jsonlite fromJSON
#' @export
#' 
sebms_naturum_climate <- function() {
  
  #api <- function(x, y) paste0("https://opendata-download-metfcst.smhi.se/api/", 
  #  "category/pmp3g/version/2/geotype/point/lon/", x ,"/lat/", y,"/data.json")
  
  api <- function(x, y) paste0("https://opendata-download-metfcst.smhi.se/api/", 
    "category/pmp2g/version/2/geotype/point/lon/", x ,"/lat/", y,"/data.json")
  
  res <- 
    sebms_data_sites_naturum %>% 
    mutate(api = api(Long, Lat))
  
  smhi_call <- function(api) {
    
    res <- fromJSON(api)$timeSeries  
    names(res$parameters) <- res$validTime
    df <- bind_rows(res$parameters, .id = "id")
    df$values <- unlist(df$values)
    df <- as_tibble(df)
    
    res <- 
      df %>% 
      mutate(date = ymd_hms(id)) %>%
      dplyr::select(date, name, values) %>%
      spread(name, values) %>%
      mutate(is_gogogo = t >= 13 & ws < 7.9 & tcc_mean <= 4 & pmean == 0)
    
    return (res)
  }
  
  naturum_climate <- map(res$api, smhi_call)
  names(naturum_climate) <- res$Namn
  res <- bind_rows(naturum_climate, .id = "id")
  return (res)
}

##' @importFrom utils globalVariables
#if (getRversion() >= "2.15.1")
#  globalVariables(names = unlist(strsplit(split = " ",
# paste0("word1 ",
#  "word2"))))