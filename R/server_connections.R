
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
      dbname = config$sebms$database,
      host = config$sebms$server, 
      port = config$sebms$port,
      user = config$sebms$dbuser,
      password = config$sebms$dbpass), # some bug - uses present working dir instead of the password
    error = function(e) {
      message("Cannot connect to SeBMS db, are credentials invalid and/or do you need an ssh tunnel?")
      #e$message <- paste("Error connecting to SeBMS ", e, sep = " ")
      #warning(e)
      message("Config file used: ", cfgfile, ", timestamp: ", Sys.time())
      message("Using driver: ", config$sebms$driver) 
      message("Using database: ", config$sebms$database)
      message("Using server: ", config$sebms$server)
      message("Using port: ", config$sebms$port)
      message("Using dbuser: ", config$sebms$dbuser)
      
      message("Now proceeding without valid db connection...")
    })
  
  return(pool)
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
