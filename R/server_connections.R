#' Open the .Renviron File in Editor
#' 
#' This makes it possible to set the credential variables in .Renviron file.
#' It can be in the user home directory or possibly in the R project working directory.
#'
#' @param homepath the path to the home directory
#' 
#' @importFrom utils file.edit
#' @importFrom stringr str_detect
#' @return open the .Renviron file to add PostgreSQL credentials
#' @export
#'
editcred <- function(homepath = "~/") {
  message("\n\nThese SQL database credentials should be present in .Renviron file\n")
  cat("DBUSER = 'username'\n")
  cat("DBPASS = 'passw'\n")
  cat("DBNAME = 'database name'\n")
  cat("DBPORT = 'database port'")
  
  Renv.file <- paste0(homepath, ".Renviron")
  
  if (file.exists(Renv.file)) {
    
    creexist <- readLines("~/.Renviron") %>% paste0(collapse = "\n") %>% str_detect("DBUSER")  
    
    if (creexist) {
      file.edit(Renv.file)
      
    }else {
      write("\n\nDBUSER = 'username'\nDBPASS = 'passw'\nDBNAME = 'database name'\nDBPORT = 'database port'", file = Renv.file, append = TRUE)
      file.edit(Renv.file)
    }
    
  }else {
    cat("THERE IS NO .Renviron FILE IN THE GIVEN DIRECTORY\n")
    
    answ <- readline(prompt = "Do you want to create a .Renviron file? ")
    
    if (answ %in% c("Y", "y", "Yes", "yes")) {
      
      write("\n\nDBUSER = 'username'\nDBPASS = 'passw'\nDBNAME = 'database name'\nDBPORT = 'database port'", file = Renv.file, append = TRUE)
      
      file.edit(Renv.file)
      
    }else {
      return()
    }
  }
  
  
  
}

#' Create a Connection pool from Config File
#' 
#' 
#' @importFrom RPostgres Postgres
#' @importFrom pool dbPool
#' @importFrom rstudioapi askForPassword
#' @importFrom config get
#' @importFrom rappdirs app_dir
#' 
#' @return return a pool for a [dbPool()] connection
#' 
#' @noRd
sebms_connect <- function() {
  
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
      message("Using database: ", config$sebms$database)
      message("Using server: ", config$sebms$server)
      message("Using port: ", config$sebms$port)
      message("Using dbuser: ", config$sebms$dbuser)
      message("Using driver: ", config$sebms$driver)
      
      message("Now proceeding without valid db connection...")
    })
  
  return(pool)
}

#' Connection pool Used for DB Connections
#' 
#' @param pool the connection pool
#' @param quiet logical; if FALSE (default) it will suppress error messages
#' @noMd
sebms_assert_connection <- function(pool, quiet = FALSE) {
  if (!missing(pool)) return(pool)
  if (is.null(sebms_pool)) {
    if(!quiet) {message("Attempting reconnect to db...")}
    if (exists("sebms_pool")) {
      sebms_pool <- base::get("sebms_pool")
      rm(sebms_pool)
    }
    sebms_pool <- sebms_connect()
    if (is.null(sebms_pool))
      warning("No connection. Please check connection settings in config.yml...")
    else
      if(!quiet){ message("Connected!")}
  }
  return(sebms_pool)
}

#' Get user data
#' 
#' @param my_username the user name
#' @import dplyr
#' @export
sebms_users <- function(my_username = NULL) {
  
  sebms_pool <- sebms_assert_connection()
  
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
  
  sebms_pool <- sebms_assert_connection()
  
  s <- "UPDATE per_person 
    SET per_modifiedtime = CURRENT_TIMESTAMP, 
    per_usr_modifiedbyid = $1
    WHERE per_uid IN ($2);"
  
  poolWithTransaction(sebms_pool, function(conn) {
    res <- dbSendQuery(conn, s, params = list(my_usr_uid, target_per_uid))
    DBI::dbClearResult(res)
  })
  
}
