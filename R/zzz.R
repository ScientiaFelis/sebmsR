sebms_pool <- NULL

.onAttach <- function(lib, pkg){

  # echo "swedish" | toilet -f cricket
  # echo "butterflies" | toilet -f cricket
  
  welcome <-
"http://dagfjarilar.lu.se__ __         __    
.-----.--.--.--.-----.--|  |__|.-----.|  |--.
|__ --|  |  |  |  -__|  _  |  ||__ --||     |
|_____|________|_____|_____|__||_____||__|__|
 __           __   __               ___ __ __              
|  |--.--.--.|  |_|  |_.-----.----.'  _|  |__|.-----.-----.
|  _  |  |  ||   _|   _|  -__|   _|   _|  |  ||  -__|__ --|
|_____|_____||____|____|_____|__| |__| |__|__||_____|_____|
Use runShinyApp('poc') to run the Shiny proof-of-concept app"
#TODO: Fix other start-up message
  #packageStartupMessage(welcome)
  packageStartupMessage("\nFONT PROBLEMS?\n See READMEs 'Font Issues in Windows'", appendLF = T)
  

}

.onLoad <- function(libname, pkgname) {
  # NOTE: run tunnels.sh before establishing db connection
  # TODO set utf-8 on the postgres connection
  #conn <- poolCheckout(pool)
  #dbSendQuery(conn, "SET NAMES utf8;")
  # do something with conn
  #poolReturn(conn)  
  #}
     sebms_pool <<- sebmsR:::sebms_connect()
   if (is.null(sebms_pool)) {
     packageStartupMessage("Please enable db connections for full functionality")
     packageStartupMessage("edit settings with 'editcred()'")
   }
#   pool::poolReturn(pool::poolCheckout(sebms_pool))
}

.onDetach <- function(libpath) {
  if (base::exists("sebms_pool")) {
    message("Closing sebms connection pool")
    pool::poolClose(sebms_pool)
    #rm(list = c("sebms_pool"), inherits = TRUE)
    rm(sebms_pool)
  }
}

# .onUnload <- function(libpath) {
# 
#   message("Closing sebms connection pool")
#   pool::poolClose(sebms_pool)
#   
#   # kill_cons <- function () {
#   #  all_cons <- DBI::dbListConnections(pool)
#   #  message(all_cons)
#   #  purrr::walk(all_cons, dbDisconnect)
#   # }
# 
# }
