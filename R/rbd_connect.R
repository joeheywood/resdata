
#' Get database connection
#'
#'
#' @return #' A DBI dataconnect object
#' @export
#' @import here
#' @import RPostgres
#'
#' @examples
#' conn <- rdb_connect()
#' 

rdb_connect <- function() {
    # configdir <- Sys.getenv("RDCONFIG")
    cnfg <- yaml::yaml.load_file(here("rconfig.yml"))
    conSuper <- dbConnect( RPostgres::Postgres(),
                           dbname = cnfg$dbname,
                           host = cnfg$host,
                           port = cnfg$port,
                           password = cnfg$password,
                           user = cnfg$user )
    
    conSuper
    
}