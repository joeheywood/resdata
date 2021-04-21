
#' Get database connection
#'
#'
#' @return #' A DBI dataconnect object
#' @export
#'
#' @examples
#' conn <- rdb_connect()

rdb_connect <- function() {
    configdir <- Sys.getenv("RDCONFIG")
    cnfg <- yaml::yaml.load_file(file.path(configdir, "rconfig.yaml"))
    conSuper <- dbConnect( DBI::dbDriver("Postgres"),
                           dbname = cnfg$dbname,
                           host = cnfg$host,
                           port = cnfg$port,
                           password = cnfg$password,
                           user = cnfg$user )
    
    conSuper
    
}