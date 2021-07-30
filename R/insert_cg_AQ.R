#' Insert Air Quality data into the database
#'
#' @param log log file to save output to - defaults to 
#'
#' @return
#' Logical - TRUE for worked ok.
#' @export
#' 
#' @import cli
#' @import data.table
#'
#' @examples
#' insert_cg_AQ
#' 

insert_cg_AQ <- function(log = "") {
    jnk <- "Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/indicators_data/db/updates"
    tryCatch({
        fread(file.path(jnk, "air_quality_no2_type_zone.csv")) %>%
            as.data.frame() %>%
            filter(!is.na(yval)) %>%
            mutate(xvardt = as.Date(xvardt, origin = "1970-1-1")) %>%
            insert_db(log, excfl = "insert_cg_AQ")
        
        fread(file.path(jnk, "air_quality_pm10_type_zone.csv")) %>%
            as.data.frame() %>%
            filter(!is.na(yval)) %>%
            mutate(xvardt = as.Date(xvardt, origin = "1970-1-1")) %>%
            insert_db(log, excfl = "insert_cg_AQ")
        
        fread(file.path(jnk, "air_quality_pm25_type_zone.csv")) %>%
            as.data.frame() %>%
            filter(!is.na(yval)) %>%
            mutate(xvardt = as.Date(xvardt, origin = "1970-1-1")) %>%
            insert_db(log, excfl = "insert_cg_AQ")
        
    }, error = function(e){error_log(e, "Air Quality")})
}