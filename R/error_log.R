#' Function to run on error when inserting data into the database
#'
#' @param dtst The dataset from which the error occurred
#'
#' @return
#' Logical - TRUE for worked ok.
#' @export
#' 
#' @import here
#'
#' @examples
#' dat <- data.frame()
#' insert_db(dat)
error_log <- function(e, dtst) {
    message("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
    cli_alert_danger(glue("ERROR in {dtst}"))
    message("CAll", e$call)
    message(e$message)
    message("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
}