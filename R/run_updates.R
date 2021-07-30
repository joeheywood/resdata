#' Runs all update files
#'
#'
#' @return
#' Logical - TRUE for worked ok.
#' @export
#' 
#' @import purrr
#' @import dplyr
#' @import glue
#'
#' @examples
#' run_updates()



## runs all the above files
run_updates <- function(log = "") {
    insert_cg_AQ(log)
    insert_cg_datastore(log)
    insert_cg_other(log)
    insert_communities_datastore(log)
    insert_communities_other(log)
    insert_communities_socpol(log)
    insert_econ_datastore(log)
    insert_econ_labmarket(log)
    insert_econ_macro(log)
    insert_econ_other(log)
    insert_inequalities_survey(log)
    insert_subregional(log)
    insert_support_young_am(log)
    insert_support_young_datastore(log)
    insert_support_young_other(log)
}