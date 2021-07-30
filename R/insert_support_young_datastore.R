#' Inserts data for young people from Datatstore
#'
#' @param log log file to save output to - defaults to 
#' 
#' @return
#' Logical - TRUE for worked ok.
#' @export
#' 
#' @import dplyr
#' @import glue
#' @import readxl
#'
#' @examples
#' run_updates()
#' 


insert_support_young_datastore <- function(log = "") {
    dt_pth <- "E:/project_folders/apps/db/downloads/DataStore/"
    xfl <- "insert_support_young_datastore"
    tryCatch({
        fread(file.path(dt_pth, "gcse-results-by-borough",
                        "gcse-results.csv")) %>%
            filter(Code %in% c("E92000001", "E12000007"), 
                   Year > "2014/15", 
                   Sex == "All") %>%
            mutate(text = case_when(
                Year == "2019/20" ~ "dotted",
                TRUE ~ "solid"
            ),
            dataset = "att", xwhich = 1, xvarchar = Year, xvardt = NA, 
            yval = Attainment8, yvllb = Area) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Support Young - Datastore")})
    
}