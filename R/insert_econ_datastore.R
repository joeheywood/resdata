#' Inserts economy datastore data
#'
#' @param log log file to save output to - defaults to 
#' 
#' @return
#' Logical - TRUE for worked ok.
#' @export
#' 
#' @import purrr
#' @import dplyr
#' @import glue
#' @import tidyxl
#' @import unpivotr
#' @import data.table
#' 
#'
#' @examples
#' run_updates()
#' 


insert_econ_datastore <- function(log = "") {
    
    dt_pth <- "E:/project_folders/apps/db/downloads/DataStore/"
    xfl <- "insert_econ_datastore"
    #### Mastercard ####
    tryCatch({
        mcd <- fread(file.path(dt_pth, 
                               "mastercard-retail-location-insights", 
                               "txn_london.csv")) %>%
            as.data.frame() %>%
            select(week_start, starts_with("txn_amt_")) %>%
            pivot_longer(-week_start) %>%
            mutate(name = str_replace(name, "txn_amt_", "")) %>%
            mutate(name = str_replace(name, "^wd_", "mcwdy_")) %>%
            mutate(name = str_replace(name, "^we_", "mcwkd_")) %>%
            separate(name, sep = "_", into = c("dataset", "yvllb")) %>%
            mutate(xwhich = 2, xvarchar = "",
                   yvllb = str_to_sentence(yvllb),
                   xvardt = as.Date(week_start, format = "%d/%m/%Y"), 
                   yval = value, text = "") %>%
            select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) 
        
        mcd %>%
            filter(dataset == "mcwdy") %>% 
            insert_db(log = log, excfl = xfl)
        
        mcd %>%
            filter(dataset == "mcwkd") %>% 
            insert_db(log = log, excfl = xfl)
        
    }, error = function(e){error_log(e, "Economy - Datastore")}  )
    
    
    #### adult education ####
    tryCatch({
        aefl <- file.path(dt_pth, "gla-adult-education-budget",
                          "GLA%20AEB%202019-2020%20August%20-%20July%20R14%20Data%20Tables%20London.xlsx" )
        
        xlsx_cells(aefl, "2.2 LAD, Volumes") %>%
            behead("NNW", "table") %>%
            behead("NNW", "section") %>%
            behead("N", "vrb") %>%
            behead("W", "LAD11NM") %>%
            filter(LAD11NM != "Total", vrb == "Learner Participation") %>% 
            mutate(dataset = "ae", xwhich = 1, xvarchar = LAD11NM, xvardt = NA,  
                   yval = numeric,  yvllb = "",  
                   text = str_pad(numeric, width = 12, side = "left", pad = "0")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Economy - Datastore")})
    
    
    #### AE - Maths/English ####
    tryCatch({
        xlsx_cells(aefl, "4.8 Eng and Maths Ent, Ach") %>%
            behead("NNW", "table") %>%
            behead("NNW", "section") %>%
            behead("N", "vrb") %>%
            behead("WNW", "subject") %>%
            behead("W", "level") %>%
            filter(level %in% c("Level 1", "Level 2"), 
                   vrb == "Achieved" ) %>%
            mutate(dataset = "aeme", xwhich = 1, xvarchar = level, xvardt = NA,  
                   yval = numeric,  yvllb = subject,  
                   text = as.character(level)) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Economy - Datastore")})
    
    #### AE - low wage ####
    tryCatch({
        xlsx_cells(aefl, "4.3 Low Wage by Level, Volumes") %>%
            behead("NNW", "table") %>%
            behead("NNW", "variable") %>%
            behead("N", "category") %>%
            behead("WNW", "wage_category") %>%
            behead("W", "level") %>%
            filter(category == "Learner Participation", level == "Total") %>%
            mutate(dataset = "aelw", xwhich = 1, xvarchar = wage_category, 
                   xvardt = NA,  yval = numeric,  yvllb = "",  
                   text = as.character(row_number())) %>%
            # select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) 
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Economy - Datastore")})
    
    
    #### Insecure work #### (
    ## need to change to xlsx ## 
    tryCatch({
        xlsx_cells(file.path(dt_pth, 
                             "insecure-employment", 
                             "insecure-employment.xlsx"), 2) %>%
            behead("NNW", "yvllb") %>%
            behead("NNW", "vrb") %>%
            behead("N", "valtype") %>%
            behead("W", "year") %>%
            filter(vrb == "Percent employed in insecure employment",
                   valtype == "%",
                   year > 2011) %>%
            mutate(dataset = "inswk", xwhich = 1, xvarchar = as.character(year),
                   xvardt = NA, yval = numeric, text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Economy - Datastore")})
    
    
    #### London Living Wage ####
    tryCatch({
        xlsx_cells(
            file.path(dt_pth, 
                      "earning-below-llw", 
                      "employees-earning-below-llw-ft-pt-sex.xlsx"), "London") %>% 
            dplyr::filter(row > 1) %>% 
            behead("NNW", "population") %>%
            behead("N", "valtypeb") %>%
            behead("N", "valtype") %>%
            behead("W", "year") %>%
            filter(population == "All employees", year > 2011, 
                   valtype == "Number") %>%
            mutate(dataset = "llw", xwhich = 1, xvarchar = as.character(year),
                   xvardt = NA, yvllb = "", text = "",
                   yval = numeric) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Economy - Datastore")})
    
}