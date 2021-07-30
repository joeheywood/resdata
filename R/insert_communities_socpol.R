#' Inserts other communities data (Crime - random health etc)
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
#'
#' @examples
#' run_updates()
#' 
#' 
#' 
#' 


insert_communities_socpol <- function(log = "") {
    bf_fl <- "Q:/Teams/D&PA/Social Policy/COVID-19 data/Recovery Dashboard data/BF data for Resilience Dashboard March 2021.xlsx"
    xfl <- "insert_communities_socpol"
    
    #### borrowing while earning less ####
    tryCatch({
        xlsx_cells(bf_fl, "1") %>%
            behead("N", "dt") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "brrw", xwhich = 2, xvarchar = "", xvardt = as.Date(dt), 
                   yval = numeric, yvllb = "",  text = format(as.Date(dt), "%b '%y")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    #### Rent ####
    tryCatch({
        shorter <- c("Whether not up to date with rent or mortgage" = "All",
                     "Own outright or with mortgage" = "Own/mortgage",
                     "Rented" = "Rented")
        
        xlsx_cells(bf_fl, "2") %>%
            behead("N", "dt") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            mutate(sub = shorter[svyqn]) %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "rnt", xwhich = 2, xvarchar = "", xvardt = as.Date(dt), 
                   yval = numeric, yvllb = sub,  text = format(as.Date(dt), "%b '%y")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    #### Behind with bills ####
    tryCatch({
        xlsx_cells(bf_fl, "3") %>%
            behead("N", "dt") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            filter(!is.na(numeric)) %>%
            mutate(dt = as.Date(dt)) %>%
            mutate(dataset = "blls", xwhich = 2, xvarchar = "", xvardt = as.Date(dt), 
                   yval = numeric, yvllb = "",  text = format(as.Date(dt), "%b '%y")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    #### Food Insecurity ####
    tryCatch({
        xlsx_cells(bf_fl, "4") %>%
            behead("N", "dt") %>%
            behead("W", "indicator") %>%
            # select(dt, indicator, value = numeric) %>%
            filter(!is.na(numeric)) %>%
            mutate(date = str_replace(dt, "^(.*)-(.*)$", "\\2") ) %>%
            mutate(date = as.Date(date, format = "%d %b %Y")) %>%
            mutate(dataset = "fdins", xwhich = 2, xvarchar = "", xvardt = date, 
                   yval = numeric, yvllb = "",  text = format(date, "%b '%y")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    
    #### Life Satisfaction ####
    tryCatch({
        xlsx_cells(bf_fl, "5") %>%
            behead("N", "dt") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            filter(!is.na(numeric)) %>%
            mutate(xvardt = as.Date(dt)) %>%
            # select(dt, indicator, value = numeric, svyqn) %>%
            mutate(dataset = "lfsat", xwhich = 2, xvarchar = "",  
                   yval = numeric, yvllb = "",  text = format(xvardt, "%b '%y")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    
    #### Anxiety ####
    tryCatch({
        xlsx_cells(bf_fl, "6") %>%
            behead("N", "dt") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            # select(dt, indicator, value = numeric, svyqn) %>%
            mutate(date = str_replace(dt, "^(.*)-(.*)$", "\\2 2020") ) %>%
            mutate(date = as.Date(date, format = "%d %B %Y")) %>%
            mutate(dataset = "anx", xwhich = 2, xvarchar = "", xvardt = date, 
                   yval = numeric, yvllb = "",  text = format(date, "%b '%y")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    #### Loneliness ####
    tryCatch({
        xlsx_cells(bf_fl, "7") %>%
            behead("N", "dt") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            filter(!is.na(numeric)) %>%
            mutate(date = as.Date(dt)) %>%
            mutate(dataset = "lnl", xwhich = 2, xvarchar = "", xvardt = date, 
                   yval = numeric, yvllb = "",  text = format(date, "%b '%y")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    #### Get Help if needed ####
    tryCatch({
        xlsx_cells(bf_fl, "8") %>%
            behead("N", "yr") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "hlp", xwhich = 1, xvarchar = yr, xvardt = NA, 
                   yval = numeric, yvllb = "",  text = "") %>%
            select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    #### Talk to neighbours ####
    tryCatch({
        xlsx_cells(bf_fl, "9") %>%
            behead("N", "yr") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "nghb", xwhich = 1, xvarchar = yr, xvardt = NA, 
                   yval = numeric, yvllb = "",  text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    #### Informal volunteering ####
    tryCatch({
        xlsx_cells(bf_fl, "10") %>%
            behead("N", "yr") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "infhlp", xwhich = 1, xvarchar = yr, xvardt = NA, 
                   yval = numeric, yvllb = "",  text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    #### Formal volunteering ####
    tryCatch({
        xlsx_cells(bf_fl, "11") %>%
            behead("N", "yr") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "fmlvol", xwhich = 1, xvarchar = yr, xvardt = NA, 
                   yval = numeric, yvllb = "",  text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    #### Cohesion ####
    tryCatch({
        xlsx_cells(bf_fl, "12") %>%
            behead("N", "dt") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            behead("W", "last_year") %>%
            filter(!is.na(numeric)) %>%
            # select(dt, indicator, value = numeric, svyqn, last_year) %>%
            mutate(date = as.Date(dt)) %>%
            mutate(dataset = "coh", xwhich = 2, xvarchar = "", xvardt = date, 
                   yval = numeric, yvllb = "",  text = format(date, "%b '%y")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    
    
    #### Physical Activity ####
    tryCatch({
        xlsx_cells(bf_fl, "13") %>%
            behead("N", "dt") %>%
            behead("W", "indicator") %>%
            behead("W", "svyqn") %>%
            filter(!is.na(numeric)) %>%
            mutate(date = str_replace(dt, "^(.*)-(.*)$", "\\2 2020") ) %>%
            mutate(date = as.Date(date, format = "%d %B %Y")) %>%
            mutate(dataset = "phys", xwhich = 2, xvarchar = "", xvardt = date, 
                   yval = numeric, yvllb = "",  text = format(date, "%d %b '%y")) %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    
    #### Prevalence ####
    tryCatch({
        xlsx_cells(bf_fl, "14") %>%
            behead("N", "xvarchar") %>%
            behead("W", "indicator") %>%
            behead("W", "yvllb") %>%
            mutate(dataset = "prvl", xwhich = 1, xvardt = NA, 
                   yval = numeric, text = "") %>%
            insert_db(log = log, excfl = xfl)
    }, error = function(e){error_log(e, "Communities - socpol")})
    
    
}