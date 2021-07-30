#' Inserts other economy data
#'
#' @param log log file to save output to - defaults to output to console
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
#' @examples
#' run_updates()
#' 
#' 
#' 
#' 


insert_econ_other <- function(log = "") {
    
    #### TfL ####
    tryCatch({
        fread("Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/indicators_data/db/sub_updates/tfl_num_journeys2.csv") %>%      
            as.data.frame() %>%
            clean_names() %>%
            mutate(dataset = "tfl", xwhich = 2, xvarchar = "", 
                   xvardt = as.Date(date, format = "%d/%m/%Y %H:%M"),  
                   yval = journey_taps, yvllb = travel_mode,  text = "") %>%
            arrange(xvardt) %>%
            insert_db()
        
    }, error = function(e){error_log(e, "Economy - other")})
    
    #### FROM JENKINS AUTOMATIONS #### 
    
    #### Employment #### 
    tryCatch({
        fread("Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/indicators_data/db/updates/employment.csv") %>%
            as.data.frame() %>%
            mutate(xvardt = as.Date(xvardt, origin = "1970-1-1")) %>%
            arrange(xvardt) %>%
            insert_db()
        
    }, error = function(e){error_log(e, "Economy - other")})
    
    #### Unemployment #### 
    tryCatch({
        fread("Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/indicators_data/db/updates/unemployment.csv") %>%
            as.data.frame() %>%
            mutate(xvardt = as.Date(xvardt, origin = "1970-1-1")) %>%
            arrange(xvardt) %>%
            insert_db()
        
        
    }, error = function(e){error_log(e, "Economy - other")})
    
    #### Heathrow ####
    tryCatch({
        
        read_excel(file.path("Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/data/", 
                             "economic_damage", 
                             "Heathrow Traffic indicator spreadsheet.xlsx"),
                   "Indexes & graphs") %>%
            mutate(Month = as.Date(Month )) %>%
            select(Month, Passengers = 5, `Air transport movements` = 6, 
                   Cargo = 7) %>%
            filter(Cargo > -1) %>%
            pivot_longer(-Month) %>%
            mutate(dataset = "htrw", xwhich = 2, xvarchar = "", xvardt = Month,  
                   yval = value,  yvllb = name,  text = "") %>%
            insert_db()
        
        
    }, error = function(e){error_log(e, "Economy - other")})
    
    
    
    
    #### BICS #### This is going to be replaced soon.
    tryCatch({
        xlsx_cells(
            file.path("Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/data/economic_damage",  
                      "Business site closures time series.xlsx"), "Time series") %>% 
            filter(row > 5) %>%
            behead("N", "wave") %>%
            behead("N", "daterng") %>%
            behead("W", "industry") %>%
            separate(daterng, sep = "-", into = c("stdt", "endt")) %>%
            mutate(xvardt = as.Date(endt, format = "%d/%m/%y")) %>%
            filter(!is.na(numeric), 
                   industry == "All Industries") %>%
            mutate(dataset = "bics", xwhich = 2, xvarchar = "",
                   yval = numeric, yvllb = "", text = "") %>%
            insert_db()
        
    }, error = function(e){error_log(e, "Economy - other")})
    
}
    
    
    