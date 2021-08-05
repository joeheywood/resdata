#' Inserts economy labour market file data
#'
#' @param log log file to save output to - defaults to 
#' 
#' @return
#' Logical - TRUE for worked ok.
#' @export
#' 
#' @import dplyr
#' @import glue
#' @import tidyxl
#' @import unpivotr
#' @import readxl
#' @import janitor
#' 
#'
#' @examples
#' run_updates()
#' 


insert_econ_macro <- function(log = "") {
    
    xfl <- "insert_econ_macro"
    
    ## File that mike hope updates
    # mh_fl <- "Q:/Teams/Intelligence Unit - General/Recovery Dashboard/Previous versions/Covid-19 Recovery Dashboard - Macroeconomic indicators_20210616.xlsx"
    mh_fl <- "Q:/Teams/Intelligence Unit - General/Recovery Dashboard/Covid-19 Recovery Dashboard - Macroeconomic indicators.xlsx"
    # mh_fl <- "Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/indicators_data/db/sub_updates/Covid-19 Recovery Dashboard - Macroeconomic indicators.xlsx"
    
    
    
    tryCatch({
    #### INDICATORS FROM MACRO ECON FILE #### 
    ec_inc <- xlsx_cells(mh_fl, "1") %>%
        filter(is_blank == FALSE) %>%
        behead("NNW", "indicator") %>%
        behead("NNW", "area") %>%
        behead("N", "measure") %>%
        behead("WNW", "year") %>%
        behead("W", "quarter") %>%
        select(measure, year, quarter, val = numeric) %>%
        pivot_wider(names_from = measure, values_from = val) %>% 
        clean_names() %>%
        mutate(xvarchar = paste0(year, quarter), yval = annual_growth_rate,
               xwhich = 1, xvardt = NA, dataset = "gva", yvllb = "") %>%
        mutate(is_forecast = year == 2020 & quarter > "Q2") %>%
        mutate(text = case_when(
            is_forecast == FALSE ~ "solid",
            is_forecast == TRUE ~ "dotted"
        )) %>%
        insert_db(log, xfl)
    }, error = function(e){error_log(e, "Economy - Macro file")})
    #### PMI ####
    
    tryCatch({
    xlsx_cells(mh_fl, "2") %>%
        filter(is_blank == FALSE) %>%
        behead("NNW", "indicator") %>%
        behead("N", "area") %>%
        behead("W", "time") %>%
        mutate(time = as.Date(time)) %>%
        filter(time > as.Date("2019-12-31")) %>%
        mutate(dataset = "pmi", xwhich = 2, xvarchar = "", xvardt = time, 
               yval = numeric, yvllb = "", text = "") %>% 
        insert_db(log, xfl)
    }, error = function(e){error_log(e, "Economy - Macro file")})
    
    
    tryCatch({
    #### PMI New ####
    xlsx_cells(mh_fl, "3") %>%
        filter(is_blank == FALSE) %>%
        behead("NNW", "indicator") %>%
        behead("N", "area") %>%
        behead("W", "time") %>%
        mutate(time = as.Date(time)) %>%
        filter(time > as.Date("2019-12-31")) %>%
        mutate(dataset = "pmi_new", xwhich = 2, xvarchar = "", xvardt = time, 
               yval = numeric, yvllb = "", text = "") %>% 
        insert_db(log, xfl)
    }, error = function(e){error_log(e, "Economy - Macro file")})
    
    tryCatch({
    #### Consumer confidence ####
    xlsx_cells(mh_fl, "4") %>%
        filter(is_blank == FALSE) %>%
        behead("NNW", "indicator") %>%
        behead("N", "area") %>%
        behead("W", "time") %>%
        mutate(dataset = "cconf", xwhich = 2, xvarchar = "", 
               xvardt = as.Date(time), yval = numeric, yvllb = "", text = "") %>% 
        insert_db(log, xfl)
    }, error = function(e){error_log(e, "Economy - Macro file")})
    
    tryCatch({
    #### House prices ####
    xlsx_cells(mh_fl, "5") %>%
        filter(is_blank == FALSE) %>%
        behead("NNW", "indicator") %>%
        behead("NNW", "area") %>%
        behead("N", "numtype") %>%
        behead("W", "time") %>%
        filter(numtype == "Annual growth") %>%
        mutate(xvardt = as.Date(time)) %>%
        mutate(text = format(xvardt, "%b %Y")) %>%
        mutate(dataset = "hsprc", xwhich = 2, xvarchar = "", 
               yval = numeric, yvllb = "") %>% 
        insert_db(log, xfl)
    }, error = function(e){error_log(e, "Economy - Macro file")})
    
    tryCatch({
    #### House price expectations ####
    read_excel(mh_fl, "6", skip = 3) %>%
        filter(!is.na(London)) %>%
        mutate(xvardt = as.Date(as.numeric(Month), origin = "1899-12-30")) %>%
        mutate(text = format(xvardt, "%b %Y"),
               dataset = "hsprcex", xwhich = 2, xvarchar = "", yval = London,
               yvllb = "") %>%
        filter(xvardt > (max(xvardt) - (365* 1.48))) %>%
        insert_db(log, xfl)
    }, error = function(e){error_log(e, "Economy - Macro file")})
    
    
    
    
    tryCatch({
    #### International students ####
    xlsx_cells(mh_fl, "8") %>%
        behead("NNW", "variable") %>%
        behead("N","year") %>%
        filter(!is.na(numeric)) %>%
        mutate(dataset = "intst", xwhich = 1, xvarchar = as.character(year),
               xvardt = NA, yval = numeric, yvllb = "", text = "") %>%
        insert_db(log, xfl)
    }, error = function(e){error_log(e, "Economy - Macro file")})
    
    tryCatch({
    #### International visitors ####
    xlsx_cells(mh_fl, "7") %>%
        behead("NNW", "variable") %>%
        behead("N","year") %>%
        filter(!is.na(numeric), year > 2011) %>%
        mutate(dataset = "intvst", xwhich = 1, xvarchar = as.character(year),
               xvardt = NA, yval = numeric, yvllb = "", text = "") %>%
        select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) %>% 
        insert_db(log, xfl)
    }, error = function(e){error_log(e, "Economy - Macro file")})
    
}