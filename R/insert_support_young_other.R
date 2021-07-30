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
#' @import data.table
#' @import unpivotr
#' @import tidyxl
#' 
#'
#' @examples
#' run_updates()
#' 


insert_support_young_other <- function(log = "") {
    dt_pth <- "Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/data"
    
    # dpth <- dirname(Sys.getenv("DASH_DATAPATH"))
    xfl <- "support_young_other"
    
    
    ###########################
    #### *** CRIME *** ####
    ###########################
    
    #### LAS assaults ####
    tryCatch({
        read_excel(file.path(dt_pth, 
                             "supporting_young",
                             "LAS - Incidents by Month 10-24 years.xlsx"), 
                   "Raw Data") %>%
            mutate(xvardt = as.Date(glue("1 {Month} {`Calendar Year`}"), format = "%d %B %Y")) %>%
            group_by(xvardt) %>%
            summarise(yval = sum(Count)) %>% ungroup() %>%
            mutate(dataset = "las_ass", xwhich = 2, xvarchar = "", 
                   text = "", yvllb = "" ) %>% 
            insert_db(log, excfl = xfl)
        
    }, error = function(e){error_log(e, "Support Young - other")})
    
    #### cpps - neglect trend ####
    ## need to figure out how to automate this
    tryCatch({
        fread(file.path(dt_pth, "supporting_young", "cpps_neglect_trend.csv")) %>%  
            as.data.frame() %>%
            clean_names() %>%
            mutate(dataset = "cpps", xwhich = 1, xvardt = NA, yvllb = "", 
                   yval = number_of_plans, text = "",
                   xvarchar = str_replace(fy, "(\\d{4})(\\d{2})", "\\1/\\2")) %>%
            insert_db(log, excfl = xfl)
        
    }, error = function(e){error_log(e, "Support Young - other")})
    
    
    
    #### Children in poverty ####
    tryCatch({
        pvty <- xlsx_cells(file.path(dt_pth, "support_communities", "poverty-summary-data.xlsx"), 
                           sheet = "Poverty") %>%
            filter(is_blank == FALSE) %>%
            behead("NNW", "overall_theme") %>%
            behead("NNW", "source") %>%
            # behead("NNW", "type_data") %>%
            behead("NNW", "population") %>%
            behead("NNW", "aft_bef_hsg") %>%
            behead("NNW", "area") %>%
            behead("N", "numtype") %>%
            behead("W", "time") %>%
            select(overall_theme, source, population, aft_bef_hsg, area, numtype, time, val = numeric)
        
        pvty %>% 
            filter(aft_bef_hsg == "AHC",
                   population == "Children",
                   numtype == "%") %>%
            mutate(c19 = ifelse(grepl("^9", time), "19", "20")) %>%
            mutate(time = paste0(c19, time)) %>%
            filter(time > "2010/11-12/13") %>%
            mutate(dataset = "chpv", xwhich = 1, xvarchar = time,
                   xvardt = NA, yvllb = area,  yval = val, 
                   text = "" ) %>%
            insert_db(log, excfl = xfl)
        
        
        
    }, error = function(e){error_log(e, "Support Young - other")})
    
    
    #### Youth unemployment ####
    tryCatch({
        yfl <- "Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/indicators_data/db/updates/youth_unemployment.csv"
        fread(yfl) %>%
            as.data.frame() %>%
            mutate(xvardt = as.Date(xvardt, origin = "1970-1-1")) %>%
            insert_db(log, excfl = xfl)
    
    }, error = function(e){error_log(e, "Support Young - other")})
    
    
}