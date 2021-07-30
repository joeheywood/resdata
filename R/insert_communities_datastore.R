#' Inserts communities datastore data
#'
#' @return
#' Logical - TRUE for worked ok.
#' @export
#' 
#' @import purrr
#' @import dplyr
#' @import glue
#' @import unpivotr
#' @import tidyxl
#' @import stringr
#'
#' @examples
#' run_updates()
#' 

insert_communities_datastore = function(log = "") {
    
    #### Rough Sleeping ####
    tryCatch({
        tribble(
            ~xvarchar, ~`New to streets`, ~`Total seen`, 
            "2021Q1", 1567, 3002, 
            "2020Q4", 1582, 3307, 
            "2020Q3", 1901, 3444,
            "2020Q2", 2680, 4227,
            "2020Q1", 1841, 3692,
            "2019Q4", 1729, 3637,
            "2019Q3", 2069, 3985,
            "2019Q2", 1513, 3172,
            "2019Q1", 1558, 3217,
            "2018Q4", 1551, 3289,
            "2018Q3", 1382, 3103,
            "2018Q2", 1087, 2595,
        ) %>%
            pivot_longer(-xvarchar) %>%
            mutate(dataset = "rghlsp", xwhich = 1, xvardt = NA, 
                   yval = value, yvllb = name, text = "") %>%
            insert_db()
    }, error = function(e){error_log(e, "Communities datastore")})
    
    tryCatch({
        # pvfl <- "../downloads/DataStore/hbai-poverty/poverty-summary-data.xls"
        # change_to_xlsx(pvfl)
        pvfl <- "E:/project_folders/apps/db/downloads/DataStore/hbai-poverty/poverty-summary-data.xlsx"
        # pv1 <- readxl::read_excel(pvfl,  "Relative poverty")
        pvty <- xlsx_cells(pvfl, sheet = "Relative poverty") %>%
            filter(is_blank == FALSE) %>%
            behead("NNW", "overall_theme") %>%
            behead("NNW", "source") %>%
            # behead("NNW", "type_data") %>%
            behead("NNW", "population") %>%
            behead("NNW", "aft_bef_hsg") %>%
            behead("NNW", "area") %>%
            behead("N", "numtype") %>%
            behead("W", "time") %>%
            select(overall_theme, source, population, aft_bef_hsg, area, numtype, 
                   time, val = numeric)
    }, error = function(e){error_log(e, "Communities datastore")})
    
    
    #### Relative Poverty ####
    tryCatch({
            pvty %>% 
                filter(aft_bef_hsg == "AHC",
                       population == "All people",
                       numtype == "%",
                       !str_detect(time, "^9"),
                       time > "09/10-11/12") %>%
                mutate(time = paste0("20", time)) %>%
                mutate(dataset = "pvty", xwhich = 1, xvarchar = time, xvardt = NA, 
                       yval = val, yvllb = area, text = "") %>%
                insert_db()
    }, error = function(e){error_log(e, "Communities datastore")})
    
    #### Absolute Poverty ####
    tryCatch({
            pvfl <- "E:/project_folders/apps/db/downloads/DataStore/hbai-poverty/poverty-summary-data.xlsx"
            abpvty <- xlsx_cells(pvfl, sheet = "Absolute poverty") %>%
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
            
            abpvty %>% 
                filter(aft_bef_hsg == "AHC",
                       population == "All people",
                       numtype == "%",
                       !str_detect(time, "^9"),
                       time > "09/10-11/12") %>%
                mutate(time = paste0("20", time)) %>%
                mutate(dataset = "abspv", xwhich = 1, xvarchar = time, xvardt = NA, 
                       yval = val, yvllb = area, text = "") %>%
                insert_db()
    }, error = function(e){error_log(e, "Communities datastore")})
}