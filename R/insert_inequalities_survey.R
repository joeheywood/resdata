#' Inserts data from surveys file maintained by Barry Fong
#'
#' @param log log file to save output to - defaults to 
#' 
#' @return
#' Logical - TRUE for worked ok.
#' @export
#' 
#' @import dplyr
#' @import glue
#' @import data.table
#' @import readxl
#' @import tidyxl
#' @import unpivotr
#'
#' @examples
#' run_updates()
#' 
#' 
#' 
#' 


insert_inequalities_survey <- function(log = "") {
    bf_fl <- "Q:/Teams/D&PA/Social Policy/COVID-19 data/Recovery Dashboard data/BF data for Resilience Dashboard March 2021.xlsx"
    
    #### Adult mental health by age ####
    tryCatch({
        xlsx_cells(bf_fl, "15") %>%
            behead("NNW", "ttl") %>%
            behead("NNW", "vrb") %>%
            behead("N", "yvllb") %>%
            behead("W", "xvardt") %>%
            filter(vrb == "Age group") %>%
            mutate(dataset = "amh_age", xwhich = 2, xvardt = as.Date(xvardt), 
                   yval = numeric, text = "", xvarchar = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    #### Adult mental health by sex ####
    tryCatch({
        xlsx_cells(bf_fl, "16") %>%
            behead("NNW", "ttl") %>%
            behead("NNW", "vrb") %>%
            behead("N", "yvllb") %>%
            behead("W", "xvardt") %>%
            filter(vrb == "Sex") %>%
            mutate(dataset = "amh_sx", xwhich = 2, xvardt = as.Date(xvardt), 
                   yval = numeric, text = "", xvarchar = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    
    #### Housing affordability by ethnicity ####
    tryCatch({
        xlsx_cells(bf_fl, "17") %>%
            filter(row > 3) %>%
            behead("N", "year") %>%
            behead("W", "eth") %>%
            mutate(dataset = "hsaffeth", xwhich = 1, xvarchar = year, 
                   xvardt = NA,  yval = numeric, yvllb = eth, text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    
    #### Overcrowding by tenure ####
    tryCatch({
        read_excel(bf_fl, "18", skip = 2) %>%
            select(year = 1, everything()) %>%
            filter(year > "2012/13") %>%
            pivot_longer(-year) %>%
            mutate(dataset = "ovrten", xwhich = 1, xvarchar = year, 
                   xvardt = NA,  yval = value, yvllb = name, text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    
    #### Overcrowding by ethnicity ####
    tryCatch({
        xlsx_cells(bf_fl, "19") %>%
            filter(row > 2) %>%
            behead("N", "year") %>%
            behead("W", "eth") %>%
            mutate(dataset = "ovreth", xwhich = 1, xvarchar = year, 
                   xvardt = NA,  yval = numeric, yvllb = eth, text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    
    #### Overcrowding by household type ####
    tryCatch({
        xlsx_cells(bf_fl, "20") %>%
            filter(row > 2) %>%
            behead("N", "year") %>%
            behead("W", "hhtype") %>%
            mutate(dataset = "ovrhh", xwhich = 1, xvarchar = year, 
                   xvardt = NA,  yval = numeric, yvllb = hhtype, text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    #### Homelessness by ethnicity ####
    tryCatch({
        xlsx_cells(bf_fl, "21") %>%
            filter(row > 3) %>%
            behead("N", "vrb") %>%
            behead("N", "year") %>%
            behead("W", "eth") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "hmleth", xwhich = 1, xvarchar = year, 
                   xvardt = NA,  yval = numeric, yvllb = eth, text = vrb) %>%
            select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    #### Homelessness by family type ####
    tryCatch({
        xlsx_cells(bf_fl, "22") %>%
            filter(row > 3) %>%
            behead("N", "vrb") %>%
            behead("N", "year") %>%
            behead("W", "fml") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "hmlfml", xwhich = 1, xvarchar = year, 
                   xvardt = NA,  yval = numeric, yvllb = fml, text = vrb) %>%
            select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    
    #### Internet users by age group ####
    tryCatch({
        xlsx_cells(bf_fl, "23") %>%
            filter(row > 3) %>%
            behead("N", "year") %>%
            behead("W", "age") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "intage", xwhich = 1, xvarchar = year, 
                   xvardt = NA,  yval = numeric, yvllb = age, text = "") %>%
            select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    
    #### Internet users by ethnicity ####
    tryCatch({
        xlsx_cells(bf_fl, "24") %>%
            filter(row > 3) %>%
            behead("N", "year") %>%
            behead("W", "eth") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "inteth", xwhich = 1, xvarchar = year, 
                   xvardt = NA,  yval = numeric, yvllb = eth, text = "") %>%
            select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    #### Internet users by disability ####
    tryCatch({
        xlsx_cells(bf_fl, "25") %>%
            filter(row > 3) %>%
            behead("N", "year") %>%
            behead("W", "disab") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "intdis", xwhich = 1, xvarchar = year, 
                   xvardt = NA,  yval = numeric, yvllb = disab, text = "") %>%
            select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    
    tryCatch({
        read_excel(bf_fl, "26") %>%
            select(yvllb = 1, `2019`, `2020`) %>%
            filter(yvllb %in% c("London", "UK")) %>%
            pivot_longer(-yvllb) %>%
            mutate(dataset = "dg_ess", xwhich = 1, xvarchar = name, 
                   xvardt = NA,  yval = value, text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    
    tryCatch({
        read_excel(bf_fl, "28") %>%
            select(yvllb = 1, `2019`, `2020`) %>%
            filter(yvllb %in% c("London", "UK")) %>%
            pivot_longer(-yvllb) %>%
            mutate(dataset = "dg_fs", xwhich = 1, xvarchar = name, 
                   xvardt = NA,  yval = value, text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    
    
    #### Access to private outdoor spaces by ethnicity ####
    tryCatch({
        xlsx_cells(bf_fl, "29") %>%
            filter(row > 2) %>%
            behead("N", "ethnic_group") %>%
            behead("W", "date") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "prvspc_eth", xwhich = 1, xvarchar = ethnic_group, 
                   xvardt = NA,  yval = numeric, yvllb = "", text = "") %>%
            select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) %>%
            insert_db()
    }, error = function(e){error_log(e, "Inequality surveys")})
    
    
    
    #### Access to private outdoor spaces by age ####
    tryCatch({
        xlsx_cells(bf_fl, "30") %>%
            filter(row > 2) %>%
            behead("N", "age_group") %>%
            behead("W", "date") %>%
            filter(!is.na(numeric)) %>%
            mutate(dataset = "prvspc_age", xwhich = 1, xvarchar = age_group, 
                   xvardt = NA,  yval = numeric, yvllb = "", text = "") %>%
            select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) %>%
            insert_db()
    }, error = function(e){error_log(e, "Inequality surveys")})
}
