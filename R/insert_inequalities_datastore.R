#' Inserts inequalities datastore
#'
#' @param log log file to save output to - defaults to output to console
#' 
#' @return
#' Logical - TRUE for worked ok.
#' @export
#' 
#' @import dplyr
#' @import glue
#' @import tidyxl
#' @import unpivotr
#' @import data.table
#'
#' @examples
#' run_updates()
#' 


insert_inequalities_datastore <- function(log = "") {
    dt_pth <- "Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/data"
    
    #### Employment Gaps #### 
    emp_gaps_fl <- "E:/project_folders/apps/db/downloads/DataStore/employment-gaps/employment-gaps.xlsx"
    
    tryCatch({
        xlsx_cells(emp_gaps_fl, "Female employment gap") %>%
            filter(is_blank == FALSE) %>%
            behead("NNW", "area") %>%
            behead("N", "yvllb") %>%
            behead("W", "xvarchar") %>%
            filter(xvarchar > 2011, 
                   area == "London",
                   str_detect(yvllb, "rate") == TRUE) %>%
            mutate(dataset = "empsx_gap", xwhich = 1, xvardt = NA,  yval = numeric,  
                   xvarchar = as.character(xvarchar), text = "") %>%
            insert_db(log)
        
    }, error = function(e){error_log(e, "Inequalities - datastore")})
    
    tryCatch({
        xlsx_cells(emp_gaps_fl, "Disabled employment gap") %>%
            filter(is_blank == FALSE) %>%
            behead("NNW", "area") %>%
            behead("N", "yvllb") %>%
            behead("W", "xvarchar") %>%
            filter(xvarchar > 2010, 
                   area == "London",
                   str_detect(yvllb, "rate( not)? disabled") == TRUE) %>%
            mutate(dataset = "empdis_gap", xwhich = 1, xvardt = NA,  yval = numeric,  
                   xvarchar = as.character(xvarchar), text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequalities - datastore")})
    
    tryCatch({
        xlsx_cells(emp_gaps_fl, "Ethnicity employment gap") %>%
            filter(is_blank == FALSE, row < 22) %>%
            behead("NNW", "pop") %>%
            behead("N", "yvllb") %>%
            behead("W", "xvarchar") %>%
            filter(xvarchar > 2011, 
                   str_detect(yvllb, "rate"),
                   pop == "All",
                   yvllb %in% c("Employment rate - white", 
                                "Employment rate - ethnic minority"),
                   !is.na(numeric)) %>%
            mutate(yvllb = str_replace(yvllb, "ethnic minority", "BAME")) %>%
            mutate(dataset = "empeth_gap", xwhich = 1, xvardt = NA,  yval = numeric,  
                   xvarchar = as.character(xvarchar), text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Inequalities - datastore")})
    
    
    #### Attainment - gender ####
    tryCatch({
        gcse_slug <- "E:/project_folders/apps/db/downloads/DataStore/gcse-results-by-borough/"
        fread(file.path(gcse_slug, "/gcse-results.csv")) %>%
            as.data.frame() %>%
            filter(Sex %in% c("Boys", "Girls"), Area == c("London")) %>%
            mutate(dataset = "att_sx", xwhich = 1, xvarchar = as.character(Year), 
                   xvardt = NA,  yval = Attainment8, yvllb = Sex, 
                   text = ifelse(Year == "2019/20", "dotted", "solid")) %>%
            # select(dataset, xwhich, xvarchar, xvardt, yval, yvllb, text) %>%
            insert_db()
        
    }, error = function(e){error_log(e, "Inequalities - datastore")})
    
    
    #### Attainment - ethnicity ####
    tryCatch({
        fread(file.path(gcse_slug, "gcse-results-ethnicity.csv")) %>%
            as.data.frame() %>%
            filter(Sex == "All", Area == "London", 
                   Ethnicity %in% c("Asian", "Black", "Chinese", "Mixed", "White")) %>%
            mutate(att8 = as.numeric(Attainment8)) %>%
            select(Ethnicity, Year, att8) %>%
            mutate(dataset = "att_eth", xwhich = 1, xvarchar = as.character(Year), 
                   xvardt = NA,  yval = att8, yvllb = Ethnicity, 
                   text = ifelse(Year == "2019/20", "dotted", "solid")) %>%
            insert_db()
    }, error = function(e){error_log(e, "Inequalities - datastore")})
    
    
    #### Attainment - FSM ####
    tryCatch({
        fread(file.path(dt_pth, "narrow_inequalities", "gcse-results-fsm.csv")) %>%
            as.data.frame() %>%
            filter(Sex == "All", Area == "London") %>%
            mutate(dataset = "att_fsm", xwhich = 1, xvarchar = as.character(Year), 
                   xvardt = NA,  yval = Attainment8, yvllb = FSM, 
                   text = ifelse(Year == "2019/20", "dotted", "solid")) %>%
            insert_db()
    }, error = function(e){error_log(e, "Inequalities - datastore")})
}