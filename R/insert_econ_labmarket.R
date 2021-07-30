#' Inserts economy labour market file data
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
#' @import readxl
#' 
#'
#' @examples
#' run_updates()
#' 


insert_econ_labmarket <- function(log = "") {
    cr_fl <- "Q:/Teams/Intelligence Unit - General/Recovery Dashboard/Covid-19 Recovery Dashboard - labour market.xlsx"
    
    #### Payrolled employees ####
    tryCatch({
        read_excel(cr_fl, "Payrolled employees", skip = 6) %>%
            select(Month = 1, ldn_chng = 3) %>%
            filter(!is.na(ldn_chng)) %>%
            mutate(xvardt = as.Date(as.numeric(Month), origin = "1899-12-30"),
                   xvarchar = "", xwhich = 2, 
                   dataset = "payrempl", yval = ldn_chng, yvllb = "", text = "a")  %>%
            arrange(xvardt) %>%
            insert_db()
    }, error = function(e){error_log(e, "Economy - Labour market")})
    
    #### Job postings ####
    tryCatch({
        read_excel(cr_fl, "Job Postings Trend", skip = 6) %>%
            select(Month = 1, ldn_chng = 3) %>%
            filter(!is.na(ldn_chng)) %>%
            mutate(xvardt = as.Date(paste0("1 ", Month), format = "%d %b %Y"),
                   xvarchar = "", xwhich = 2, 
                   dataset = "pstngs", yval = ldn_chng, yvllb = "", text = "")  %>%
            insert_db()
    }, error = function(e){error_log(e, "Economy - Labour market")})
    
    #### Furlough (ts) ####
    tryCatch({
        read_excel(cr_fl, "CJRS - London", skip = 6) %>%
            select(xvardt = 1, yval = 3) %>%
            mutate(dataset = "frl", xwhich = 2, xvarchar = 1, 
                   xvardt = as.Date(xvardt), yvllb = "", text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Economy - Labour market")})
    
    #### Furlough (boro) ####
    tryCatch({
        read_excel(cr_fl, "CJRS - borough", skip = 9) %>%
            select(xvarchar = 1, yval = 3) %>%
            filter(!is.na(yval)) %>%
            mutate(dataset = "frlb", xwhich = 1,xvardt = NA,
                   yvllb = "", text = as.character(yval*-1)) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Economy - Labour market")})
    
    #### SEISS #### 
    tryCatch({
        read_excel(cr_fl, "SEISS - borough", skip = 9) %>%
            select(xvarchar = 1, yval = 3) %>%
            filter(!is.na(yval)) %>%
            mutate(dataset = "seiss", xwhich = 1,xvardt = NA,
                   yvllb = "", text = as.character(yval*-1)) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Economy - Labour market")})
    
    
    
    
    
}