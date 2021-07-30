#' Inserts data from file created by Alice Major on young people
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


insert_support_young_am <- function(log = "") {
    
    am_fl <- file.path("Q:/Teams/D&PA/Social Policy/COVID-19 data/",
                       "Recovery Dashboard data/AM Data",
                       "AM Dashboard latest indicators sources and instruction.xlsx")
    
    ### Time spent outdoors ###
    tryCatch({
        read_excel(am_fl, "time outside") %>%
            filter(Category == "Everyday") %>%
            select(-Category) %>%
            tidyr::pivot_longer(-Date, names_to = "yvllb", values_to = "yval") %>%
            mutate(text = Date) %>%
            tidyr::separate(Date, c("from", "todt"), sep = " - ") %>%
            mutate(todt = str_replace(todt, " \\(Q\\d\\)", "")) %>%
            mutate(dataset = "chtmout", xwhich = 2, xvarchar = "", 
                   xvardt = as.Date(paste0("1 ", todt), format = "%d %b %Y")) %>%
            insert_db()
        
    }, error = function(e){error_log(e, "Support Young - AM file")})
    
    #### Children's happiness####
    tryCatch({
        read_excel(am_fl, "GCH Trends") %>% 
            select(wave = 1, yval = 2) %>%
            mutate(xvarchar = str_replace(wave, "Wave \\d+ ", "")) %>%
            mutate(dataset = "ylstsf", xwhich = 1,  xvardt = NA,  
                   yvllb = "", text = "") %>% 
            insert_db()
    }, error = function(e){error_log(e, "Support Young - AM file")})
    
    #### Children's happiness####
    tryCatch({
        read_excel(am_fl, "GCH Index score") %>% 
            select(wave = 1, yval = 2) %>%
            mutate(xvarchar = str_replace(wave, "Wave \\d+ ", "")) %>%
            mutate(dataset = "ylstsf", xwhich = 1,  xvardt = NA,  
                   yvllb = "", text = "") %>% 
            insert_db()
    }, error = function(e){error_log(e, "Support Young - AM file")})
    
    
    #### Overcrowding ####  
    ## This should eventually be  on datastore
    tryCatch({
        read_excel(am_fl, "overcrowding") %>%
            mutate(dataset = "chovrcrwd", xwhich = 2, xvarchar = Year, 
                   yval = `All tenures`, yvllb = Area, xvardt = NA, text = "") %>%
            insert_db()
    }, error = function(e){error_log(e, "Support Young - AM file")})
    
    #### Physical Activity ####
    tryCatch({
        physpttn <- "((Week|Wave) \\d{1,2}):([0-9A-Z a-z]+)[ -]+(\\d+ \\w+ \\d+).*"
        
        read_excel(am_fl, "physical activity") %>%  
            filter(Indicator != "A typical weekday (outside of school hours)") %>%
            select(week = 1, time_spent = 2, Value) %>%
            mutate(week = str_replace_all(week, "(\\d+)(th|rd|st)", "\\1")) %>%
            mutate(weekno = str_replace(week, physpttn, "\\1"),
                   weekdate = as.Date(str_replace(week, physpttn, "\\4"), 
                                      format = "%d %B %Y")
            ) %>%
            mutate(dataset = "chphys", xwhich = 2, xvarchar = "", xvardt = weekdate,
                   yvllb = time_spent,  yval = Value,  text = "" ) %>%
            arrange(xvardt) %>%
            insert_db()
        
    }, error = function(e){error_log(e, "Support Young - AM file")})
    
}

