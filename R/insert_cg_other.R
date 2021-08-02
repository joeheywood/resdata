#' Inserts other cleaner greener data
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
#' @import tibble
#'
#' @examples
#' run_updates()
#' 
#' 

insert_cg_other = function(log = "") {
    dt_pth <- "Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/data"
    #### Good Growth ####
    ## This data is from a PDF. Easiest just to get the data like this
    tryCatch({
        gg_dat <- tribble(
            ~xvarchar, ~yval, 
            "2010/11", 24157.1,
            "2011/12", 25418.4,
            "2012/13", 26742.6,
            "2013/14", 28499.9,
            "2014/15", 30436.4,
            "2015/16", 32983.1,
            "2016/17", 36016.2,
            "2017/18", 39697.3
        ) %>% mutate(dataset = "gg_dat", xwhich = 1, xvardt = NA,  
                     yvllb = "", text = "") %>% 
            insert_db(log)
        
    }, error = function(e){error_log(e, "CG - other")})
    
    
    
    #### Good Growth employment ####
    ## This data is from a PDF. Easiest just to get the data like this
    tryCatch({
        tribble(
            ~xvarchar, ~yval, 
            "2010/11", 163840,
            "2011/12", 163672,
            "2012/13", 171222,
            "2013/14", 181538,
            "2014/15", 192416,
            "2015/16", 207049,
            "2016/17", 224659,
            "2017/18", 246073,
        ) %>%
            mutate(dataset = "ggemp", xwhich = 1, xvardt = NA, 
                   yvllb = "", text = "") %>% 
            insert_db(log)
    }, error = function(e){error_log(e, "CG - other")})
    
    
    #### EPC ####
    ## (from website?)
    
    tryCatch({
        pop <- read_excel(file.path(dt_pth, 
                                    "economic_damage", 
                                    "housing_led_2018_base.xlsx"), "Persons") %>%
            filter(str_detect(gss_code, "E090"),
                   age %in% 18:65) %>%
            select(borough) %>% unique()
        
        read_excel( file.path(dt_pth, "cleaner_greener", 
                              "LA1_-_Domestic_EPCs.xlsx"), "LA1", skip = 5) %>%
            filter(`Country/Local Authority` %in% pop$borough,
                   `Year/Quarter` > "2012") %>%
            mutate(AC = A + B + C) %>%
            select(quarter = `Year/Quarter`, AC, num = `Number of Lodgements`) %>%
            group_by(quarter) %>%
            summarise(num = sum(num), ac = sum(AC)) %>% ungroup() %>%
            mutate(yval = ac / num, dataset = "epc", xwhich = 1, xvardt = NA, 
                   xvarchar = quarter, yvllb = "", text = "" ) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "CG - other")})
    
    
    #### Walking/cycling ####
    ## from ?? file sent?
    ## Codes here are the 33 London Boroughs (E09...01-33), then London, England etc
    ldn_codes <- c(sprintf("E090000%02d", 1:33), 
                   "E12000008", "E12000006", "E92000001",
                   "E13000001", "E13000002", "E12000007")
    
    ## each year is on a different tab, so we loop through them to make a
    ## time series
    wtabs <- c("CW0301_1819", "CW0301_1718", "CW0301_1617", "CW0301_1516")
    wlk_cyc_fl <- file.path(dt_pth, "cleaner_greener", "cw0301.xlsx")
    
    tryCatch({
        wlkall <- map_df(wtabs, function(tab){
            ### bit of a maze to pick out the right data! tidyxl useful here.
            xlsx_cells(wlk_cyc_fl, tab) %>%
                filter(is_blank == FALSE) %>%
                behead("NNW", "dataset") %>%
                behead("NNW", "link") %>%
                behead("NNW", "table") %>%
                behead("NNW", "specific_measure") %>%
                behead("NNW", "info") %>%
                behead("N", "activity_frequency") %>%
                behead("N", "time") %>%
                behead("W", "LAD11CD") %>%
                behead("W", "areaName") %>%
                select(dataset, specific_measure, activity_frequency, time, LAD11CD, 
                       areaName, val = numeric) %>%
                filter(LAD11CD %in% ldn_codes)
        })
        
        
        wlkall %>% 
            mutate(areaName = gsub(" of England", "", areaName)) %>%
            filter(areaName %in% c("London", "ENGLAND"),
                   activity_frequency == "Three times per week") %>%
            mutate(dataset = "wlkcyc", xwhich = 1, xvarchar = time, 
                   xvardt = NA, yval = val, yvllb = areaName, text = "") %>% 
            insert_db(log)
        
    }, error = function(e){error_log(e, "CG - other")})
    
    #### Waste #### 
    tryCatch({
        xlsx_cells(file.path(dt_pth, "cleaner_greener", "la-collected-waste.xlsx"), 2) %>%
            filter(row < 22) %>%
            behead("WNW", "yvllb") %>%
            behead("NNE", "subsection") %>%
            behead("N", "xvarchar") %>%
            behead("W", "vrb") %>%
            filter(xvarchar > "2010/11",
                   !is.na(numeric),
                   vrb == "Total household waste") %>%
            ## calculate change since first value ##
            group_by(yvllb) %>%
            mutate(yval = numeric / dplyr::first(numeric)) %>%  ungroup() %>%
            mutate(dataset = "wst", xvardt = NA, xwhich = 1, text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "CG - other")})
    
    
    # #### Renewable energy #### n
    tryCatch({
        rnwb_fl <- file.path(dt_pth, "cleaner_greener",
                             "Renewable_electricity_by_LA2014_to_2019.xlsx")
        
        map_df(2019:2014, function(yr) {
            dat <- read_excel(rnwb_fl, glue("LA - Generation, {yr}"), skip = 2)
            
            dat %>% # count(Region)
                filter(Region %in% "London") %>%
                select(`Local Authority Name`, Total) %>%
                summarise(yval = sum(Total)) %>%
                mutate(xvarchar = yr)
        }) %>%
            mutate(dataset = "bio", xwhich = 1, xvardt = NA, yvllb = "", text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "CG - other")})
    
}