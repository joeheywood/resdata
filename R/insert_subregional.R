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
#' @import lubridate
#'
#' @examples
#' run_updates()
#' 
#' 
#' 
#' 


insert_subregional <- function(log = "") {
    
    dt_pth <- "Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/data"
    dt_pth <- "Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/data"
    fl <- "Q:/Teams/Intelligence Unit - General/Recovery Dashboard/Covid-19 Recovery Dashboard - labour market.xlsx"
    
    
    #### People on UC ####
    
    tryCatch({
        # need population data to get proportion of people on UC
        pop <- read_excel(file.path(dt_pth, 
                                    "economic_damage", 
                                    "housing_led_2018_base.xlsx"), "Persons") %>%
            filter(str_detect(gss_code, "E090"),
                   age %in% 18:65) %>%
            group_by(gss_code, borough) %>%
            summarise(tot20 = sum(`2020`), tot21 = sum(`2021`)) %>%
            mutate(LAD11NM = str_replace(borough, " and ", " & "))
        
        
        dat <- xlsx_cells(fl, "people on UC - borough" ) %>%
            filter(row > 7) %>%
            behead("N", "month") %>%
            behead("W", "blank") %>%
            behead("W", "LAD11NM") %>%
            filter(!is.na(numeric)) %>%
            mutate(x = as.Date(paste0("1 ", month), format = "%d %B %Y"),
                   LAD11NM = str_replace(LAD11NM, " and ", " & ")) %>%
            filter(x > as.Date("2018-12-31")) %>%
            left_join(pop %>% select(LAD11NM, tot20, tot21)) %>%
            mutate(tot = if_else(x <= as.Date("2020-12-31"), 
                                 tot20, tot21)) %>% 
            mutate(y = numeric/tot, dataset = "uc")  %>%
            filter(LAD11NM != "Total")
        
        
        ldat <- dat %>%
            group_by(x) %>%
            summarise(
                y = sum(numeric, na.rm = TRUE) / sum(tot, na.rm = TRUE)) %>%
            ungroup() %>%
            mutate(LAD11NM = "London", dataset = "uc") 
        
        rbind(
            dat %>% select(dataset, x, y, LAD11NM),
            ldat %>% select(dataset, x, y, LAD11NM)
        ) %>%
            insert_boro_db()
        
    }, error = function(e){error_log(e, "Subregional")})
    
    tryCatch({
        clmts <- fread("Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/indicators_data/db/updates/claimant_count.csv") %>%
            as.data.frame()
        
        clmts %>% 
            filter(LAD11NM != "City of London") %>%
            insert_boro_db()
        
    }, error = function(e){error_log(e, "Subregional")})
    
    
    # #### Crime ####
    # crm_dat <- read_excel(file.path(dt_pth, "support_communities", 
    #                                 "Crime Data-week 48-CV-19.xlsx")) %>%
    #     clean_names() %>% 
    #     select(week_end = 1, LAD11NM = ocu_name, y = tno_offs) %>%
    #     mutate(x = as.Date(week_end), dataset = "tno_b")   
    # 
    # 
    # l_crm <- crm_dat %>% 
    #     group_by(x) %>%
    #     summarise(y = sum(y)) %>% ungroup() %>%
    #     mutate(dataset  = "tno_b", LAD11NM = "London")
    # 
    # rbind(crm_dat %>% select(dataset, x, y, LAD11NM), 
    #       l_crm %>% select(dataset, x, y, LAD11NM)) %>%
    #     insert_boro_db()
    
    
    #### Mobility Data ####
    
    tryCatch({
        google_act_fl <- file.path("Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/", 
                                   "indicators_data/db/sub_updates/google.RDS") 
        
        
        dat <- readRDS(google_act_fl) %>% 
            select(date, activity, value, boro) %>%
            mutate(wk = ceiling_date(as.Date(date), "week")) %>%
            
            group_by(wk, boro) %>%
            pivot_wider(names_from = activity, values_from = value) %>%
            janitor::clean_names() %>%
            summarise(res_wk = mean(residential, 
                                    na.rm = TRUE),
                      transit_wk = mean(transit_stations,
                                        na.rm = TRUE)) %>%
            ungroup() %>%
            mutate(LAD11NM = str_replace(
                boro, " and ", " & ")) %>%
            mutate(LAD11NM = str_replace(
                LAD11NM, "((London|Royal) Borough|City) of ", "")) %>%
            mutate(LAD11NM = str_replace(LAD11NM, "London", "City") ) %>%
            mutate(LAD11NM = ifelse(nchar(LAD11NM) == 0, "London", LAD11NM)) %>%    
            arrange(wk)
        
        
        #### Transit ####
        dat %>% 
            mutate(dataset = "gtransit") %>%
            select(dataset,x = wk, y = transit_wk, LAD11NM) %>% #names()
            insert_boro_db()
        
        
        #### Homeworking ####
        dat %>% 
            mutate(dataset = "gres") %>%
            select(dataset, x = wk, y = res_wk, LAD11NM) %>%
            insert_boro_db()
        
    }, error = function(e){error_log(e, "Subregional")})
    
}
