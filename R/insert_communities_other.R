#' Inserts other communities data (Crime - random health etc)
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
#' @import data.table
#'
#' @examples
#' run_updates()
#' 
#' 
#' 
#' 


insert_communities_other <- function(log = "") {
    
    dt_pth <- "E:/project_folders/apps/db/downloads/Other"
    
    ########################
    #### **CRIME DATA** ####
    ########################
    
    #### TNO ####
    tryCatch({
        fread(file.path(dt_pth, "support_communities", "tno_offs.csv")) %>%
            as.data.frame() %>%
            mutate(dataset = "tno", xwhich = 2, xvarchar = "", 
                   xvardt = as.Date(`Month-Year`, format = "%d/%m/%Y"),
                   yval = `Moving Sum of Offences`, yvllb = "", text = "") %>%
            filter(xvardt > as.Date("2018-12-31")) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Communities - other")})
    
    #### Hate Crime Incidents ####
    tryCatch({
        fread(file.path(dt_pth, "support_communities", "hc_rel_racist.csv")) %>%
            as.data.frame() %>%
            rename(mnth = 1) %>%
            mutate(xvardt = as.Date(paste0("1 ", mnth), format = "%d %b-%y"), 
                   dataset = "htcrm", xwhich = 2, xvarchar = "", 
                   yval = Count, yvllb = "", text = "") %>%
            filter(xvardt > as.Date("2018-12-31")) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Communities - other")})
    
    #### Domestic Abuse Incidents ####
    tryCatch({
        fread(file.path(dt_pth, "support_communities", "hc_dom_abuse.csv")) %>%
            as.data.frame() %>%
            rename(mnth = 1) %>%
            mutate(xvardt = as.Date(paste0("1 ", mnth), format = "%d %b-%y"), 
                   dataset = "dmab", xwhich = 2, xvarchar = "", 
                   yval = Count, yvllb = "", text = "") %>%
            filter(xvardt > as.Date("2018-12-31")) %>% 
            insert_db(log)
    }, error = function(e){error_log(e, "Communities - other")})
    
    
    #### ASB ####
    tryCatch({
        fread(file.path(dt_pth, "support_communities", "asb_offs.csv")) %>%
            as.data.frame() %>%
            mutate(dataset = "asb", xwhich = 2, xvarchar = "", 
                   xvardt = as.Date(`Month-Year`, format = "%d/%m/%Y"),
                   yval = `Moving Sum of Offences`, yvllb = "", text = "") %>%
            filter(xvardt > as.Date("2018-12-31")) %>% 
            insert_db(log)
    }, error = function(e){error_log(e, "Communities - other")})
    
    
    ################
    #### HEALTH ####
    ################
    dt_pth <- "Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/data/"
    
    #### GP Appointments ####
    tryCatch({
        gpfl <- file.path("Q:/Teams/D&PA/Demography/david_kingman_files/", 
                          "COVID_19_analysis/data/",
                          "gp_london_gp_appointments_by_mode.csv")
        fread(file.path(dt_pth, "support_communities",
                        "gp_london_gp_appointments_by_mode.csv")) %>%
            as.data.frame() %>%
            mutate(dataset = "gp", xwhich = 2, xvarchar = "", xvardt = appt_date,
                   yval = total_appointments, yvllb = mode,
                   text = format(appt_date, "%d %b %y")) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Communities - other")})
    
    #### Hospital Activity ####
    tryCatch({
        fread(file.path(dt_pth, "support_communities",  "mar_london_new.csv")) %>% 
            as.data.frame() %>%
            select(-fy, -yr) %>%
            pivot_longer(-dt) %>%
            mutate(dataset = "mar", xwhich = 2, xvarchar = "", 
                   xvardt = as.Date(dt, format = "%d/%m/%Y"), 
                   yval = value, yvllb = name,  text = "") %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Communities - other")})
    
    
    #### Homelessness ####
    tryCatch({
        fread(file.path("Q:/Teams/D&PA/Apps/COVID19 Recovery Dashboard/indicators_data/",
                        "db/updates/homelessness_relief.csv")) %>%
            as.data.frame() %>%
            insert_db()
    }, error = function(e){error_log(e, "Communities - other")})
    
    
    tryCatch({
        fread(file.path(dt_pth, "support_communities",
                        "nhs_two_week_cancer_target_data.csv")) %>%
            as.data.frame() %>%
            filter(metric == "two_week_target") %>%
            mutate(xvardt = as.Date(date, format = "%d/%m/%Y"), dataset = "cnc", 
                   xwhich = 2, xvarchar = "", text = "", yvllb = "", yval = value) %>%
            insert_db(log)
    }, error = function(e){error_log(e, "Communities - other")})
    
    
}



