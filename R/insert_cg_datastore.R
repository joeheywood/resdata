#' Insert datastore pages from the cleaner, greener outcome
#'
#' @param log log file to save output to - defaults to 
#'
#' @return
#' Logical - TRUE for worked ok.
#' @export
#' 
#' @import readxl
#' @import tidyxl
#' @import unpivotr
#' @import data.table
#' @import tidyr
#'
#' @examples
#' insert_cg_AQ
#' 

insert_cg_datastore <- function(log = "") {
    dt_pth <- "E:/project_folders/apps/db/downloads/DataStore/"
    #### Recycling ####
    tryCatch({
        read_excel(file.path(dt_pth, "household-waste-recycling-rates-borough", 
                             "household-recycling-borough.xlsx"), 2) %>%
            filter(Area %in% c("London", "England")) %>%
            select(yvllb = Area, `2011/12`:`2018/19`) %>%
            pivot_longer(-yvllb, names_to = "xvarchar", values_to = "yval") %>%
            mutate(dataset = "rcyc", xwhich = 1, xvardt = NA, text = "") %>% 
            insert_db()
    }, error = function(e){error_log(e, "Cleaner, greener datastore pages")})
    
    
    
    #### Emissions ####
    tryCatch({
        emms <- xlsx_cells(file.path(dt_pth, 
                                     "leggi", 
                                     "LEGGI_2018_FINAL.xlsx"), "00 Summary") %>%
            dplyr::filter(row %in% 15:74) %>%
            # behead("NNE", "general_title") %>%
            behead("NNE", "measure") %>%
            behead("N", "year") %>%
            behead("NNE", "measure_desc") %>%
            behead("W", "subset") %>%
            select(year, measure, subset,  val = numeric, measure_desc) %>%
            filter(subset == "TOTAL", !is.na(val), year > 2011) %>%
            mutate(dataset = "emms", xwhich = 1, xvarchar = as.character(year), 
                   xvardt = NA, yval = val, yvllb = "", text = "") %>% 
            insert_db(log)
    }, error = function(e){error_log(e, "Cleaner, greener datastore pages")})
    
    
    #### Energy Consumption ####
    tryCatch({
        fread(file.path(dt_pth, "leggi", "energy-consumption-borough-leggi.csv")) %>%
            as.data.frame() %>%
            filter(LEGGI_Year > 2011,
                   Sector == "Total",
                   Fuel == "Total",
                   Area == "London") %>%
            mutate(dataset = "energy", xwhich = 1, xvarchar = LEGGI_Year, xvardt = NA,
                   yval = kWh, text = "", yvllb = "") %>%
            insert_db()
    }, error = function(e){error_log(e, "Cleaner, greener datastore pages")})
    
    
    
}