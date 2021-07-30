#' Refresh the metadata 
#'
#'
#' @return
#' Logical - TRUE for worked ok
#' @export
#'
#' @examples
#' refresh_metadata()
refresh_metadata <- function() {
    # flpth <- file.path(dt_pth, "Recovery Data for Dashboard.xlsx")
    # flpth <- Sys.getenv("DASH_METAPATH")
    # dpth <- Sys.getenv("DASH_DATAPATH")
    cnfg <- yaml::yaml.load_file(here("rconfig.yml"))
    
    mtd <- read_excel(cnfg$flpth, "Charts info") %>%
        select( 
            dataset, markers, bar, stack, borolevel, linewidth, hvr,
            dash, tickformat, highlight, tabname = `Tab name`, 
            outcome = Outcome, subcat = Page, 
            charttitle, yaxistitle, xaxistitle, source, link   
            
        ) %>%
        mutate(circles = FALSE, ylabs = "", sub = "")
    
    # conn <- dbConnect(SQLite(), dpth)
    conn <- rdb_connect()
    clnms <- DBI::dbListFields(conn, "mtd")
    if(all(clnms %in% names(mtd))) {
        mtd <- mtd[, clnms]
        DBI::dbSendQuery(conn, "DELETE FROM mtd")
        DBI::dbWriteTable(conn, "mtd", mtd, append = TRUE, overwrite = FALSE)
        
        DBI::dbDisconnect(conn) 
        TRUE
        
    } else {
        print("NOPE. Columns missing!")
        FALSE
    }
}
