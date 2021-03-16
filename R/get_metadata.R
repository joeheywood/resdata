#' get all metadata 
#'
#'
#' @return
#' All metadata in a data.frame
#' @export
#'
#' @examples
#' get_metadata()
#' 
#' import RSQLite
get_metadata <- function() {
    # flpth <- file.path(dt_pth, "Recovery Data for Dashboard.xlsx")
    dpth <- Sys.getenv("DASH_DATAPATH")
    
    
    conn <- dbConnect(SQLite(), dpth)
    
    mtd <- dbReadTable(conn, "mtd")
    
    
    dbDisconnect(conn) 
    mtd
}

