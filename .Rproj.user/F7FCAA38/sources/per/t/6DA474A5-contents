#' Refresh the metadata 
#'
#'
#' @return
#' Logical - TRUE for worked ok
#' @export
#'
#' @examples
#' # my_median(1:12)
#' refresh_metadata()
refresh_metadata <- function() {
    # flpth <- file.path(dt_pth, "Recovery Data for Dashboard.xlsx")
    flpth <- Sys.getenv("DASH_METAPATH")
    dpth <- Sys.getenv("DASH_DATAPATH")
    
    mtd <- read_excel(flpth, "Charts info") 
    
    conn <- dbConnect(SQLite(), dpth)
    dbWriteTable(conn, "mtd", mtd, overwrite = TRUE)
    
    dbDisconnect(conn) 
    TRUE
}
