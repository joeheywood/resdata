#' Add borough-level data to database
#'
#' @param dat A data.frame with correct columns for the boro table and at least one row
#'
#' @param log Location of log file. Default is print to console
#' 
#' @return
#' Logical - TRUE for worked ok.
#' @export
#'
#' @examples
#' dat <- data.frame()
#' insert_db(dat)
#' @import readxl
#' @import glue
#' @import RSQLite
insert_boro_db <- function(dat, log = "", excfl = "") {
    dpth <- Sys.getenv("DASH_DATAPATH")
    dtst <- unique(dat$dataset)
    clmns <- c("dataset", "x", "y", "lad11nm")
    colnames(dat) <- tolower(names(dat))
    
    ## throw error if all the columns are not there
    if(!all(clmns %in% names(dat))) {
        print(names(dat))
        stop("This is not going to work. You need all the columns")
    } else {
        ## remove columns we don't need
        dat <- dat[, clmns]
        ## make sure this is date
        dat$x <- as.Date(as.numeric(dat$x), origin = "1970-1-1")
    }
    cat(glue("\n\n############ ADDING {dtst} ####################\n\n"), 
        file = log, append = TRUE)
    # conn <-  dbConnect(SQLite(), dpth)
    conn <- rdb_connect()
    if("ind_boro_dat" %in% dbListTables(conn)) {
        lmx <- ""
        last_max <- NA
        new_vals <- FALSE
        prev <- tbl(conn, "ind_dat") %>% 
            filter(dataset == dtst) %>% 
            collect()
        
        if(nrow(prev) > 0) {
            if(prev$xwhich[1] == 2) {
                print("getting date")
                last_max <- as.Date(max(prev$x), origin = "1970-1-1")
                new_vals <- max(dat$x) > last_max
                last_max <- as.character(last_max)
            } else {
                print("getting chars")
                last_max <- max(prev$x)
                new_vals <- max(dat$x) > last_max
            }
            
            lmx <- glue("Last max: {last_max}\n\n")
        } else {
            lmx <- "Laste max: N/A\n\n"
        }
        
        gsql <- glue_sql("DELETE FROM ind_boro_dat WHERE dataset = {dtst}", 
                         .con = conn)
        qry <- dbSendQuery(conn = conn, gsql)
        rws <- dbGetRowsAffected(qry)
        cat(glue("Number of rows deleted: {rws}\n\n"), file = log, append = TRUE)
        dbClearResult(qry)
        dbAppendTable(conn, "ind_boro_dat", dat)
        cat(glue("Number of rows added: {nrow(dat)}\n\n"), 
            file = log, append = TRUE)
    } else {
        dbWriteTable(conn, "ind_boro_dat", dat)
        cat(glue("Number of rows added: {nrow(dat)} TO NEW TABLE\n\n"), 
            file = log, append = TRUE)
    }
    upds <- data.frame(dataset = dat$dataset[1], 
                       lastmax = last_max,
                       newvals = new_vals,
                       timestamp = Sys.time(),
                       execfile = excfl)
    if("updates" %in% dbListTables(conn)) {
        dbAppendTable(conn, "updates", upds)
    } else {
        dbWriteTable(conn, "updates", upds)
        
    }
    # dbCommit(conn)
    dbDisconnect(conn) 
    TRUE
}


####
# Important to remember that for this data, a London-level data set needs to 
# be created and stacked to the rest. 
