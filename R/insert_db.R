#' Add data to database
#'
#' @param dat A data.frame with correct columns and at least one row
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
insert_db <- function(dat, log = "") {
    dpth <- Sys.getenv("DASH_DATAPATH")
    # tmstmp <- format(Sys.time(), "%Y-%m-%dT%H%M")
    # logfl <- file.path(dirname(dpth, glue("update_")))
    clmns <- c("dataset", "xwhich", "xvarchar", "xvardt", 
               "yval", "yvllb", "text")
    
    if(!all(clmns %in% names(dat))) {
        print(names(dat))
        stop("This is not going to work. You need all the columns")
    } else {
        dat <- dat[, clmns]
    }
    dtst <- unique(dat$dataset)
    cat(glue("\n\n############ ADDING {dtst} ####################\n\n"), 
        file = log, append = TRUE)
    if(dat$xwhich[1] == 2) {
        dat$xvardt <- as.Date(dat$xvardt, origin = "1970-1-1")
        mindt <- format(min(dat$xvardt), "%d %b '%y")
        maxdt <- format(max(dat$xvardt), "%d %b '%y")
        xmsg <- glue("Xaxis Date. Range:{mindt} - {maxdt}\n\n")
    } else {
        xmsg <- glue("Xaxis Character: {toString(unique(dat$xvarchar))}\n\n")
    }
    # conn <- dbConnect(SQLite(), dpth)
    conn <- rdb_connect()
    m <- tbl(conn, "mtd") %>% 
        filter(dataset == dtst) %>% 
        collect()
    if(nrow(m) == 0) {
        cat("NO METADATA ASSOCIATED WITH THIS DATA\n\n", file = log)
        
    } else {
        cat(m$charttitle, "\n\n", file = log, append = TRUE)
    }
    cat(glue("Y range: {min(dat$yval)} - {max(dat$yval)}\n\n"), 
        file = log, append = TRUE)
    cat(xmsg, file = log, append = TRUE)
    
    if("ind_dat" %in% dbListTables(conn)) {
        gsql <- glue_sql("DELETE FROM ind_dat WHERE dataset = {dtst}", 
                         .con = conn)
        qry <- dbSendQuery(conn = conn, gsql)
        rws <- dbGetRowsAffected(qry)
        cat(glue("Number of rows deleted: {rws}\n\n"), file = log, append = TRUE)
        dbClearResult(qry)
        dbAppendTable(conn, "ind_dat", dat)
        cat(glue("Number of rows added: {nrow(dat)}\n\n"), 
            file = log, append = TRUE)
    } else {
        dbWriteTable(conn, "ind_dat", dat)
        print(glue("Number of rows added: {nrow(dat)}\n\n"), file = log, 
              append = TRUE)
    }
    # dbCommit(conn)
    dbDisconnect(conn) 
    TRUE
}

