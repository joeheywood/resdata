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
#' @import here
#'
#' @examples
#' dat <- data.frame()
#' insert_db(dat)
insert_db <- function(dat, log = "", excfl = "") {
    save(dat, log, excfl, file = "debug.Rda")
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
        lmx <- ""
        last_max <- NA
        new_vals <- FALSE
        prev <- tbl(conn, "ind_dat") %>% 
            filter(dataset == dtst) %>% 
            collect()
        # print(tail(prev))
        if(nrow(prev) > 0) {
            if(prev$xwhich[1] == 2) {
                last_max <- as.Date(max(prev$xvardt), origin = "1970-1-1")
                new_vals <- max(dat$xvardt) > last_max
                last_max <- as.character(last_max)
            } else {
                last_max <- max(prev$xvarchar)
                new_vals <- max(dat$xvarchar) > last_max
            }
            
            lmx <- glue("Last max: {last_max}\n\n")
        } else {
            lmx <- "Last max: N/A"
        }
        gsql <- glue_sql("DELETE FROM ind_dat WHERE dataset = {dtst}", 
                         .con = conn)
        qry <- dbSendQuery(conn = conn, gsql)
        rws <- dbGetRowsAffected(qry)
        cat(glue("Number of rows deleted: {rws}\n\n"), file = log, append = TRUE)
        cat(lmx, file = log, append = TRUE)
        dbClearResult(qry)
        dbAppendTable(conn, "ind_dat", dat)
        cat(glue("Number of rows added: {nrow(dat)}\n\n"), 
            file = log, append = TRUE)
    } else {
        dbWriteTable(conn, "ind_dat", dat)
        print(glue("Number of rows added: {nrow(dat)}\n\n"), file = log, 
              append = TRUE)
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

