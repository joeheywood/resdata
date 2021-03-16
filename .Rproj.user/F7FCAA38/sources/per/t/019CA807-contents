#' Chart from data - produces chart for single indicator
#'
#' @param dtst Character string that is primary key for indicator
#'
#' @return
#' Plotly chart corresponding to indicator key given
#' @export
#'
#' @examples
#' chart_from_data("unempl")
#' 
#' @import dplyr
#' @import RSQLite
#' @import DBI
#' 
chart_from_data <- function(dtst) {
    # print(dtst)
    x <- get_data_for_dataset(dtst)
    dat <- x$dat
    m <- x$m
    sub <- x$sub
    
    if((nrow(dat) == 0 & nrow(sub) == 0)| nrow(m) == 0) {
        return(glue("Not enough data! Data: {nrow(dat)} Meta: {nrow(m)} 
                    Subregional: {nrow(sub)}"))
    }
    if(m$borolevel == TRUE) {
        sub$x <- as.Date(sub$x, origin = as.Date("1970-01-01"))
        return(
            dsh_sub_region_line(sub, m$charttitle, xlb = m$xaxistitle, 
                                ylb = m$yaxistitle, src = m$source,  
                                lnk = m$link, hv = m$hvr) )
    }  

    
    if(dat$xwhich[1] == 2) {
        dat$xvar <- as.Date(dat$xvardt,
                              origin = as.Date("1970-01-01"))
    } else {
        dat$xvar <- as.character(dat$xvarchar)
    }
    
    
    numlbls <- length(unique(dat$yvllb))
    if(numlbls == 1) {
        if(m$bar == TRUE) {
            dsh_bar(dat, ttl = m$charttitle, yxttl = m$yaxistitle, 
                    xxttl = m$xaxistitle, src = m$source, lnk = m$link, 
                    tckfmt = m$tickformat, hv = m$hvr)
        } else {
            dsh_line(dat, m$charttitle, m$markers, m$linewidth, m$source,
                     m$link, m$xaxistitle, m$yaxistitle, m$tickformat, m$hvr)
        }
    } else {
        if(m$bar == TRUE) {
            dsh_mbar(dat, ttl = m$charttitle, yxttl = m$yaxistitle, 
                    xxttl = m$xaxistitle, src = m$source, lnk = m$link, 
                    tckfmt = m$tickformat, hv = m$hvr, stack = m$stack)
            
        } else {
            dsh_mline(dat, ttl = m$charttitle, mrk = m$markers, 
                      wdth = m$linewidth, src = m$source,  link = m$link, 
                      xxttl = m$xaxistitle, yxttl = m$yaxistitle, 
                      tckfmt = m$tickformat, hv = m$hvr,  hgh = m$highlight)
        }
    }
    
}

#' get data for single indicator
#' @param dtst Character string of dataset primary key
get_data_for_dataset <- function(dtst) {
    ## get data sqlite, csv or RData files depeneding on what is 
    ## in the "dash_data" environment variable
    fl <- Sys.getenv("DASH_DATAPATH")
    fltype <- tools::file_ext(fl)
    
    if(fltype == "sqlite") {
        return(data_from_sqlite(dtst = dtst, fl = fl))
    } else if(fltype == "csv") {
        return(data_from_csv(dtst, fl))
    }
}

#' get data for single indicator from sqlite database
#' @param dtst Character string of dataset primary key
#' @param fl Path location of sqlite file
data_from_sqlite <- function(dtst, fl) {
    conn <- dbConnect(SQLite(), fl)
    dat <- tbl(conn, "ind_dat") %>%  
        filter(dataset == dtst) %>% 
        collect()
    m <- tbl(conn, "mtd") %>% 
        filter(dataset == dtst) %>% 
        collect()
    sub <- tbl(conn, "ind_sub_dat") %>% 
        filter(dtst == dtst) %>% 
        collect()
    dbDisconnect(conn)
    list(dat = dat, meta = m, sub = sub)
}

#' get data for single indicator from csv files
#' @param dtst Character string of dataset primary key
#' @param dr Path where csv files are kept
data_from_csv <- function(x, na.rm = TRUE) {
}

#' get data for single indicator from RData files
#' @param dtst Character string of dataset primary key
#' @param fl Path location of RData file
data_from_csv <- function(x, na.rm = TRUE) {
}

