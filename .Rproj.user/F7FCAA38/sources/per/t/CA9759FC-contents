#' Produce a multi-line chart in plotly
#'
#' @param dat data frame with correct column headings
#'
#' @param ttl Chart title (character)
#' 
#' @param mrk TRUE if markers added to each point (default FALSE)
#' 
#' @param wdth Width of line (default 4)
#' 
#' @param src Character string of source of data
#' 
#' @param link Character string of link of source
#' 
#' @param xttl Character string of title for xaxis
#' 
#' @param yttl Character string of title for yaxis
#' 
#' @param tckfmt Tick format (eg % for percentage)
#' 
#' @param hv hovertemplate
#' 
#' @param hgh TRUE/FALSE if use highlight colour palette
#' 
#' @return
#' Multi-line plotly chart object
#' @export
#'
#' @examples
#' chart_from_data("unemp")
#' 
#' @import plotly
#' 
dsh_mline <- function(dat, ttl = "", mrk = FALSE, wdth = 4, src = "*", 
                     link = "", xxttl = "", yxttl = "", tckfmt = "", 
                     hv = "%{y}", hgh = FALSE) {
    
    nmv <- length(unique(dat$yvllb))
    
    if(hgh == TRUE) {
        mpl <- c("#6da7de", "#cccccc")
    } else {
        mpl <- c("#6da7de","#9e0059", "#dee000", "#d82222", "#5ea15d", 
                 "#943fa6", "#63c5b5", "#ff38ba")[1:nmv]
    }
    
    if(mrk == TRUE) {
        mrkx <- list(size = wdth * 2)
    } else {
        mrkx <- list()
    }
    
    hv <- paste0(hv, "<extra></extra>")
    
    lnx <- list(width = wdth)
    
    md <- ifelse(mrk, "lines+markers", "lines")
    
    # dat$xvar <- ifelse(dat$xwhich == 1, dat$xvarchar, dat$xvardt)
    
    plot_ly(
        type = "scatter", mode = md,  data = dat, colors = mpl,
        x = ~xvar, y = ~yval, color = ~yvllb, hovertemplate = hv, 
            text = ~text, line = lnx, marker = mrkx) %>%  
        layout(title = list(text = ttl, x = 0.01), 
               margin = TRUE,
               legend = list(orientation = "h"),
               hovermode = "x unified",
               annotations = add_source(src, link),
               yaxis = list(title = yxttl, 
                            showgrid = TRUE, 
                            tickformat = tckfmt,
                            gridcolor = "#dddddd"), 
               xaxis = list(title = xxttl, showgrid = FALSE))
    
}