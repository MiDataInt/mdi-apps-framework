#----------------------------------------------------------------------
# create MDI-styled tooltips and popovers
#----------------------------------------------------------------------
# bootstrap tooltip options that can be set are listed here:
#   https://www.w3schools.com/bootstrap/bootstrap_ref_js_tooltip.asp
#----------------------------------------------------------------------
# colors and other styling are set in CSS, here:
#   .tooltip.top .tooltip-inner{}
#----------------------------------------------------------------------

# standardize tooltip appearance
mdiTooltipOptions <- list(
    delay = 200, # shinyBS::bsTooltip no longer seems to honor this 
    animation = TRUE
)
toolTipLineWidth <- 35

# ensure tooltip text has ~ equal characters in every text line
paginateTooltip <- function(title){
    if(nchar(title) < toolTipLineWidth) return(title)
    words  <- strsplit(title, " ")[[1]]
    nWords <- length(words)
    nChars <- nchar(title)
    lines <- ""
    nLines <- 1
    while(nChars / nLines > toolTipLineWidth) nLines <- nLines + 1
    lineLength <- nChars / nLines
    i <- 1
    j <- 1
    while(j <= nWords){
        lines[i] <- paste(lines[i], words[j])
        j <- j + 1
        if(nchar(lines[i]) > lineLength) {
            i <- i + 1
            lines[i] <- ""
        }
    }
    paste(lines, collapse = "<br>")
}

# a single tooltip
mdiTooltip <- function(ns, id, title, placement = "top", ui = FALSE){
    title <- paginateTooltip(title)

    # caller is adding a tooltip via UI as part of a renderUI expression
        if(ui) bsTooltip(ns(id), title, placement, options = mdiTooltipOptions)

    # caller is adding a tooltip from within a server function (but not via renderUI)
        else addTooltip(session, ns(id), title, placement, options = mdiTooltipOptions)
}

# multiple tooltips, e.g., called by a module server function
# takes a list of tooltips, each as character(id, title, [placement])
mdiTooltips <- function(ns, tooltips, ui = FALSE){
    for(tooltip in tooltips){
        placement <- if(is.na(tooltip[3])) "top" else tooltip[3]
        mdiTooltip(ns, tooltip[1], tooltip[2], placement, ui)
    }
}

# a single tooltip placed from a UI function
mdiTooltipUI <- function(id, title, placement = "top"){
    # title <- paginateTooltip(title)
    bsTooltip(id, title, placement, options = mdiTooltipOptions)
}
