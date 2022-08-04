#----------------------------------------------------------------------
# create MDI-styled tooltips and popovers
#----------------------------------------------------------------------
# bootstrap tooltip options that can be set are listed here:
#   https://www.w3schools.com/bootstrap/bootstrap_ref_js_tooltip.asp
#----------------------------------------------------------------------
# colors and other styling are set in www/framework.css, here:
#   .tooltip
#   .tooltip-inner{}
#----------------------------------------------------------------------

# standardize tooltip appearance and behavior
mdiTooltipOptions <- list(
    delay = 200,
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
mdiTooltip <- function(session, id, title, placement = "top", ui = FALSE, useNamespace = TRUE){
    title <- paginateTooltip(title)
    if(useNamespace) id <- session$ns(id)

    # caller is adding a tooltip via UI as part of a renderUI expression
    if(ui) bsTooltip(id, title, placement, options = mdiTooltipOptions)

    # caller is adding a tooltip from within a server function (but not via renderUI)
    else addTooltip(session, id, title, placement, options = mdiTooltipOptions)
}

# multiple tooltips, e.g., called by a module server function
# takes a list of tooltips, each as character(id, title, [placement])
mdiTooltips <- function(session, tooltips, ui = FALSE){
    for(tooltip in tooltips){
        placement <- if(is.na(tooltip[3])) "top" else tooltip[3]
        mdiTooltip(session, tooltip[1], tooltip[2], placement, ui)
    }
}

# a single tooltip placed from a UI function
mdiTooltipUI <- function(id, title, placement = "top"){
    # title <- paginateTooltip(title)
    bsTooltip(id, title, placement, options = mdiTooltipOptions)
}

# a function to add a ? help tooltip after an input label
addInputHelp <- function(session, id, title){
    labelId <- paste(id, 'label', sep = "-")
    helpId  <- paste(id, 'help',  sep = "-")
    insertUI(
        paste0('#', session$ns(labelId)),
        where = "beforeEnd",
        tags$span(id = session$ns(helpId), class = "mdi-help-icon", icon("question")),
        immediate = TRUE,
        session = session
    )
    mdiTooltip(session, labelId, title)
}
