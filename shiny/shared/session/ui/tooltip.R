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

#----------------------------------------------------------------------
# standardize tooltip appearance and behavior
#----------------------------------------------------------------------
mdiTooltipOptions <- list(
    animation = TRUE,    
    delay = list(show = 700, hide = 100),
    placement = "top",
    html = TRUE, # support <br> added by paginateTooltip
    viewport = list(selector = "body", padding = 0)
)

#----------------------------------------------------------------------
# ensure tooltip text has ~ equal characters in every text line
#----------------------------------------------------------------------
paginateTooltip <- function(title, lineWidth = 35){
    if(nchar(title) < lineWidth) return(title)
    words  <- strsplit(title, " ")[[1]]
    nWords <- length(words)
    nChars <- nchar(title)
    lines <- ""
    nLines <- 1
    while(nChars / nLines > lineWidth) nLines <- nLines + 1
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

#----------------------------------------------------------------------
# the main MDI tooltip functions, via javascript calls to bootstrap tooltip()
#----------------------------------------------------------------------

# add a single tooltip, ... passed as options to bootstrap tooltip()
addMdiTooltip <- function(session, id, ..., asis = FALSE, lineWidth = 35){
    options <- setDefaultOptions(list(...), mdiTooltipOptions)
    if(is.null(options$title)) return()
    options$title <- paginateTooltip(options$title, lineWidth)
    session$sendCustomMessage("addMdiTooltip", list(
        id = if(asis) id else session$ns(id),
        options = options
    ))
}
# add multiple tooltips, each as character(id, title, [lineWidth])
addMdiTooltips <- function(session, tooltips, ..., asis = FALSE, lineWidth = 35){
    for(tooltip in tooltips){
        addMdiTooltip(
            session, id = tooltip[1], title = tooltip[2], ..., asis = asis, 
            lineWidth = if(is.na(tooltip[3])) lineWidth else as.integer(tooltip[3])
        )
    }
}

# a function to add a ? help tooltip after an input label
addInputHelp <- function(session, id, title, lineWidth = 35){
    labelId <- paste(id, 'label', sep = "-")
    helpId  <- paste(id, 'help',  sep = "-")
    insertUI(
        paste0('#', session$ns(labelId)),
        where = "beforeEnd",
        tags$span(id = session$ns(helpId), class = "mdi-help-icon", icon("question", verify_fa = FALSE)),
        immediate = TRUE,
        session = session
    )
    addMdiTooltip(session, id = labelId, title = title, placement = "top", lineWidth = lineWidth)
}

#----------------------------------------------------------------------
# use shinyBS to add tooltips as part of a renderUI expression
#----------------------------------------------------------------------
mdiTooltip <- function(session, id, title, placement = "top", asis = FALSE, lineWidth = 35){
    if(!asis) id <- session$ns(id)
    title <- paginateTooltip(title, lineWidth)
    options <- mdiTooltipOptions
    options$placement <- NULL
    bsTooltip(id, title, placement, options = options)
}
