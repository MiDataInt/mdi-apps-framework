#----------------------------------------------------------------------
# options list tools
#----------------------------------------------------------------------

# fill in any missing options values with module-provided defaults
setDefaultOptions <- function(options, defaults){
    if(is.null(options)) options <- list()
    for(option in names(defaults)){
        if(is.null(options[[option]])) {
            options[[option]] <- defaults[[option]]
        }
    }
    options
}

#----------------------------------------------------------------
# spinner control
#----------------------------------------------------------------
createSpinner <- function(session){ # insertUI needed due to shinydashboard limitations
    insertUI("body", where = 'afterBegin', immediate = TRUE,        
        ui = tags$div(
            class = "progress-spinner-div",
            tags$div(
                class = "progress-spinner",
                role = "status",
                tags$span(class = "sr-only", "Loading...")
            )   
        )
    )
}
startSpinner <- function(session, caller = NULL, message = NULL, log = FALSE){
    if(log) {
        if(is.null(caller)) caller <- ''
        reportProgress(caller, paste('>>> startSpinner', message))
    }
    session$sendCustomMessage('toggleSpinner', list(
        visibility = 'visible',
        message = message
    ))
}
updateSpinnerMessage <- function(session, message){
    session$sendCustomMessage('toggleSpinner', list(
        visibility = 'visible',
        message = message
    ))
}
stopSpinner <- function(session, caller = NULL, log = FALSE){
    if(log) {
        if(is.null(caller)) caller <- ''
       reportProgress(caller, '<<< stopSpinner')
    }
    session$sendCustomMessage('toggleSpinner', list(
        visibility = 'hidden',
        message = ""
    ))
}

#----------------------------------------------------------------
# html bits
#----------------------------------------------------------------

# Shiny does not have hidden inputs(?)
hiddenDiv <- function(...) tags$div(class = "hidden", ...)

#----------------------------------------------------------------
# provide feedback text that can be modified from multiple sources, with error coloring
#----------------------------------------------------------------
recordFeedbackFunction <- function(output, outputId){
    noMargin <- "margin: 0 8px;"
    nullMessage <- tags$p(style = noMargin, HTML("&nbsp;"))
    feedback <- reactiveVal("")
    output[[outputId]] <- renderUI({
        f <- feedback()
        if(is.null(f)) f <- nullMessage
        f
    })
    function(message, isError=FALSE){
        style <- if(isError) "color: rgb(200,0,0);" else ""
        if(is.null(message)) message <- "&nbsp;"
        feedback(tags$p(style = paste(noMargin, style), HTML(message)))
        if(isError) req(FALSE) # simple way to generate a silent error
    }
}

#----------------------------------------------------------------
# shortcut for standard, collapsible box 
#----------------------------------------------------------------
collapsibleBox <- function(
    ...,   
    title = NULL,
    width = 6,
    status = 'primary',
    solidHeader = TRUE,
    collapsed = FALSE
){
    box(
        ...,
        title = title,        
        width = width,
        status = status,
        solidHeader = solidHeader,
        collapsible = TRUE,
        collapsed = collapsed
    )
}

#----------------------------------------------------------------
# UI/graphics unit conversion
#----------------------------------------------------------------
getInches <- function(value, unit, linesPerInch){
    if(is.null(unit)) unit <- "inches"
    switch(
        substr(unit, 1, 2),
        "in" = value,
        "cm" = value / CONSTANTS$cmPerInch,
        "li" = value / linesPerInch
    )
}

#----------------------------------------------------------------
# color manipulation
#----------------------------------------------------------------
addAlphaToColor <- function(color, alpha) { # expects a single color and a vector of alphas[0:1]
    rgb_ <- col2rgb(color)
    rgb(
        rgb_[1], rgb_[2], rgb_[3],
        max = 255,
        alpha = pmin(1, pmax(0, alpha)) * 255
    )
}

#----------------------------------------------------------------
# plotting helpers
#----------------------------------------------------------------
# jitter a vector of points for plotting, a bit more intelligently than just random placement
jitter2 <- function(v, min, max){
    N <- length(v)
    width <- max - min
    if(N == 1) return(min + width / 2) # place single points at center
    if(N <= 4) { # place only a few points closer around the center
        min <- min + width / 4
        max <- max - width / 4
    } 
    x <- seq(min, max, length.out = N)
    x[sample(N)]
}
