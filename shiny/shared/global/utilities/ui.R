
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
    insertUI("body", where = 'afterBegin', immediate=TRUE,        
        ui = tags$div(
            class="progress-spinner-div",
            tags$div(
                class="progress-spinner",
                role="status",
                tags$span(class="sr-only", "Loading...")
            )   
        )
    )
}
startSpinner <- function(session, caller=NULL){
    if(is.null(caller)) caller <- ''
    reportProgress(caller, '>>> startSpinner')
    session$sendCustomMessage('toggleSpinner', 'visible')
}
stopSpinner <- function(session, caller=NULL){
    if(is.null(caller)) caller <- ''
    reportProgress(caller, '<<< stopSpinner')
    session$sendCustomMessage('toggleSpinner', 'hidden')
}

#----------------------------------------------------------------
# html bits
#----------------------------------------------------------------

# Shiny does not have hidden inputs(?)
hiddenDiv <- function(...) tags$div(class="hidden", ...)

#----------------------------------------------------------------
# provide feedback text that can be modified from multiple sources, with error coloring
#----------------------------------------------------------------
recordFeedbackFunction <- function(output, outputId){
    noMargin="margin: 0 8px;"
    nullMessage <- tags$p(style=noMargin, HTML("&nbsp;"))
    feedback <- reactiveVal("")
    output[[outputId]] <- renderUI({
        f <- feedback()
        if(is.null(f)) f <- nullMessage
        f
    })
    function(message, isError=FALSE){
        style <- if(isError) "color: rgb(200,0,0);" else ""
        if(is.null(message)) message = "&nbsp;"
        feedback(tags$p(style=paste(noMargin, style), HTML(message)))
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

