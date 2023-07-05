#----------------------------------------------------------------------
# static components to generate a DT/datatable that uses a buffer
# to minimize repeated redraws of the table
#----------------------------------------------------------------------

# module ui function
bufferedTableUI <- function(id, title = NULL, downloadable = FALSE, settings = NULL, ...) {
    ns <- NS(id)
    if(downloadable && is.null(title)) title <- ""
    if(!is.null(title)){ # require a title, even an empty one, to use an enhanced box around the table
        title <- tags$span(
            tags$span(id = ns("title"), title),
            tags$span(id = ns("titleSuffix"), "")
        )
        if(downloadable) title <- tagList(
            title,
            tags$span(
                style = "font-size: 0.8em; margin-left: 10px;", 
                downloadLink(ns("download"), label = icon("download"))
            )
        )
        if(isTruthy(settings)) title <- tagList(
            title,
            tags$span(
                style = "font-size: 0.8em; margin-left: 10px;", 
                actionLink(ns("openSettings"), NULL, icon("cog"))
            )
        )
        box(title = title, ..., DTOutput(ns('table')))
    } else {
        DTOutput(ns('table'))
    }
}
