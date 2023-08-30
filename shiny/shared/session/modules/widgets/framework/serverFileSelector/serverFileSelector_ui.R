#----------------------------------------------------------------------
# static components for constructing an shinyFiles path browser, suitable for embedding in a modal
#----------------------------------------------------------------------

# module ui function
serverFileSelectorUI <- function(
    id
) {
    ns <- NS(id)
    paths <- getAuthorizedServerPaths("read")
    tagList(
        tags$div(
            selectInput(ns("basePath"), NULL, choices = names(paths))
        ),
        tags$div(
            style = "padding-left: 5px;",
            uiOutput(ns("currentPath"))
        ),
        tags$div(
            style = "padding-left: 15px; padding-top: 10px; vertical-align: top;",
            tags$div(
                style = "display: inline-block; vertical-align: top;",
                uiOutput(ns("currentPathSubDirs"))
            ),
            tags$div(
                style = "display: inline-block; vertical-align: top; padding-left: 25px; margin-left: 25px; border-left: 1px solid grey;",
                uiOutput(ns("currentPathFiles"))
            )
        )
    )
}
