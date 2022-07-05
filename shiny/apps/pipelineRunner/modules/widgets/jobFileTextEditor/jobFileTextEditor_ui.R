#----------------------------------------------------------------------
# static components for text editing a job configuration file
#----------------------------------------------------------------------

# module ui function
jobFileTextEditorUI <- function(id) {
    ns <- NS(id)
    fluidRow(box(
        width = 12,
        title = "Job File Editor",
        status = 'primary',
        solidHeader = TRUE,
        style = "padding: 0;",
        tags$div(
            textOutput(ns("editorFile")),
            style = "line-height: 2em; border-left: 47px solid #eee; padding-left: 10px; font-style: italic; border-bottom: 1px solid #ddd;"
        ),
        tags$div(
            id = ns("editor"),
            style = "min-height: 650px;"
        )                  
    ))
}
