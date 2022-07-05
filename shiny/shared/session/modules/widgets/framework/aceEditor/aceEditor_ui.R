#----------------------------------------------------------------------
# static components for constructing an Ace code viewer/editor panel
#----------------------------------------------------------------------

# module ui function
aceEditorUI <- function(
    id, 
    options = list(
        baseDir   = character(),
        editable  = FALSE,
        fileTree  = FALSE,
        multiPane = FALSE
    ),
    state = list()
) {
    ns <- NS(id)
    widths <- list(
        tree   = if(options$fileTree) 4  else 0,
        editor = if(options$fileTree) 8 else 12
    )
    names(options$baseDir) <- basename(options$baseDir)
    tagList(
        fluidRow(
            class = "aceEditor-controls",
            if(options$fileTree) column(
                width = widths$tree,
                selectInput(ns("baseDir"), label = NULL, choices = options$baseDir, selected = state$baseDir)
            ) else "",
            if(options$multiPane) column(
                width = widths$editor,
                style = "padding-left: 0;",
                uiOutput(ns("tabs"))
            ) else ""
        ),
        fluidRow(
            if(options$fileTree) column(
                width = widths$tree,
                class = "ace-editor-tree-lg",
                style = "overflow: auto; padding-right: 0;",
                shinyTree::shinyTree(
                    ns("tree"),
                    checkbox = FALSE,
                    search = FALSE,
                    searchtime = 250,
                    dragAndDrop = FALSE,
                    types = NULL,
                    theme = "proton",
                    themeIcons = TRUE, #FALSE,
                    themeDots = TRUE,
                    sort = FALSE,
                    unique = FALSE,
                    wholerow = TRUE,
                    stripes = FALSE,
                    multiple = FALSE,
                    animation = FALSE,
                    contextmenu = FALSE
                )    
            ) else "",
            column(
                width = widths$editor,
                style = "padding-left: 0; border-left: 1px solid #ddd;",
                tags$div(
                    textOutput(ns("file")), # the display of the active file path
                    class = "aceEditorDialog-file"
                ),
                tags$div(
                    id = ns("ace"), # the Ace editor itself
                    class = "ace-editor ace-editor-lg"
                )               
            )
        ),
        fluidRow(column(
            width = 12,
            actionLink(ns("toggleWidth"),  "Toggle Width",  style = "margin-right: 15px;"),
            actionLink(ns("toggleHeight"), "Toggle Height", style = "margin-right: 15px;")
        ))
    )
}
