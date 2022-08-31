#----------------------------------------------------------------------
# static components for constructing an Ace code viewer/editor panel
#----------------------------------------------------------------------

# module ui function
aceEditorUI <- function(
    id, 
    baseDirs = NULL, # one or more directories from which all files are shown as trees
    showFile = NULL, # a single target file to show in lieu of baseDirs
    baseDir  = NULL, # which of baseDirs or showFile to place into view
    editable = FALSE # whether to show the create/delete links
) {
    ns <- NS(id)
    widths <- list(tree = 4, editor = 8)
    if(!is.null(showFile)){
        baseDirs <- dirname(showFile)
        baseDir <- baseDirs
    }
    names(baseDirs) <- basename(baseDirs)
    tagList(
        fluidRow(
            class = "aceEditor-controls",
            column(
                width = widths$tree, # choose the root of the file tree
                selectInput(ns("baseDir"), label = NULL, choices = baseDirs, selected = baseDir)
            ),
            column(
                width = widths$editor, # the file tabs
                style = "padding-left: 0;",
                uiOutput(ns("tabs"))
            )
        ),
        fluidRow(
            column( # the file navigation tree
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
            ),
            column(
                width = widths$editor,
                style = "padding-left: 0; border-left: 1px solid #ddd;",
                tags$div(
                    if(editable) tags$div(
                        class = "ace-file-menu",
                        actionLink(
                            ns("fileMenu"),   
                            "File",
                            class = "ace-file-option"
                        ),
                        actionLink(
                            ns("confirmAction"),   
                            "",
                            class = "ace-file-option"
                        ),
                        actionLink(
                            ns("deletePath"), 
                            "Delete", 
                            class = "ace-file-option ace-path-action ace-dir-action ace-file-second", 
                            style = "display: none;"
                        ),
                        actionLink(
                            ns("movePath"), 
                            "Move", 
                            class = "ace-file-option ace-path-action ace-dir-action", 
                            style = "display: none;"
                        ),
                        actionLink(
                            ns("addFile"), 
                            "Add File", 
                            class = "ace-file-option ace-dir-action", 
                            style = "display: none;"
                        ),
                        actionLink(
                            ns("addDir"), 
                            "Add Dir", 
                            class = "ace-file-option ace-dir-action", 
                            style = "display: none;"
                        ),
                        actionLink(
                            ns("cancelAction"), 
                            "Cancel", 
                            class = "ace-file-option ace-file-second", 
                            style = "display: none;"
                        )
                    ) else "",
                    textOutput(ns("file")), # the display of the active file path
                    class = "ace-file-line"
                ),
                tags$span(
                    id = ns("path-edit-wrapper"),
                    textInput(
                        ns("pathEdit"), # for editing the file path for move/rename
                        NULL,
                        value = "",
                        width = "100%"
                    ),
                    style = "display: none"
                ),
                tags$div(
                    id = ns("ace"), # the Ace editor itself
                    class = "ace-editor ace-editor-lg"
                )               
            )
        ),
        fluidRow(column( # syntax check reporting
            width = 12,
            class = "ace-editor-error-wrapper",
            style = "border: 1px solid #ddd; margin-bottom: 5px; background-color: #f5f5f5;",
            verbatimTextOutput(ns("error"))
        )),
        fluidRow(column( # some additional links
            width = 12,
            actionLink(ns("toggleWidth"),  "Toggle Width",  style = "margin-right: 15px;"),
            actionLink(ns("toggleHeight"), "Toggle Height", style = "margin-right: 15px;"),
            actionLink(ns("checkSyntax"),  "(Re)Check Syntax", style = "margin-right: 15px;")
        ))
    )
}
