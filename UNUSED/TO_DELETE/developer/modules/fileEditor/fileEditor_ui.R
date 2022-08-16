
#----------------------------------------------------------------------
# static components that provide a code editor for framework files
# used live within the running application framework
#----------------------------------------------------------------------
# must never be enabled in server mode, as it allows arbitrary code execution 
#----------------------------------------------------------------------

# module ui function
fileEditorUI <- function(id, options) {
    
    # initialize namespace
    ns <- NS(id)

    # incorporate options text into templates
    leaderText <- tagList(
        #tags$p("Use this code editor to edit the files in your local repository.")
        tags$p("")
    )

    # return the UI contents
    standardSequentialTabItem("", leaderText,
        fluidRow(
            box(
                width=4,
                title = "File Selector",
                status = 'primary',
                solidHeader = TRUE,
                # custom search box
                shinyTree::shinyTree(
                    ns("fileTree"),
                    checkbox = FALSE,
                    search = FALSE,
                    searchtime = 250,
                    dragAndDrop = FALSE,
                    types = NULL,
                    theme = "default",
                    themeIcons = FALSE,
                    themeDots = TRUE,
                    sort = FALSE,
                    unique = FALSE,
                    wholerow = TRUE,
                    stripes = FALSE,
                    multiple = FALSE,
                    animation = 200,
                    contextmenu = FALSE
                )                 
            ),
            box(
                width=8,
                title = "Code Editor",
                status = 'primary',
                solidHeader = TRUE,
                tags$div(
                    id=ns("code-editor"),
                    style="height: 800px;"
                )                  
            ),
        )
    )    
}

