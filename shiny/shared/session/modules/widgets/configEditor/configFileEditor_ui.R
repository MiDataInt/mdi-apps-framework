#----------------------------------------------------------------------
# static components that provide a code editor for MDI installation config files
#----------------------------------------------------------------------

# module ui function
configFileEditorUI <- function(id) {
    ns <- NS(id)
    tags$div(
        tags$p("Please select and edit the desired MDI installation configuration files."),
        fluidRow(
            box(
                width = 3,
                title = NULL, # "File Selector",
                status = 'primary',
                solidHeader = FALSE,
                shinyTree::shinyTree(
                    ns("fileTree"),
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
            box(
                width = 9,
                title = NULL, # "Editor",
                status = 'primary',
                solidHeader = FALSE,
                tags$div(
                    id = ns("editor"),
                    style = "height: 500px;"
                )                  
            )
        )
    )
}
