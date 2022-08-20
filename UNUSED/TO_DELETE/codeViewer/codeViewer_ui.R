#----------------------------------------------------------------------
# static components that provide a code viewer for page-level MDI scripts
#----------------------------------------------------------------------

# module ui function
codeViewerUI <- function(id) {
    ns <- NS(id)

    addTree <- function(id){
        tagList(
            tags$strong(textOutput(ns(paste0(id, 'Path')))),
            shinyTree::shinyTree(
                ns(paste0(id, 'Tree')),
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
        )   
    }

    tags$div(
        actionLink(ns('toggleCode'), "Show Code"),
        fluidRow(
            id = ns("codeViewerDiv"),
            #style = "border-top: 2px solid grey; margin: 0.5em 0 0 0; padding-top: 1em;",
            style = "display: none; border-top: 2px solid grey; margin: 0.5em 0 0 0; padding-top: 1em;",
            column(
                width = 3,
                style = "padding: 0;",
                addTree('appStep'),
                addTree('app')
            ),
            column(
                width = 9,
                tags$strong(textOutput(ns('workingFile'))),
                tags$div(
                    id = ns("editor"),
                    style = "height: 500px;"
                )                  
            )
        )                
    )
}
