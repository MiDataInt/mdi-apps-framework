#----------------------------------------------------------------------
# static components for constructing an interface to delete dataDir folders
#----------------------------------------------------------------------

# module ui function
serverCleanupUI <- function(
    id
) {
    ns <- NS(id)
    tagList(
        tags$p(
            "Use the checkboxes to select data packages to delete from the server."
        ),
        fluidRow(
            box(
                width = 12,
                collapsible = FALSE,
                bufferedTableUI(
                    ns("packageTable"), 
                    title = NULL, 
                    downloadable = FALSE
                )
            )
        ),
        fluidRow(
            actionLink(ns("deleteSelectedPackages"), "Delete Selected Packages", style = "margin-left: 25px;"),
            tags$span(tags$strong("This action is immediate and permanent!"), style = "margin-left: 15px;")
        )       
    )
}
