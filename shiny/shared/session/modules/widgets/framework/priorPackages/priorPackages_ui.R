#----------------------------------------------------------------------
# static components for creating a button to load a previously loaded data package
#----------------------------------------------------------------------

# module ui function
priorPackagesUI <- function(
    id
) {
    ns <- NS(id)
    tagList(
        tags$p(
            "Select a previously loaded data package to load again and click OK."
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
        )     
    )
}
