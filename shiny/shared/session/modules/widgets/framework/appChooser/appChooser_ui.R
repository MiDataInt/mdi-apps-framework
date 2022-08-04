#----------------------------------------------------------------------
# static components for constructing a modal panel to select an app directly
#----------------------------------------------------------------------

# module ui function
appChooserUI <- function(
    id
) {
    ns <- NS(id)
    tagList(
        fluidRow(
            class = "",
            tags$p("pending")
        )
    )
}
