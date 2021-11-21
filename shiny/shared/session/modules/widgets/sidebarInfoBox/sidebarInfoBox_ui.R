#----------------------------------------------------------------------
# information display widget for sidebar
#----------------------------------------------------------------------

# module ui function
sibebarInfoBoxUI <- function(id, supertitle = "") {
    
    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    tags$div(
        class = "sidebar-info-box",
        tags$div(supertitle, class = "sidebar-info-box-supertitle"),        
        tags$div(textOutput(ns('value')), class = "sidebar-info-box-value")
    )         
}
