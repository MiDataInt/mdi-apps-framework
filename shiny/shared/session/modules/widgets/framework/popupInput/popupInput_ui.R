#----------------------------------------------------------------------
# UI components for the popupInput widget module
#----------------------------------------------------------------------

# module ui function
popupInputUI <- function(id, label, value = "Click Me", buttonFn = actionButton, icon = NULL) {
    ns <- NS(id)
    buttonId <- ns("button")
    tags$div(
        class = "form-group shiny-input-container",
        if(is.null(label)) "" else tags$label(
            id = ns("label"),
            class = "control-label",
            "for" = buttonId,
            label
        ),
        tags$div(
            buttonFn(
                buttonId, 
                label = value, 
                icon = icon,
                style = "width: 100%; text-overflow: ellipsis; overflow: hidden; direction: rtl;"
            )      
        )
    )
}
