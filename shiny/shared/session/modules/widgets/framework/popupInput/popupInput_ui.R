#----------------------------------------------------------------------
# UI components for the popupInput widget module
#----------------------------------------------------------------------

# module ui function
popupInputUI <- function(id, label, value = "Click Me") {
    ns <- NS(id)
    buttonId <- ns("button")
    tags$div(
        class = "form-group shiny-input-container",
        tags$label(
            id = ns("label"),
            class = "control-label",
            "for" = buttonId,
            label
        ),
        tags$div(
            actionButton(buttonId, value, style = "width: 100%;")  
        )
    )
}
