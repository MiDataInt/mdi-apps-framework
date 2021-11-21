#----------------------------------------------------------------------
# static components for a widget that scrolls through a list of values
# << < ## of ## > >>
#----------------------------------------------------------------------

# module ui function
listStepperButtonsUI <- function(id, textAlign="left") {

    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    nbsp <- HTML("&nbsp;")
    tags$div(
        style = paste("text-align:", textAlign, ";"),
        actionButton(ns('first'), '<<'),
        nbsp,
        actionButton(ns('previous'), '<'),
        nbsp,
        tags$div(bookmarkInput('textInput', ns('current'), NULL, 1, width = '40px'),
                 style = "display:inline-block; text-align: center;"),
        nbsp,
        tags$span(" of "),
        nbsp,
        textOutput(ns('total'), inline = TRUE),
        nbsp,
        actionButton(ns('next_'), '>'),
        nbsp,
        actionButton(ns('last'), '>>'),
        nbsp,
        nbsp,
        tags$strong(textOutput(ns('name'), inline = TRUE))
    )
}
