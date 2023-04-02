#----------------------------------------------------------------------
# static components for a widget that scrolls through a list of values
# << < ## of ## > >>
#----------------------------------------------------------------------

# module ui function
listStepperButtonsUI <- function(id, textAlign = "left", display = "inline-block", marginLeft = NULL) {

    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    nbsp <- HTML("&nbsp;")
    marginLeft <- if(is.null(marginLeft)) "" else paste("margin-left:", marginLeft, ";")
    tags$div(
        style = paste("text-align:", textAlign, ";", " display:", display, ";", marginLeft),
        actionButton(ns('first'), '<<', style = "vertical-align: top;"),
        # nbsp,
        actionButton(ns('previous'), '<', style = "vertical-align: top;"),
        # nbsp,
        tags$style(".listStepperCurrent .form-control { text-align: center; padding: 5px; }"),
        tags$div(bookmarkInput('textInput', ns('current'), NULL, 1, width = '50px'),
                 class = "listStepperCurrent",
                 style = "display:inline-block; text-align: center;"),
        # nbsp,
        # tags$span(" of "),
        nbsp,
        textOutput(ns('total'), inline = TRUE),
        nbsp,
        actionButton(ns('next_'), '>', style = "vertical-align: top;"),
        # nbsp,
        actionButton(ns('last'), '>>', style = "vertical-align: top;"),
        # nbsp,
        nbsp,
        tags$strong(textOutput(ns('name'), inline = TRUE))
    )
}
