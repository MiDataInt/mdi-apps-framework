#----------------------------------------------------------------------
# server components for the popupInput widget module
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
popupInputServer <- function(
    id,
    title,
    callback,
    ..., # additional options passed to showUserDialog
    size = "l",
    type = 'okCancel', 
    easyClose = TRUE
) { 
    moduleServer(id, function(input, output, session) {    
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
module <- 'popupInput'
value <- reactiveVal(NULL)

#----------------------------------------------------------------------
# open dialog in response to button click for complicated input value selection
#----------------------------------------------------------------------
observeEvent(input$button, {   
    showUserDialog(
        title,
        ...,
        callback = function(parentInput) value( callback(parentInput) ),
        size = size,
        type = type, 
        easyClose = easyClose
    )
})

#----------------------------------------------------------------------
# update the button label with the selected value
#----------------------------------------------------------------------
observeEvent(value(), {
    val <- value()
    label <- if(is.null(val) || is.na(val)) "Click Me" 
             else if(is.list(val)) val$label
             else val
    updateActionButton(session, "button", label = label)
})

#----------------------------------------------------------------------
# set return value, typically NULL or a list of reactives
#----------------------------------------------------------------------
value

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
