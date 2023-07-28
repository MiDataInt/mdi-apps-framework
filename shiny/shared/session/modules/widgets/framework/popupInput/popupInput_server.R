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
    active = NULL,    
    size = "l",
    type = 'okCancel', 
    easyClose = TRUE,
    updateLabel = TRUE,
    labelCol = "label"
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
    req(is.null(active) || active())
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
if(updateLabel) observeEvent(value(), {
    val <- value()
    label <- if(is.list(val)) { # handles lists and data.frame/table
                if(!objectHasData(val) || is.null(val[[labelCol]])) "Click Me" else val[[labelCol]]
            } else if(is.null(val) || is.na(val) || length(val) == 0) "Click Me" 
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
