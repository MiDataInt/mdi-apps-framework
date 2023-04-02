#----------------------------------------------------------------------
# reactive components for a widget that scrolls through a list of values
# << < ## of ## > >>
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
listStepperButtonsServer <- function(id, dataReactive, nameFn=NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'listStepperButtons' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------

# the number of events in the list
total   <- reactiveVal(0) 
current <- reactive(as.integer(input$current))

# the index of the list element being displayed
setCurrent <- function(i){
    i <- as.integer(i)
    if(i > total()) i <- total()    
    if(i < 1) i <- 1
    updateTextInput(session, 'current', value = i)
}

# the index that will be used when the stepper first load a new list
default <- reactiveVal(1)
overrideDefault <- function(inputId, value){ # set a temporary, one-time override on default
    inputId <- rev(strsplit(inputId, '-')[[1]])[1]
    if(inputId == 'current') default(value)
}
 
#----------------------------------------------------------------------
# react to user actions
#----------------------------------------------------------------------
observeEvent(dataReactive(), {
    x <- dataReactive()
    total(if(is.null(x)) 0 else nrow(x))    
    setCurrent(default())
    default(1) # remove any temporary override on the default value
})
output$total <- renderText({ total() })
observeEvent(input$first,    { setCurrent(1) })
observeEvent(input$previous, { setCurrent(current() - 1) })
observeEvent(input$next_,    { setCurrent(current() + 1) })
observeEvent(input$last,     { setCurrent(total()) })

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
output$name <- renderText({
    req(nameFn)    
    x <- dataReactive()
    i <- current()
    req(x, i, nrow(x) >= i)
    nameFn(x[i])
})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
getCurrent <- function(){
    x <- dataReactive()
    i <- current()
    req(x, i, nrow(x) >= i)
    x[i]
}
list(
    current = current,
    getCurrent = getCurrent,
    setCurrent = setCurrent,
    overrideDefault = overrideDefault,
    session
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
