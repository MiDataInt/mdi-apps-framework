
#----------------------------------------------------------------------
# reactive components for a widget that enables users to construct an
# arbitrarily long list of an element class defined by the caller
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
listMakerServer <- function(id, parentId, options) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- NS(parentId)
        fullNs <- function(x) parentNs(ns(x))
        module <- 'listMaker' # for reportProgress tracing
        listId <- fullNs('list')
        listCssId <- paste0("#", listId)
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# activate sub-modules
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# add an item
#----------------------------------------------------------------------
itemI <- 0
fullNsI <- function(x) fullNs(paste(x, itemI, sep="-"))
observeEvent(input$addItem, {
    itemI <<- itemI + 1
    removeStepButton <- actionLink(fullNsI('removeItem'), 'remove', class="list-maker-remove")
    insertUI(listCssId, where = "beforeEnd", immediate=TRUE,
             ui = tags$div(class="rank-list-item", options$newItem(itemI, removeStepButton)))  # let caller place the remove button as desired  
})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

