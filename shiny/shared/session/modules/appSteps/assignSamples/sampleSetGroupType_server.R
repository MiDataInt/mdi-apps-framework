#----------------------------------------------------------------------
# reactive components for a set of inputs that help users select
# a SampleSet as well as filter for a specific group and/or type of Sample
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sampleSetGroupTypeServer <- function(id, parentId) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- function(x) paste(parentId, id, x, sep = "-")
        module <- 'sampleSetGroupType' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
sampleSetSource <- appStepNamesByType$assign
source <- app[[sampleSetSource]]
sourceOptions <- app$config$appSteps[[sampleSetSource]]$options

#----------------------------------------------------------------------
# allows values overrides during bookmark loading
#----------------------------------------------------------------------
default <- reactiveValues(group = 1, type = 1)
overrideDefault <- function(inputId, value){
    inputId <- rev(strsplit(inputId, '-')[[1]])[1]
    default[[inputId]] <- value
}

#----------------------------------------------------------------------
# fill the Sample Set, Group and Type selectors
#----------------------------------------------------------------------
observeEvent({
    source$outcomes$sampleSets()
    source$outcomes$sampleSetNames()
}, {
    x <- getSampleSetNames()
    req(length(x) > 0)
    updateSelectInput(session, 'sampleSet', choices = setNames(names(x), x))
})
categories <- list()
observeEvent(input$sampleSet, {
    sss <- source$outcomes$sampleSets()
    req(sss)
    req(length(sss) > 0)
    categories <<- sss[[input$sampleSet]]$categoryNames
    values <- reactiveValuesToList(default)
    default$group <- 1
    default$type  <- 1
    mapply(function(id, i){
        cssId <- paste0('#', parentNs(id))
        x <- sourceOptions$categories[[id]]
        if(is.null(x)) return( hide(selector = cssId) ) 
        y <- categories[[i]]
        if(length(y) <= 1) return( hide(selector = cssId) ) 
        z <- seq_along(y)
        names(z) <- y
        updateSelectInput(session, id, choices = z, selected = values[[id]])
        show(selector = cssId)
    }, c('group', 'type'), 1:2)
})

#----------------------------------------------------------------------
# react to user set/group/type selections by setting a reactive
#----------------------------------------------------------------------
assignments <- reactive({
    req(input$sampleSet) 
    isGroup <- !is.null(sourceOptions$categories$group) && length(categories[[1]]) > 1
    isType  <- !is.null(sourceOptions$categories$type)  && length(categories[[2]]) > 1     
    if(isGroup) req(input$group)
    if(isType)  req(input$type)
    groupI <- if(isGroup) input$group else 1
    typeI  <- if(isType)  input$type  else 1
    getSampleSetAssignments(input$sampleSet, category1 = groupI, category2 = typeI)
})
allAssignments <- reactive({
    req(input$sampleSet)
    getSampleSetAssignments(input$sampleSet) # all assignment for this is sample set (i.e, all groups and types)
})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    assignments = assignments,       # the set of samples assigned to the selected sampleSet+group+type
    allAssignments = allAssignments, # the set of all samples in the selected sampleSet regardless of assignment
    overrideDefault = overrideDefault,
    input = input
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
