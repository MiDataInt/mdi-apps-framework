#----------------------------------------------------------------------
# reactive components for a set of inputs that help users select
# a single data source associated with a SampleSet 
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sampleSetSourceServer <- function(id, parentId) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- function(x) paste(parentId, id, x, sep = "-")
        module <- 'sampleSetSource' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
sampleSetSource <- appStepNamesByType$assign
source <- app[[sampleSetSource]] # i.e. the app step source for sample assignment into sets (not the input data source)
sourceOptions <- app$info$appSteps[[sampleSetSource]]$options

#----------------------------------------------------------------------
# fill the Sample Set and Source selectors
#----------------------------------------------------------------------
observeEvent({
    source$outcomes$sampleSets()
    source$outcomes$sampleSetNames()
}, {
    x <- getSampleSetNames()
    req(length(x) > 0)
    updateSelectInput(session, 'sampleSet', choices = setNames(names(x), x))
})
observeEvent(assignments(), {
    dataSources <- unique(assignments()$Source_ID)
    names(dataSources) <- sapply(dataSources, function(sourceId){
        getSourceFromId(sourceId)$unique$Project[1]
    })
    updateSelectInput(session, 'dataSource', choices = dataSources)
})

#----------------------------------------------------------------------
# react to user set/source selections by setting a reactive
#----------------------------------------------------------------------
assignments <- reactive({
    req(input$sampleSet)    
    getSampleSetAssignments(input$sampleSet)
})
allAssignments <- assignments

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    assignments = assignments,       # the set of samples assigned to the selected sampleSet+group+type
    allAssignments = allAssignments, # the set of all samples in the selected sampleSet regardless of assignment
    input = input
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
