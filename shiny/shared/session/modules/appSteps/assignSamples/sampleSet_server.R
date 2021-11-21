#----------------------------------------------------------------------
# reactive components for a single input to select a SampleSet
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sampleSetServer <- function(id, parentId) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- function(x) paste(parentId, id, x, sep = "-")
        module <- 'sampleSet' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
sampleSetSource <- appStepNamesByType$assign
source <- app[[sampleSetSource]]

#----------------------------------------------------------------------
# fill the Sample Set selector
#----------------------------------------------------------------------
observeEvent({
    source$outcomes$sampleSets()
    source$outcomes$sampleSetNames()
}, {
    x <- getSampleSetNames()
    req(length(x) > 0)
    updateSelectInput(session, 'sampleSet', choices = setNames(names(x), x))
})

#----------------------------------------------------------------------
# react to user set/group/type selections by setting a reactive
#----------------------------------------------------------------------
assignments <- reactive({
    req(input$sampleSet)
    getSampleSetAssignments(input$sampleSet) # all assignment for this is sample set (i.e, all groups and types)
})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    assignments = assignments, # the set of sample sources assigned to the selected sampleSet+group+type
    input = input
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
