#----------------------------------------------------------------------
# static components for a set of inputs that help users select
# a SampleSet as well as filter for a specific group and/or type of Sample
#----------------------------------------------------------------------

# module ui function
sampleSetGroupTypeUI <- function(id) {

    # initialize namespace
    ns <- NS(id)
    
    # determine which selectInputs are needed
    sampleSetSource <- appStepNamesByType$assign
    categories <- app$config$appSteps[[sampleSetSource]]$options$categories

    # return the UI contents
    fluidRow(
        column(width = 4, bookmarkInput('selectInput', ns('sampleSet'), 'Sample Set', "")),
        column(width = 4, bookmarkInput('selectInput', ns('group'), categories$group$singular, "")),
        column(width = 4, bookmarkInput('selectInput', ns('type'),  categories$type$singular,  ""))
    )
}
