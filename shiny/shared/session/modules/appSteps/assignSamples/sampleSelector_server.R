#----------------------------------------------------------------------
# reactive components to select one or more samples from a single sample set
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
sampleSelectorServer <- function(id, parentId) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- function(x) paste(parentId, id, x, sep = "-")
        module <- 'sampleSelector' # for reportProgress tracing
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
# react to user set selection by setting a reactive
#----------------------------------------------------------------------
assignments <- reactive({
    req(input$sampleSet)
    getSampleSetAssignments(input$sampleSet) # all assignment for this is sample set (i.e, all groups and types)
})

#----------------------------------------------------------------------
# open a dialog to enable sample selection from among the samples in the selected set
#----------------------------------------------------------------------
commitSelectedSamples <- function(parentInput){
    dprint(names(parentInput))
    # "sampleSelector-selectSample-1-1-1"
}
observeEvent(input$selectSamples, {
    assignments <- assignments()
    req(assignments)
# Classes 'data.table' and 'data.frame':  6 obs. of  5 variables:
#  $ Source_ID: chr  "6741c8f40c01f6ed4760406898b1607c" "6741c8f40c01f6ed4760406898b1607c" "6741c8f40c01f6ed4760406898b1607c" "6741c8f40c01f6ed4760406898b1607c" ...
#  $ Project  : chr  "high_APH_040422" "high_APH_040422" "high_APH_040422" "high_APH_040422" ...
#  $ Sample_ID: chr  "HCT_0.2APH_2APH_M" "HCT_0.2APH_G2" "HCT_0.2APH__M" "HCT_2APH_M" ...
#  $ Category1: int  2 1 2 2 1 2
#  $ Category2: int  4 2 2 3 1 1
    nCol <- assignments[, max(Category1)]
    nRow <- assignments[, max(Category2)]
    colWidth <- floor(12 / (nCol + 1))
    categoryNames <- getCategoryNames()

    stepName <- appStepNamesByType$assign
    sampleSet <- app[[stepName]]$outcomes$sampleSets()[[input$sampleSet]]
    dstr(sampleSet)

    # dstr(getSampleSetAssignments(input$sampleSet, categoryNames = TRUE)) #, category1=NULL, category2=NULL, categoryNames=FALSE)

    table <- fluidRow(
        style = "margin: 0 15px;",
        fluidRow(
            column(width = colWidth, ""),
            lapply(1:nCol, function(col){
                column(
                    style = "border-left: 1px solid grey; text-align: center;",
                    width = colWidth,
                    tags$strong(categoryNames$Category1)
                )
            })              
        ),
        lapply(1:nRow, function(row){
            fluidRow(
                style = "border-top: 1px solid grey;",
                column(
                    style = "text-align: right;",
                    width = colWidth, 
                    tags$strong(categoryNames$Category2)
                ),
                lapply(1:nCol, function(col){
                    sampleNames <- assignments[
                        Category2 == row & Category1 == col, 
                        getSampleNames(sampleUniqueIds = paste(Project, Sample_ID, sep = ":"))
                    ]
                    dstr(sampleNames)
                    nSamples <- length(sampleNames)
                    column(
                        style = "border-left: 1px solid grey;",
                        width = colWidth,
                        if(nSamples == 0) "-" else lapply(1:nSamples, function(i){
                            id <- paste('selectSample', row, col, i, sep = "-")
                            checkboxInput(ns(id), label = sampleNames[i], value = FALSE)
                        })
                    )
                })
            )
        })
    )
    showUserDialog(
        "Select Samples", 
        table, 
        callback = commitSelectedSamples,
        size = if(nCol > 3) "l" else "m"
    )
})

#----------------------------------------------------------------------
# provide feedback on the selected samples
#----------------------------------------------------------------------
output$selectedSamples <- renderText({
    "PENDING"
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
