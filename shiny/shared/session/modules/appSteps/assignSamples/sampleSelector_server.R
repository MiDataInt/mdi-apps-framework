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
allAssignments <- reactive({ # all assignments for the selected sample set (not just the selected samples)
    req(input$sampleSet)
    x <- getSampleSetAssignments(input$sampleSet)
    x[, uniqueId := paste(Project, Sample_ID, sep = ":")]
    x
})

#----------------------------------------------------------------------
# open a dialog to enable sample selection from among the samples in the selected set
#----------------------------------------------------------------------
selectedSamples <- reactiveVal(list()) # logical, TRUE if selected, NULL if not selected
observeEvent(input$selectSamples, {
    allAssignments <- allAssignments()
    req(allAssignments)
    selectedSamples <- selectedSamples()

    # collect sample grid metadata
    nCol <- allAssignments[, max(Category1)]
    nRow <- allAssignments[, max(Category2)]
    colWidth <- floor(12 / (nCol + 1))
    stepName <- appStepNamesByType$assign
    sampleSet <- app[[stepName]]$outcomes$sampleSets()[[input$sampleSet]]

    # assemble the selection grid
    grid <- fluidRow(
        style = "margin: 0 15px;",
        fluidRow(
            column(width = colWidth, ""),
            lapply(1:nCol, function(col){
                column(
                    style = "border-left: 1px solid grey; text-align: center;",
                    width = colWidth,
                    tags$strong(sampleSet$categoryNames[[1]][col])
                )
            })
        ),
        lapply(1:nRow, function(row){
            fluidRow(
                style = "border-top: 1px solid grey;",
                column( 
                    style = "text-align: right;", 
                    width = colWidth, 
                    tags$strong(sampleSet$categoryNames[[2]][row]) 
                ),
                lapply(1:nCol, function(col){
                    uniqueIds <- allAssignments[Category2 == row & Category1 == col, uniqueId]
                    sampleNames <- getSampleNames(sampleUniqueIds = uniqueIds)
                    nSamples <- length(uniqueIds)
                    column(
                        style = "border-left: 1px solid grey;",
                        width = colWidth,
                        if(nSamples == 0) "-" else lapply(1:nSamples, function(i){
                            id <- paste('selectSample', row, col, i, sep = "-")
                            selected <- selectedSamples[[uniqueIds[i]]]
                            checkboxInput(
                                ns(id), 
                                label = sampleNames[i], 
                                value = if(is.null(selected)) FALSE else selected
                            )
                        })
                    )
                })
            )
        })
    )

    # show grid for sample selection via modal
    showUserDialog(
        "Select Samples", 
        grid, 
        callback = commitSelectedSamples,
        size = if(nCol > 3) "l" else "m"
    )
})

#----------------------------------------------------------------------
# react to user set selection by setting a reactive
#----------------------------------------------------------------------
commitSelectedSamples <- function(parentInput){
    allAssignments <- allAssignments()
    nCol <- allAssignments[, max(Category1)]
    nRow <- allAssignments[, max(Category2)]
    selectedSamples <- selectedSamples()
    for(row in 1:nRow) for (col in 1:nCol){
        uniqueIds <- allAssignments[Category2 == row & Category1 == col, uniqueId]
        nSamples <- length(uniqueIds)
        if(nSamples == 0) next
        for(i in 1:nSamples){ # "sampleSelector-selectSample-1-1-1"
            inputId <- paste('sampleSelector', 'selectSample', row, col, i, sep = "-")
            selectedSamples[[uniqueIds[i]]] <- if(parentInput[[inputId]]) TRUE else NULL
        } 
    }  
    selectedSamples(selectedSamples)
}
selectedAssignments <- reactive({ # the subset of assignments for just the selected samples
    allAssignments <- allAssignments()
    req(allAssignments)
    allAssignments[uniqueId %in% names(selectedSamples())]
})

#----------------------------------------------------------------------
# provide feedback on the selected samples
#----------------------------------------------------------------------
output$selectedSampleCount <- renderText({
    allAssignments <- allAssignments()
    req(allAssignments)
    nSamples <- nrow(allAssignments)    
    nSelected <- length(names(selectedSamples()))
    paste(nSelected, 'of', nSamples, 'samples are selected')
})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
list(
    allAssignments      = allAssignments, # all assignments for the selected sample set
    selectedAssignments = selectedAssignments, # the subset of assignments for the selected samples
    selectedSamples     = reactive({ names(selectedSamples()) }), # unique IDs (Project:Sample_ID) for the selected samples # nolint
    input = input
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
