#----------------------------------------------------------------------
# static components to select samples from a single list of all samples
#----------------------------------------------------------------------

# module ui function
selectSamplesUI <- function(id, options) {

    # initialize namespace
    ns <- NS(id)
    
    # override missing options to module defaults
    options <- setDefaultOptions(options, stepModuleInfo$selectSamples)

    # return the UI contents
    standardSequentialTabItem(

        # page header text
        options$longLabel,
        options$leaderText,

        # page header links, uncomment as needed
        id = id,
        documentation = serverEnv$IS_DEVELOPER,
        terminal = FALSE,
        console = serverEnv$IS_DEVELOPER,
        code = serverEnv$IS_DEVELOPER,
        settings = FALSE,

        # analysis set name
        tags$p(
            "Give this analysis set a short, useful name. ",
            "Then click available sample rows to add them to the selection list and rename as needed."
        ) ,      
        fluidRow(
            box(
                title = 'Analysis Set Name',
                status = 'primary',
                solidHeader = TRUE,
                width = 4,
                textInput(ns('analysisSetName'), NULL, paste(app$config$name, Sys.Date(), sep = "."))
            )
        ),

        # sample boxes
        fluidRow(

            # table of all selected samples with "Remove" buttons and name edit boxes
            summaryTableUI(
                id = ns("selectedSamples"), 
                title = "Selected Samples", 
                width = options$selectedWidth, 
                collapsible = TRUE, 
                skipFluidRow = TRUE
            ),

            # table of all available samples for making selections
            bufferedTableBoxUI(
                id = ns("availableSamples"),
                title = "Available Samples",
                #----------------------------
                reload = TRUE,
                download = TRUE,
                #----------------------------
                width = options$availableWidth,
                solidHeader = TRUE,
                status = 'primary',
                collapsible = TRUE,
                collapsed = FALSE
            )
        )
    )
}
