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
        documentation = FALSE,
        terminal = FALSE,
        console = serverEnv$IS_DEVELOPER,
        code = serverEnv$IS_DEVELOPER,
        settings = FALSE,

        # analysis set name
        tags$p(
            "Give this analysis set a short, useful name, ",
            "then click available sample rows to add them to the selection list."
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

        # # table of all selected samples with "Remove" buttons and name edit boxes
        # summaryTableUI(
        #     id = ns("selectedSamples"), 
        #     title = "Selected Samples", 
        #     width = 12, 
        #     collapsible = TRUE
        # ),

        # table of all available samples for making selections
        fluidRow(
            bufferedTableBoxUI(
                id = ns("selectedSamples"),
                title = "Selected Samples",
                #----------------------------
                download = TRUE,
                #----------------------------
                width = options$selectedWidth,
                solidHeader = TRUE,
                status = 'primary',
                collapsible = TRUE,
                collapsed = FALSE
            ),
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
