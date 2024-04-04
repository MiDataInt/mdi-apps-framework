#----------------------------------------------------------------------
# static components to upload additional sample data files and rename samples
# this is typically the first module of all apps
#----------------------------------------------------------------------

# module ui function
sourceFileUploadUI <- function(id, options) {
    appName <- app$config$name

    # initialize namespace
    ns <- NS(id)
    
    # override missing options to defaults
    options <- setDefaultOptions(options, stepModuleInfo$sourceFileUpload)
    
    # incorporate options text into templates
    leaderText <- tags$p(HTML(paste0(
        "If desired, upload additional ", appName, "-compatible sample data files."
    )))

    # return the UI contents
    standardSequentialTabItem(
        options$longLabel,
        leaderText, 
        id = id,
        documentation = serverEnv$IS_DEVELOPER,
        code = serverEnv$IS_DEVELOPER,
        console = serverEnv$IS_DEVELOPER,
    
        # enable merging additional sample sources into this one
        tags$div(
            class = "text-block",
            sourceFileInputUI(ns('fileInput'), appName = appName),
            verbatimTextOutput(ns("lastLoadedBookmark"))
        ),

        # tables of the sample sources and samples that are uploaded and ready
        conditionalPanel( 
            condition = paste0("window['", ns('sources-count'), "'] > 0"),
            tags$hr(),
            tags$p("Give this analysis set a short, useful name."),
            fluidRow(box(
                title = 'Analysis Set Name',
                status = 'primary',
                solidHeader = TRUE,
                width = 4,
                textInput(ns('analysisSetName'), NULL, paste(appName, Sys.Date(), sep = "."))
            )),
            tags$p(HTML("Click to remove a data source from this analysis set or filter the samples table.")),    
            summaryTableUI(ns('sources'), 'Data Sources', width = 12)
        ), 
        conditionalPanel( 
            condition = paste0("window['", ns('samples-count'), "'] > 0"),
            tags$p(HTML("If desired, edit samples to provide more human-readable names.")),                         
            summaryTableUI(ns('samples'), 'Samples',  width = 12)            
        ) 
    )
}
