#----------------------------------------------------------------------
# reactive components for app state save-and-recover tools
# NB: this implementation is better for our environment than native R Shiny bookmarking
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
bookmarkingServer <- function(id, options, locks) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        module <- 'bookmarking' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# define session-level and module-level variables
#----------------------------------------------------------------------
data <- reactiveValues(
    file  = "",
    input = list(),
    settings = list(),
    outcomes = list(),
    locks = list(),
    step  = ""
)
isServer <- !is.null(options$shinyFiles) && options$shinyFiles

#----------------------------------------------------------------------
# download a bookmark file to the local computer
#----------------------------------------------------------------------
writeBookmarkToFile <- function(file) {
    req(file)
    reportProgress('writeBookmarkToFile', module)
    json <- getBookmarkJson()
    bookmarkHistory$set(json = json)
    write(json, file)
}
if(!isServer) output[[id]] <- downloadHandler(
    # filename = function() {
    #     firstStepName <- names(app$config$appSteps)[1]
    #     filename <- app[[firstStepName]]$outcomes$analysisSetName
    #     filename <- if(is.null(filename)) app$config$name else filename()
    #     filename <- gsub(' ', '_', filename)
    #     paste(filename, "mdi", sep = ".")
    # },
    filename = getDefaultBookmarkName,
    content = writeBookmarkToFile
)

#----------------------------------------------------------------------
# save a bookmark file to the server machine
#----------------------------------------------------------------------
if(isServer) serverBookmarkButtonServer(
    id, 
    input, 
    session,
    saveFn = writeBookmarkToFile
)

#----------------------------------------------------------------------
# upload a bookmark file (data$file set by upload handler)
#----------------------------------------------------------------------
observe({
    req(data$file)    
    startSpinner(session, paste(module, 'loadBookmarkFile'))    
    reportProgress(data$file)
    json <- loadResourceText(data$file) # from the file upload widget
    bookmark <- unserializeJSON(json)
    data$input <- bookmark$input # and we fill the contents for consumers
    data$settings <- bookmark$settings
    data$outcomes <- bookmark$outcomes
    data$locks <- bookmark$locks
    data$step  <- bookmark$step
    activateTab(bookmark$step)
})

#----------------------------------------------------------------------
# set return value as reactiveValues
#----------------------------------------------------------------------
data

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
