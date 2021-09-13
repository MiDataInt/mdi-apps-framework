
#----------------------------------------------------------------------
# reactive components for app state save-and-recover tools
# NB: this implementation is better for our environment than native R Shiny bookmarking
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
bookmarkingServer <- function(id, locks) {
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

#----------------------------------------------------------------------
# download a MAGC (.magc) bookmark file
#----------------------------------------------------------------------
output[[id]] <- downloadHandler(
    filename = function() {
        firstStepName <- names(app$info$appSteps)[1]
        filename <- app[[firstStepName]]$outcomes$analysisSetName
        filename <- if(is.null(filename)) app$info$name else filename()
        filename <- gsub(' ', '_', filename)
        paste(filename, "magc", sep = ".")
    },
    content = function(file) {
        reportProgress('bookmarking download', module)
        json <- getBookmarkJson()
        bookmarkHistory$set(json = json)
        write(json, file)
    }
)

#----------------------------------------------------------------------
# upload a MAGC (.magc) bookmark file (data$file set by upload handler)
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

