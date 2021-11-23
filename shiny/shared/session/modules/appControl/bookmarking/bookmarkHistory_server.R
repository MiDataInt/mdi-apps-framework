#----------------------------------------------------------------------
# reactive components for loading a user's cached, recent bookmarks
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
bookmarkHistoryServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id)
        module <- ns('bookmarkHistory') # for reportProgress tracing

#----------------------------------------------------------------------
# initialize the widget
#----------------------------------------------------------------------
list <- historyListServer(
    id = 'list',
    parentId = id,
    namespace = CONSTANTS$bookmarkKey,
    dataTable = data.table(
        App = character(),
        Analysis_Set = character(),
        stringsAsFactors = FALSE
    ),   
    maxN = 100,
    action = "Load",
    actionFn = loadBookmarkFromString,
    uniqueByContents = TRUE,
    uniqueFn = function(dt) {
        dt[Analysis_Set != CONSTANTS$autoSavedBookmark | !duplicated(Analysis_Set)]
    }
)

#----------------------------------------------------------------------
# set and get history items
#----------------------------------------------------------------------
set <- function(json=NULL, file=NULL, name=NULL){
    firstStepName <- names(app$config$appSteps)[1]     
    if(is.null(json)) {
        if(is.null(file)) {
            json <- getBookmarkJson()
        } else {
            json <- loadResourceText(file)  
            name <- unserializeJSON(json)$outcomes[[firstStepName]]$analysisSetName
        }
    }
    if(is.null(json)) return(NULL)
    list$set(
        item = list(    
            App          = app$NAME,
            Analysis_Set = if(is.null(name)) app[[firstStepName]]$outcomes$analysisSetName() else name    
        ),
        data = json
    )
}

#----------------------------------------------------------------------
# return values
#----------------------------------------------------------------------
list(
    list = list,
    set = set
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
