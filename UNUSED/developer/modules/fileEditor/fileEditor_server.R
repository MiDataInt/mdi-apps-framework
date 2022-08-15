
#----------------------------------------------------------------------
# reactive components that provide a code editor for framework files
# used live within the running application framework
#----------------------------------------------------------------------
# must never be enabled in server mode, as it allows arbitrary code execution 
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
fileEditorServer <- function(id, parentId, options) {
    moduleServer(id, function(input, output, session) {
        ns <- NS(id) # in case we create inputs, e.g. via renderUI
        parentNs <- NS(parentId)
        module <- 'fileEditor' # for reportProgress tracing
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# setup module with common results access elements
#----------------------------------------------------------------------
codeEditorId <- paste(parentNs(id), "code-editor", sep="-")
codeEditorContentsId <- paste("code-editor", "contents", sep="-")

#----------------------------------------------------------------------
# initialize file selector tree
#----------------------------------------------------------------------

# store file paths for validating leaf status (i.e. file, not directory)
treeFiles <- character()
baseDir <- serverEnv$SHINY_DIR

# render the file tree
output$fileTree <- shinyTree::renderTree({
    
    # collect and sort the files list
    treeFiles <<- list.files(baseDir, recursive=TRUE)
    x <- strsplit(treeFiles, '/')    
    lenX <- sapply(x, length)    
    maxLen <- max(lenX)
    x <- cbind(lenX==1,as.data.frame(t(sapply(1:length(x), function(i) c(x[[i]], rep(NA, maxLen-lenX[i]))))))
    order <- do.call(order, x)
    x <- x[order,][,2:ncol(x)]
    lenX <- lenX[order]
    rowIs <- 1:nrow(x)
    
    # process the files list is a list of lists compatible with ShinyTree
    # note that it is the names (not the values) of the list that represent the tree content
    parseLevel <- function(rows, col){
        uniqNames <- unique(x[rows,col])
        list <- lapply(uniqNames, function(name){
            rows_ <- which(x[[col]] == name & rowIs %in% rows)
            if(length(rows_)==1 && lenX[rows_]==col) '' # a terminal leaf (i.e. a file)
            else parseLevel(rows_, col + 1) # a node (i.e. a directory)
        })
        names(list) <- uniqNames
        list
    }
    parseLevel(rowIs, 1) # recurse through the file structure
})

# respond to a file tree click
observeEvent(input$fileTree, {
    req(input$fileTree)
    x <- shinyTree::get_selected(input$fileTree)
    req(length(x) == 1) # == 0 when not selected; never >0 since multi-select disabled
    x <- x[[1]]
    x <- paste(c(attr(x,'ancestry'), x), collapse="/") # reassemble the file path relative to tree root
    req(x %in% treeFiles)
    session$sendCustomMessage("setAceCodeContents", list(
        editorId = codeEditorId,
        code = loadResourceText(file.path(baseDir, x))
    ))
})

  
  
#----------------------------------------------------------------------
# initialize code editor
#----------------------------------------------------------------------
session$sendCustomMessage("initializeAceCodeEditor", codeEditorId)

#----------------------------------------------------------------------
# save code in response to user request
#----------------------------------------------------------------------
#observeEvent(input$codeButton,  { executeCode('code') })
#observeEvent(input$plotButton,  { executeCode('plot') })
#observeEvent(input$plotlyButton,{ executeCode('plotly') })
#observeEvent(input$tableButton, { executeCode('table') })
#codeExecutionMode <- NULL
#executeCode <- function(mode){
#    reportProgress(paste(mode, 'button'), module)
#    codeExecutionMode <<- mode
#    session$sendCustomMessage("getAceCodeContents", codeEditorId)
#}

#observeEvent(input[[codeEditorContentsId]], {
#    reportProgress('input[[codeEditorContentsId]]')
#    expr <- tryCatch({
#        parse(text=input[[codeEditorContentsId]])
#    }, error=function(e){
#        safeError(e)
#    })
#         if(codeExecutionMode == 'code')   codeExpr(expr)
#    else if(codeExecutionMode == 'plot')   plotExpr(expr)
#    else if(codeExecutionMode == 'plotly') plotlyExpr(expr)
#    else if(codeExecutionMode == 'table')  tableExpr(expr)      
#})
#
##----------------------------------------------------------------------
## execute useful preformatted code actions in response to link click ...
##----------------------------------------------------------------------
## list objects in the session environment
#observeEvent(input$ls_sessionEnv, {
#    codeExpr({
#        out <- list()
#        for(objectName in ls(sessionEnv)){
#            x <- class(get(objectName, envir=sessionEnv))[1]
#            out[[x]] <- if(is.null(out[[x]])) objectName
#                        else c(out[[x]], objectName)
#        }
#        out
#    })
#})
## show the structure of the jobResults reactive object
#observeEvent(input$str_jobResults, {
#    if(!exists('jobResults')) codeExpr(NULL)
#    else codeExpr(capture.output(str(jobResults())))
#})
#
##----------------------------------------------------------------------
## ... to generate one of several types of user-intended output
##----------------------------------------------------------------------
#
## the output of the code (not including the plot or table)
#codeExpr <- reactiveVal(NULL)
#output$codeOutput <- renderUI({
#    req(codeExpr())
#    tags$pre(
#        style="max-height: 400px; overflow: auto;",
#        tryCatch({
#            paste(collapse="\n", capture.output(eval(codeExpr())))
#        }, warning=function(warning){
#            paste(collapse="\n", warning)
#        }, error=function(error){
#            paste(collapse="\n", error)
#        }) 
#    )    
#})
#
## a static plot generated by the code
#plotExpr <- reactiveVal(NULL)
#output$plotOutput  <- renderPlot({
#    eval(plotExpr())
#})
#
## an interactive plot generated by the code
#plotlyExpr <- reactiveVal(NULL)
#output$plotlyOutput  <- renderPlotly({
#    eval(plotlyExpr())
#    #plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
#})
#
## a DT table of a data frame or matrix generated by the code
#tableExpr <- reactiveVal(NULL)
#output$tableOutput <- renderDT(
#    eval(tableExpr()),
#    options = list(
#        lengthMenu = c(20,100,500),
#        pageLength = 10
#    ),
#    class = "display table-compact-4",
#    escape = FALSE, 
#    selection = 'single', 
#    editable = FALSE, 
#    rownames = TRUE # must be true for editing to work, not sure why (datatables peculiarity)
#)

##----------------------------------------------------------------------
## define bookmarking actions
##----------------------------------------------------------------------
#observe({
#    bm <- getModuleBookmark(id, module, bookmark, locks)
#    req(bm)
#    data$list  <- bm$schema
#    data$selected <- bm$selected 
#})

#----------------------------------------------------------------------
# set return value
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------

