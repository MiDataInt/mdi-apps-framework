#----------------------------------------------------------------------
# reactive components for constructing an shinyFiles path browser, suitable for embedding in a modal
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
serverFileSelectorServer <- function(
    id,
    extensions = character()
){
    moduleServer(id, function(input, output, session){
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize the module
#----------------------------------------------------------------------
module <- "serverFileSelector"
paths <- getAuthorizedServerPaths("read")
currentPath <- reactiveVal(list(
    basePathName = character(),
    relativePath = character()
))
selectedFile <- reactiveVal(NULL)

#----------------------------------------------------------------------
# handle the basePath selectInput
#----------------------------------------------------------------------
observeEvent(input$basePath, {
    req(input$basePath)
    currentPath(list(
        basePathName = input$basePath,
        relativePath = "",
        subDirs = character()
    )) 
})

#----------------------------------------------------------------------
# the current file path with clickable folders
#----------------------------------------------------------------------
output$currentPath <- renderUI({
    cp <- currentPath()
    req(cp, cp$basePathName)
    nSubDirs <- length(cp$subDirs) 
    elements <- list(tags$span(
        tags$a(
            onclick = paste0('handleActionClick3("serverFileSelectorCurrentDirBasepathClick", ', 0, ')'),
            style = "cursor: pointer;",
            cp$basePathName
        ) 
    ))
    if(nSubDirs > 0) for(i in 1:nSubDirs) elements <- c(
        elements,
        list(
            tags$span(" / "),
            tags$a(
                onclick = paste0('handleActionClick3("serverFileSelectorCurrentDirSubfolderClick", ', i, ')'),
                style = "cursor: pointer;",
                cp$subDirs[i]
            ) 
        )
    )
    tagList(elements)
})
addMdiSharedEventHandler("serverFileSelectorCurrentDirBasepathClick", function(...){
    cp <- currentPath()
    req(cp)
    cp$relativePath <- ""
    cp$subDirs <- character()
    currentPath(cp)
})
addMdiSharedEventHandler("serverFileSelectorCurrentDirSubfolderClick", function(i){
    cp <- currentPath()
    req(cp, length(cp$subDirs) > i)
    cp$subDirs <- cp$subDirs[1:i]  
    cp$relativePath <- paste(cp$subDirs, collapse = "/") 
    currentPath(cp)
})

#----------------------------------------------------------------------
# contents of the current file path with actions
#----------------------------------------------------------------------
output$currentPathSubDirs <- renderUI({
    cp <- currentPath()
    req(cp, cp$basePathName)
    path <- file.path(paths[cp$basePathName], cp$relativePath)
    subDirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
    subDirs <- subDirs[!startsWith(subDirs, ".")]
    if(length(subDirs) == 0) "" else tagList(lapply(subDirs, function(subDir) tags$div(tags$a(
        onclick = paste0('handleActionClick3("serverFileSelectorSubDirClick", "', subDir, '")'),
        style = "cursor: pointer;",
        paste0("/", subDir)
    ))))
})
addMdiSharedEventHandler("serverFileSelectorSubDirClick", function(subDir){
    cp <- currentPath()
    cp$relativePath <- paste0(cp$relativePath, "/", subDir) 
    cp$subDirs <- c(cp$subDirs, subDir)
    currentPath(cp)
})
output$currentPathFiles <- renderUI({
    cp <- currentPath()
    req(cp, cp$basePathName)
    path <- file.path(paths[cp$basePathName], cp$relativePath)
    files <- list.files(path, include.dirs = FALSE) # include.dirs option not working...
    subDirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
    files <- files[!(files %in% subDirs)]
    if(length(files) > 0 && length(extensions) > 0) files <- {
        I <- sapply(files, function(file) any(endsWith(file, paste0(".", extensions))))
        files[I]
    }
    if(length(files) == 0) "" else tagList(lapply(files, function(file) tags$div(tags$a(
        onclick = paste0('handleActionClick3("serverFileSelectorFileClick", "', file, '")'),
        style = "cursor: pointer;",
        file
    ))))
})
addMdiSharedEventHandler("serverFileSelectorFileClick", function(file){
    cp <- currentPath()
    filePath <- file.path(paths[cp$basePath], cp$relativePath, file)
    selectedFile(filePath)
})

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
# sourceErrorType # required, otherwise lazy evaluation won't propagate value into onDestroy
list(
    selectedFile = selectedFile
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
