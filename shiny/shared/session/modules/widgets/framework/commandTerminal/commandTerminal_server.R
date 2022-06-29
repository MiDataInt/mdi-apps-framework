#----------------------------------------------------------------------
# reactive components for populating a command terminal emulator dialog
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
commandTerminalServer <- function(id, user = NULL, dir = NULL, results = "",
                                  timeout = 10, onExit = NULL) {
    moduleServer(id, function(input, output, session) {
#----------------------------------------------------------------------
if(serverEnv$IS_SERVER) return(NULL)

#----------------------------------------------------------------------
# initialize the terminal
#----------------------------------------------------------------------
module <- "commandTerminal"
chooseDirId <- "chooseDir"
spinnerSelector <- "#commandTerminalSpinner"
prefix <- session$ns("") # for passing to javascript
observers <- list() # for module self-destruction
workingDir <- reactiveVal(dir)
defaultTimeout <- 10
timeout_ <- reactive({ # since we have no way to pass SIGINT to synchronous system()
    x <- input$timeout
    if(is.null(x)) return(timeout)
    x <- trimws(x)
    if(x == "" || x == "0") return(timeout)
    as.integer(x)
})

#----------------------------------------------------------------------
# initialize the command prompt
#----------------------------------------------------------------------
output$prompt <- renderUI({
    domain <- serverEnv$MDI_REMOTE_DOMAIN
    dir <- workingDir()
    prompt <- tags$span(if(is.null(domain)) user else paste(user, domain, sep = "@"), style = "color: #500;")
    prompt <- if(is.null(dir)) prompt else paste(prompt, tags$span(dir, style = "color: #00a;"), sep = " ")
    tagList(
        HTML(paste0("[", prompt, "]", "$")),
        serverChooseDirIconUI(session$ns(chooseDirId), class = "mdi-dir-icon")
    )
})
serverChooseDirIconServer(
    chooseDirId, 
    input, 
    session,
    chooseFn = function(dir) changeTerminalDirectory(dir$dir, workingDir, prefix)
)

#----------------------------------------------------------------------
# execute the command in response to enter key or Execute button click
#----------------------------------------------------------------------
doCommand <- reactiveVal(0)
observers$commandEnterKey <- observeEvent(input$commandEnterKey, {
    doCommand(doCommand() + 1) 
})
observers$execute <- observeEvent(input$execute, {
    doCommand(doCommand() + 1) 
})
observers$doCommand <- observeEvent(doCommand(), {
    req(doCommand() > 0)
    req(input$command)
    dir <- workingDir()
    req(dir)
    command <- interceptTerminalCommands(input$command, workingDir = workingDir, 
                                         prefix = prefix, onExit = onExit)
    req(command)
    show(selector = spinnerSelector)    
    systemCommand <- paste0("cd '", dir, "'; ", command)
    if(serverEnv$IS_WINDOWS) {
        drive <- strsplit(dir, "")[[1]][1]
        systemCommand <- gsub(
            paste0(drive, ":/"), # convert "C:/" to "/mnt/c/", et.
            paste0("/mnt/", tolower(drive), "/"),
            paste0('bash -c "', systemCommand, '"') # requires Git Bash on Windows
        )
    }
    x <- c(
        results(), 
        paste0('<span style="color: #00a;">', paste("$", command), '</span>'),
        tryCatch({
            system(systemCommand, intern = TRUE, timeout = timeout_())
        }, warning = function(w){ # when command executes but it reports an error
            if(grepl("timed out after", w)) paste("command timed out after", timeout_(), "seconds")
            else system(paste(systemCommand, "2>&1"), intern = TRUE)
        }, error = function(e){c( # when the command could not be executed and the system reports an error
            "unrecognized or malformed command",
            "could not be executed on the server operating system"
        )}) 
    )
    hide(selector = spinnerSelector)
    addCommandToHistory(prefix)
    results(x)
})

#----------------------------------------------------------------------
# display the command results in a concatenated pseudo-stream
#----------------------------------------------------------------------
results <- reactiveVal(results)
observers$results <- observeEvent(results(), {
    results <- results()
    if(length(results) == 1) results <- NULL 
    html("results", html = paste(results[2:length(results)], collapse = "\n"))
    scrollCommandTerminalResults(prefix)
})
activateObserver <- observe({ # runs once after UI elements initialize
    results()
    runjs(paste0("activateCommandTerminalKeys('", prefix, "')"))
    scrollCommandTerminalResults(prefix) # unfortunately, this executes but something else steals focus after
    activateObserver$destroy()
})

#----------------------------------------------------------------------
# clear the results window
#----------------------------------------------------------------------
observers$clear <- observeEvent(input$clear, { results("") })

#----------------------------------------------------------------------
# return value for use by destroyModuleObservers
#----------------------------------------------------------------------
list(
    observers = observers,
    onDestroy = function() list(
        dir = workingDir(),
        results = results()
    )
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
