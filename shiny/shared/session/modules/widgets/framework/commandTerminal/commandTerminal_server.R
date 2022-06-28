#----------------------------------------------------------------------
# reactive components for populating a command terminal dialog
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
commandTerminalServer <- function(id, user = NULL, dir = NULL, timeout = 10) {
    moduleServer(id, function(input, output, session) {
#----------------------------------------------------------------------
if(serverEnv$IS_SERVER) return(NULL)
module <- "commandTerminal"

#----------------------------------------------------------------------
# initialize the terminal
#----------------------------------------------------------------------
workingDir <- reactiveVal(dir)
chooseDirId <- "chooseDir"
prefix <- session$ns("")
spinnerSelector <- "#commandTerminalSpinner"
defaultTimeout <- 10
timeout_ <- reactive({
    x <- input$timeout
    if(is.null(x)) return(timeout)
    x <- trimws(x)
    if(x == "" || x == "0") return(timeout)
    as.integer(x)
})

#----------------------------------------------------------------------
# initialize the command prompt
#----------------------------------------------------------------------
commandEnterKey <- reactiveVal(0)
commandEnterKeyActivated <- FALSE
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
    chooseFn = function(dir) workingDir(dir$dir)
)

#----------------------------------------------------------------------
# execute the command in response to enter key or Execute button click
#----------------------------------------------------------------------
observeEvent(input$commandEnterKey, {
    commandEnterKey(commandEnterKey() + 1)
})
observeEvent({
    input$execute
    commandEnterKey()
}, {
    req(input$command)
    dir <- workingDir()
    req(dir)
    command <- paste0("cd '", dir, "'; ", input$command)
    if(serverEnv$IS_WINDOWS) {
        drive <- strsplit(dir, "")[[1]][1]
        command <- gsub(
            paste0(drive, ":/"), # convert "C:/" to "/mnt/c/", et.
            paste0("/mnt/", tolower(drive), "/"),
            paste0('bash -c "', command, '"') # requires Git Bash on Windows
        )
    }
    show(selector = spinnerSelector)
    x <- c(
        results(), 
        paste0('<span style="color: #00a;">', paste("$", input$command), '</span>'),
        tryCatch({
            system(command, intern = TRUE, timeout = timeout_())
        }, warning = function(w){ # when command executes but it reports an error
            if(grepl("timed out after", w)) paste("command timed out after", timeout_(), "seconds")
            else system(paste(command, "2>&1"), intern = TRUE)
        }, error = function(e){c( # when the command could not be executed and the system reports an error
            "unrecognized or malformed command",
            "could not be executed on the server operating system"
        )}) 
    )
    hide(selector = spinnerSelector)
    runjs(paste0("addCommandToHistory('", prefix, "')"))
    results(x)
})

#----------------------------------------------------------------------
# display the command results in a concatenated pseudo-stream
#----------------------------------------------------------------------
results <- reactiveVal("")
observeEvent(results(), {
    if(!commandEnterKeyActivated) {
        runjs(paste0("activateCommandTerminalKeys('", prefix, "')"))
        commandEnterKeyActivated <<- TRUE
    }
    results <- results()
    if(length(results) == 1) results <- NULL 
    html("results", html = paste(results[2:length(results)], collapse = "\n"))
    runjs(paste0("scrollCommandTerminalResults('", prefix, "')"))
})

#----------------------------------------------------------------------
# clear the results window
#----------------------------------------------------------------------
observeEvent(input$clear, { results("") })

#----------------------------------------------------------------------
# return nothing
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
