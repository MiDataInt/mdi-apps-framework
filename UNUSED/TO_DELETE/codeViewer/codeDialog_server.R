#----------------------------------------------------------------------
# reactive components for an icon that loads a context-dependent code viewer/editor
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
codeDialogServer <- function(id, baseDirs = NULL, appStep = NULL) {
    moduleServer(id, function(input, output, session) {

# set the baseDirs
# search the app's suite first, then the apps framework
# expect to find modules in a predictable folder structure
frameworkModulesDir <- file.path(serverEnv$SHARED_DIR, "session", "modules")
appModulesDir <- if(is.null(app$DIRECTORY)) frameworkModulesDir 
                    else parseAppDirectory(app$DIRECTORY)$appModulesDir   
setBaseDirs <- function(...){
    baseDirs <<- file.path(appModulesDir, ...)
    if(!dir.exists(baseDirs)) baseDirs <<- file.path(frameworkModulesDir, ...)
}
if(is.null(baseDirs)){ # explicit baseDirs take precedence

    # .../modules/appSteps/<appStep>
    if(!is.null(appStep)) setBaseDirs("appSteps", appStep)

}
if(is.null(baseDirs) || length(baseDirs) == 0 || !dir.exists(baseDirs[1])) return(NULL)

# allow all users to view a module's code, and developers to edit
observeEvent(input$aceEditor, {
    showAceEditor(
        session,
        baseDirs = baseDirs,
        editable = serverEnv$IS_DEVELOPER
    )  
})

#----------------------------------------------------------------------
# return value
#----------------------------------------------------------------------
NULL

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
