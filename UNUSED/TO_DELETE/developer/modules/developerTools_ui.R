#----------------------------------------------------------------------
# static components that provide test code execution, file editing and
# git repository management to developers
#----------------------------------------------------------------------
# must never be enabled in server mode, as it allows arbitrary code 
# execution and alteration of files underlying the framework and apps
#----------------------------------------------------------------------

# module ui function
developerToolsUI <- function(id, options) {
    
    # initialize namespace
    ns <- NS(id)

    # return the UI contents    
    fluidRow(
        style = "padding: 0; margin: 0;",
        tabBox(
            width = 12,
            #tabPanel(
            #    title = 'App Config',
            #    value = 'appConfig',
            #    appConfigUI(ns('appConfig'), options)
            #),
            tabPanel(
                title = 'Code Sandbox',
                value = 'sandbox',
                sandboxUI(ns('sandbox'), options)
            ),
            tabPanel(
                title = 'File Editor',
                value = 'fileEditor',
                fileEditorUI(ns('fileEditor'), options)
            )
            # ,
            # tabPanel(
            #     title = 'Git Manager',
            #     value = 'gitManager',
            #     gitManagerUI(ns('gitManager'), options)
            # ),
            # tabPanel(
            #     title = 'Dockerfile',
            #     value = 'dockerfile',
            #     "PENDING"
            # )    
        )
    )
}
