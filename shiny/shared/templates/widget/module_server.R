#----------------------------------------------------------------------
# server components for the __MODULE_NAME__ widget module
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
__MODULE_NAME__Server <- function(id) { 
    moduleServer(id, function(input, output, session) {    
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# initialize module
#----------------------------------------------------------------------
module <- '__MODULE_NAME__'
# settings <- activateMdiHeaderLinks( # uncomment as needed
#     session,
#     url = getDocumentationUrl("path/to/docs/README", domain = "xxx"), # for documentation
#     dir = getAppStepDir(module), # for terminal emulator
#     envir = environment(), # for R console
#     baseDirs = getAppStepDir(module), # for code viewer/editor
#     settings = id, # for step-level settings
#     immediate = TRUE # plus any other arguments passed to settingsServer()
# )

#----------------------------------------------------------------------
# add server code sections as needed
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# set return value, typically NULL or a list of reactives
#----------------------------------------------------------------------
list()

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
