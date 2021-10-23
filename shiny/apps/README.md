----------------------------------------------------------------
 Individual Shiny app folder contents
----------------------------------------------------------------

Each apps folder has files that define a single app (i.e. one
Shiny data analysis interface). The folder for an app carries 
all scripts that define its specific behavior.

At the root level of the app, these must include:

    config.yml   names and describes the app structure
    server.R     contains the function 'appServer'
    overview.md  text used to describe the app on its splash page

Optionally, you can organize additional app files into the
following sub-folders, which will be loaded automatically along
with config.yml and server.R:

    modules
    types
    ui
    utilities 

----------------------------------------------------------------
 Individual Shiny app folder contents
----------------------------------------------------------------

The easiest way to get going is to copy and modify the '_template'
app, which provides a working boilerplate for all required code.
