
----------------------------------------------------------------
 Shiny app folders (i.e. family) organization
----------------------------------------------------------------

The 'shiny/apps' folder, whether in this framework repository
or an apps suite repository, is organized into sub-folders, where 
each sub-folder represents one family of related apps.

Folder 'framework/_template' has a blank app to help with developing
a new app.

----------------------------------------------------------------
 Single Shiny app folder contents
----------------------------------------------------------------

Each terminal folder has files that define a single app (i.e. one
data analysis interface in the Shiny framework). The folder for
an app carries all scripts that define its specific behavior.

At the root level of the app, these must include:

    config.yml   names and describes the app structure
    server.R     contains the function 'appServer'
    overview.md  text used to desribe the app on its splash page

Optionally, you can organize additional app files into the
following sub-folders, which will be loaded automatically along
with config.yml and server.R:

    modules
    types
    ui
    utilities 

