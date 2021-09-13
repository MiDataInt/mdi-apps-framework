
----------------------------------------------------------------
 Shiny shared modules/components folder organization
----------------------------------------------------------------

This **shiny/shared** folder has the code that creates the app
framework, with components that can be called by analysis apps.

**global.R**, **ui.R** and **server.R** are called by 
<code>shiny::runApp()</code>, as for any R Shiny app. See Shiny
documentation for more information. If these scripts need to change,
servers generally need to be restarted.

The **server folder** has scripts that are sourced by server.R
to initialize the session and contruct the launch page (they
are broken into pieces in this folder for clarity).

The **global folder** has scripts that define utility functions
that do not depend on a specific app or any user session data.
They are sourced prior to initialization of a specific
app and must not depend on <code>app</code> or other session, 
variables unless those variables are passed as function arguments.

The **session folder** has scripts that define functions that are
potentially specific to a user session or only needed to assemble
and serve a specific app. They are sourced after a user commits to
a specific app and therefore have <code>app</code> and other session 
variables in their scope without passing them as arguments.

Scripts in both **global** and **session** folders are sourced at the
beginning of a user session by <code>server.R</code> so contents are
renewed in the R environment of every specific user session, i.e.
in <code>sessionEnv</code>. This enables hotfixes and rolling updates
without server restart.

The **optional** folder is similar to **session** except that, unlike
the latter, scripts in the optional folder are not sourced unless
specifically requested by an app via a 'require' directive in its
<code>config.yml</code> files. Thus, scripts in the session folder are 
generally required for use by the Portal framework, whereas scripts in 
the optional folder might only be used by a subset of apps. Otherwise, 
the optional and session folders have a similar structure, scope and
purpose.

The **static** folder has fixed content for populating the main
framework pages with text, mostly via markdown rendered in R with
<code>includeMarkdown(file.path('static/xxx.md'))</code>.

**www** carries scripts used by the user agent to run the page,
i.e css and js.

----------------------------------------------------------------
Categories of recurring elements used to support apps
----------------------------------------------------------------

Within the **global**, **session** and **optional** folders:

**classes** are R Shiny S3 classes that define resuable data objects
for use by developers writing Portal code. Their general purpose
is to encapsulate methods and other common logics. Classes are
never used to access or create the UI.

**modules** are R Shiny modules that define resuable UI components
and associated server logic for app steps, widgets, etc.
Specifically:

    appControl      control overall framework bevhavior
    appSteps        common analysis steps used by apps, e.g. to load files
    developerTools  UI components only added during Portal development
    globus          functions that support user login via the Globus service
    widgets         UI components to embed on pages, e.g. plot boxes

**packages** holds files that declare and load the various R packages
that are used to implement the framework and handle data.

**types** define data classes that help structure app contents.
Specifically:

    analysisTypes   common ways that data may be analyzed across apps
    manifestTypes   the file formats/types by which providers deliver metadata  

**ui** scripts offer functions to help assemble page UIs. 

**utilities** scripts offer functions to get or set various data values.

Note that **modules**, **types**, **ui** and **utilities** folders can
be created within app folders also, to only be loaded with that specific 
app. However, whenever possible, it is desirable to abstract modules
and types to be reusable in other/future apps.

