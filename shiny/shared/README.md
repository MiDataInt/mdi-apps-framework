## Shiny apps framework folder organization

This **shiny/shared** folder has the code that creates the app
UI framework, including pre-app launch components as well as 
highly standardized components that can be used by all analysis 
apps.

**global.R**, **ui.R** and **server.R** are called by 
<code>shiny::runApp()</code>, as for any R Shiny app. If these 
scripts need to change, servers generally need to be restarted.

The **server** folder has scripts that are sourced by server.R
to initialize the session and construct the launch page, broken  
into pieces for clarity.

The **global** folder has scripts that define utility functions
that do not depend on a specific app or any user session data.
They are sourced prior to initialization of a specific
app and must not depend on <code>app</code> or other session 
variables, unless those variables are passed as function arguments.

The **session** folder has scripts that define functions that are
potentially specific to a user session or needed to assemble and serve 
an app. They are sourced after a user commits to a specific app and 
therefore have <code>app</code> and other session variables in their 
scope without passing them as arguments. Session components included
in the framework are very broadly applicable to many potential apps, 
such as the common interfaces for file upload, plotting, etc.

Scripts in both the **global** and **session** folders are sourced at 
the beginning of a user session by <code>server.R</code> so contents 
are renewed in the R environment of every user session, i.e.,
in <code>sessionEnv</code>. This enables hotfixes and rolling updates
without server restart, only a page reload.

The **developer** folder is similar to the sessions folder 
except that those components are only loaded when the framework
is running in developer mode. 

The **static** folder has fixed content for populating the main
framework pages with text, mostly via markdown rendered in R with
<code>includeMarkdown(file.path('static/xxx.md'))</code>.

**www** carries scripts used by the user agent to run the page,
i.e., css and js.

## Categories of recurring elements used to support apps

Within the **global** and **session** folders:

**classes** are R Shiny S3 classes that define reusable data objects
for use by developers writing MDI code. Their general purpose
is to encapsulate methods and other common logic. Classes are
never used to access or create the UI.

**modules** are R Shiny modules that define reusable UI components
and associated server logic for app steps, widgets, etc.
Specifically:

- **appControl** - control overall framework behavior
- **appSteps** - common analysis steps used by apps, e.g., to load files
- **developerTools** - UI components only added during MDI development
- **widgets** - UI components to embed on pages, e.g., plot boxes

**packages** holds files that declare and load the various R packages
that are used to implement the framework and handle data.

**types** define data classes that help structure app contents.
Specifically:

- **analysisTypes** - common ways that data may be analyzed across apps
- **manifestTypes** - the file formats/types by which data providers deliver sample metadata  

**ui** scripts offer functions to help assemble page UIs. 

**utilities** scripts offer functions to get or set various data values.

>**modules**, **types**, **ui**, and **utilities** folders can
be created within app folders also, to only be loaded with that specific 
app. However, whenever possible, it is desirable to abstract modules
and types to be reusable in other apps.
