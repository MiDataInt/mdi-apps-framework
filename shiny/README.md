-------------------------------------------------------------
Launching the MDI web server
-------------------------------------------------------------

Script <code>run_server.R</code> is the top-level script that launches
the R Shiny web server that serves the MDI Stage 2 apps.
Any method that sources <code>run_server.R</code> into an R environment
will launch the web server. However, we recommend always using
<code>mdi::run()</code> or 
<code>mdi::develop()</code>.

-------------------------------------------------------------
Folder structure
-------------------------------------------------------------

The **apps** folder has one terminal folder for each Shiny app
These are organized into app families defined by the subfolders.
Most actual app definitions are found in separate repositories.

The **shared** folder has common scripts that are made implicitly
available to all apps. It is the root Shiny folder of the running
server, and accordingly holds <code>ui.R</code> and
<code>server.R</code> scripts.
