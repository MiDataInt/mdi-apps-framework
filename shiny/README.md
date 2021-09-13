
-------------------------------------------------------------
Launching the Portal server
-------------------------------------------------------------

Script <code>run_portal.R</code> is the top-level script that launches
the R Shiny web server that serves the MiData Portal applications.
Any method that sources <code>run_portal.R</code> into an R environment
will launch the web server. However, we recommend always using
<code>midata.portal::run()</code> or 
<code>midata.portal::develop()</code>.

-------------------------------------------------------------
Folder structure
-------------------------------------------------------------

The **apps** folder has one terminal folder for each Shiny app
These are organized into app families defined by the sub-folders.
Most actual app definitions are found in separate repositories, 
but can use the 'framework/_template' app found here.

The **shared** folder has common scripts that are made implicitly
available to all apps. It is the root Shiny folder of the running
server, and accordingly holds <code>ui.R</code> and
<code>server.R</code> scripts.

