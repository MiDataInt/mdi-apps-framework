#----------------------------------------------------------------------
# static components for constructing a modal panel to add or create MDI tool suites and apps
#----------------------------------------------------------------------
addMdiToolsUI <- function(
    id
){
    ns <- NS(id)
    if(serverEnv$IS_SERVER) return("")

    # all users: add a tool suite
    addToolSuite <- tagList(
        tags$p(tags$strong("Add a tool suite to this MDI installation")),
        tags$p(
            "Paste the GitHub URL for the desired tool suite repository below and click 'Add Suite'."
        ),
        tags$p(
            "Browse the ",
            tags$a(
                href = "https://midataint.github.io/docs/registry/by_provider/", 
                target = "Docs", 
                "Tool Suite Registry"
            ),
            "to find and copy the required URL. ",
            "Use the 'Code' button on GitHub for unregistered repositories."
        ),
        fluidRow(
            column(
                width = 10,
                textInput(ns("githubUrl"), "GitHub Repository URL", width = "100%")
            ),
            column(
                width = 2,
                style = "margin-top: 24px; padding: 0",
                bsButton(ns("addToolSuite"), "Add Suite", style = "success")
            )
        ),
        fluidRow(column(
            width = 12,            
            id = ns("addSuiteError"),
            style = "margin-top: 5px; color: rgb(200, 0, 0);"
        ))
    )
    if(!serverEnv$IS_DEVELOPER) return(addToolSuite)

    # additional actions for developers
    fluidRow(tabBox(
        id = "developerTabs",
        width = 12,

        # create a tool suite        
        tabPanel(
            "Create Tool Suite",
            tags$p(tags$strong("Create a new MDI tool suite on GitHub")),
            tags$p(
                tags$strong(tags$a(
                    href = "https://github.com/MiDataInt/mdi-suite-template/generate", 
                    target = "GitHub", 
                    ">> Click here << "
                )),
                "to create a new MDI tool suite by copying the",
                tags$a(
                    href = "https://github.com/MiDataInt/mdi-suite-template", 
                    target = "GitHub", 
                    "suite repository template"
                ),
                " (if you get a page error, you need to sign in to GitHub)."
            ),
            tags$p(
                "Then, use the 'Add Tools Suite' and 'Add Tool to Suite' tabs ",
                "to add your new suite to this MDI installation and begin developing pipelines and apps."
            )            
        ),

        # add a tool suite
        tabPanel(
            "Add Tool Suite",
            addToolSuite
        ),

        # add app or pipeline
        tabPanel(
            "Add Tool to Suite",
            tags$p(tags$strong("Create a new pipeline or app in an installed tool suite")),
            tags$p(
                "Select the target tool suite, enter a name and description for your new tool, ",
                "and click the Add Pipeline or App button to create it from a minimal template."
            ),
            fluidRow(
                column(
                    width = 8,
                    selectInput(ns("toolSuite"), "Tool Suite", choices = "", width = "100%")
                ),
                column(
                    width = 4,
                    textInput(ns("toolName"), "Tool Name", width = "100%")
                )
            ),
            fluidRow(column(
                width = 12,
                textInput(ns("toolDescription"), "Description", width = "100%")   
            )),
            fluidRow(column(
                width = 12,
                style = "margin-top: 20px;",
                bsButton(ns("addPipeline"), "Add Pipeline", style = "success"),
                tags$div(style = "display: inline-block; width: 10px"),
                bsButton(ns("addApp"), "Add App", style = "success")      
            )),
            fluidRow(column(
                width = 12,            
                id = ns("addToolError"),
                style = "margin-top: 5px; color: rgb(200, 0, 0);"
            ))
        ),

        # add components to tools
        if(is.null(app$DIRECTORY)) NULL else tabPanel(
            "Add to App",
            tags$p(tags$strong(
                "Add an app step or widget to the '",
                app$NAME,
                "' app"
            )),
            tags$p(
                "Select the component type, whether it is a shared component, ",
                "enter a name, and click Add Component."
            ),
            fluidRow(
                column(
                    width = 4,
                    selectInput(ns("componentType"), "Component Type", 
                                choices = c("App Step" = "appStep", "Widget" = "widget"), 
                                width = "100%")
                ),
                column(
                    width = 4,
                    textInput(ns("componentName"), "Name", width = "100%")
                ),
                column(
                    width = 4,
                    tags$div(
                        style = "margin-top: 30px;",
                        checkboxInput(ns("sharedComponent"), "Shared", width = "100%")
                    )
                )
            ),
            fluidRow(column(
                width = 12,
                style = "margin-top: 10px;",
                textOutput(ns("componentDir"))
            )),
            fluidRow(column(
                width = 12,
                style = "margin-top: 20px;",
                bsButton(ns("addComponent"), "Add Component", style = "success")
            ))
        )
    ))
}
