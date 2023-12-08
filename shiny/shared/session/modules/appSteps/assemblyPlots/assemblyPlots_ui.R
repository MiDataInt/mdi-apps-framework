#----------------------------------------------------------------------
# UI components for the assemblyPlots appStep module
#----------------------------------------------------------------------

# module ui function
assemblyPlotsUI <- function(id, options) {

    # initialize namespace
    ns <- NS(id)
    
    # override missing options to module defaults
    options <- setDefaultOptions(options, stepModuleInfo$assemblyPlots)
    assemblyOptions <- getAssemblyTypeOptions(options)

    # bucket and static plot box
    assemblyPlotBoxUI <- function(id, title, collapsed = TRUE, plotBoxUI = "staticPlotBoxUI"){
        if(is.null(collapsed)) collapsed <- TRUE
        if(is.null(plotBoxUI)) plotBoxUI <- "staticPlotBoxUI"
        fluidRow(
            box(
                width = 12,
                title = title,
                collapsible = TRUE,
                collapsed = collapsed,
                fluidRow(
                    column(
                        width = 6,
                        style = "padding: 0;",
                        uiOutput(ns(paste("conditions", id, sep = "_"))),
                        uiOutput(ns(paste("groups", id, sep = "_")))
                    ),
                    get(plotBoxUI)(
                        ns(paste0(id, "Plot")), 
                        title = "",
                        width = 6,
                        collapsible = TRUE,
                        collapsed = collapsed
                    )
                )
            )
        )
    }

    # return the UI contents
    standardSequentialTabItem(

        # page header text
        options$longLabel,
        options$leaderText,

        # page header links, uncomment as needed
        id = id,
        # documentation = TRUE,
        # terminal = TRUE,
        console = serverEnv$IS_DEVELOPER,
        code = serverEnv$IS_DEVELOPER,
        settings = TRUE,

        # saving figures
        fluidRow(
            bufferedTableUI(
                ns("savedPlotSets"), 
                title = "Saved Plot Assemblies", 
                width = 10,
                collapsible = TRUE,
                collapsed = FALSE,
                downloadable = TRUE 
            ),
            column(
                width = 2,
                bsButton(
                    ns("savePlotSet"),
                    "Save Plots",
                    style = "success",
                    block = TRUE
                )
            ),
            column(
                width = 12,
                uiOutput(ns('savePlotSetFeedback'))
            )
        ),

        # assembly selection and display of all its samples
        dataSourceTableUI(
            ns("dataSource"), 
            "Assembly Set", 
            width = 12, 
            collapsible = TRUE
        ),
        fluidRow(
            bufferedTableUI(
                ns("allSamples"),
                title = "All Assembled Samples", 
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                downloadable = TRUE
            )
        ),

        # selection of columns and values to plot
        fluidRow(
            box(
                title = "Plot Settings", 
                width = 12,
                tags$div(
                    style = "padding: 10px;",
                    tags$div(
                        width = 3,
                        bsButton(
                            ns("suspendDataProcessing"),
                            "Allow Data Processing",
                            style = "warning",
                            block = TRUE,
                            type = "toggle",
                            value = TRUE
                        )                       
                    ),
                    checkboxGroupInput(
                        ns("Group_By"),
                        "Group By",
                        choices = NULL,
                        selected = NULL,
                        inline = TRUE,
                        width = "100%"
                    ),
                    checkboxGroupInput(
                        ns("Required"),
                        "Required",
                        choices = NULL,
                        selected = NULL,
                        inline = TRUE,
                        width = "100%"
                    ),
                    checkboxGroupInput(
                        ns("Prohibited"),
                        "Prohibited",
                        choices = NULL,
                        selected = NULL,
                        inline = TRUE,
                        width = "100%"
                    ),
                    if(is.null(assemblyOptions$dataTypeColumns)) NULL else checkboxGroupInput(
                        ns("Data_Types"), # optional data types used to filter data groupings
                        "Data_Types",
                        choices = names(assemblyOptions$dataTypeColumns),
                        selected = names(assemblyOptions$dataTypeColumns)[unlist(assemblyOptions$dataTypeColumns)],
                        inline = TRUE,
                        width = "100%"
                    ),
                    checkboxGroupInput(
                        ns("Projects"),
                        "Projects",
                        choices = character(),
                        inline = TRUE,
                        width = "100%"
                    )
                )
            )
        ), 

        # tabular views of the matching samples and groups
        fluidRow(
            bufferedTableUI(
                ns("groupedProjectSamples"), 
                title = "Matching Samples", 
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                downloadable = TRUE
            )
        ),
        fluidRow(
            bufferedTableUI(
                ns("groups"), 
                title = "Groups to Plot", 
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                downloadable = TRUE
            )
        ),

        # output plots
        lapply(names(assemblyOptions$plotTypes), function(id){
            pt <- assemblyOptions$plotTypes[[id]]
            assemblyPlotBoxUI(id, pt$label, collapsed = pt$collapsed, plotBoxUI = pt$plotBoxUI)
        }),
        NULL
    )
}
