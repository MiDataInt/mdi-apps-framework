#----------------------------------------------------------------------
# static components for constructing a modal panel to cold start an app
#----------------------------------------------------------------------
appChooserUI <- function(
    id
){
    ns <- NS(id)
    tagList(
        tags$p(
            "Click 'Launch' to cold-start the desired app with no initial data file ",
            "(you will load data within the app)."
        ),
        if(serverEnv$IS_SERVER) "" else tags$p(
            "If you don't see the app you want, browse the ",
            tags$a(
                href = "https://midataint.github.io/docs/registry/00_index/",  
                target = "Docs",
                "Tool Suite Registry"
            ),
            "  and click the '+' icon above to add the app to your MDI installation."
        ),
        fluidRow(
            style = "padding: 5px;",
            column(
                width = 12,
                DTOutput(ns("table"))
            )
        )
    )
}
