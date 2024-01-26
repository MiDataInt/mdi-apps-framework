#----------------------------------------------------------------------
# static components for an interactive, WYSIWYG, plot
#----------------------------------------------------------------------

# module ui function
mdiInteractivePlotUI <- function(id){
    module <- 'mdiInteractivePlot'
    ns <- NS(id)    
    moduleDir <- getWidgetDir(file.path("plots/general", module), framework = TRUE)
    tags$div(

        # outer wrapper attributes
        id = ns("outerWrapper"),
        class = "outerWrapper",

        # embedded styles and scripts
        tags$style(slurpFile(file.path(moduleDir, "widget.css"))),
        tags$script(HTML(slurpFile(file.path(moduleDir, "widget.js")))),    

        # elements in z-stack order (low to high)

        # the plot or image    
        tags$div(
            id = ns("imagePanel"),
            class = "mdiImagePanel",
            tags$image(
                id = ns("image"),
                class = "mdiImage",
                src = ""
            )
        ),

        # crosshairs that track the cursor
        tags$div(
            id = ns("horizontalCrosshair"),
            class = "mdiCrosshair mdiHorizontal"
        ),
        tags$div(
            id = ns("verticalCrosshair"),
            class = "mdiCrosshair mdiVertical"
        ),

        # a brush selection box
        tags$div(
            id = ns("brushBox"),
            class = "mdiBrushBox"
        )
    )   
}
