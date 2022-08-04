#----------------------------------------------------------------------
# static components used to build executable analyses from sample sets
# the module is generic and can support any analysis type, as requested in options
#----------------------------------------------------------------------
# module follows the archetype of upper edit form + lower summary list
#----------------------------------------------------------------------

# module ui function
runAnalysesUI <- function(id, options) {
    
    # initialize namespace
    ns <- NS(id)
    
    # override missing options to defaults
    options <- setDefaultOptions(options, stepModuleInfo$runAnalyses)
    
    # incorporate options text into templates
    leaderText <- tagList(
        tags$p(HTML("
Use the controls to <strong>choose samples</strong> and <strong>set options</strong>
for every analysis you wish to run. After you <strong>save</strong> your analysis
schema, click <strong>Launch Job</strong> to execute it.")),
        tags$ul(
            tags$li(HTML("You may assign a name to each analysis")),
            tags$li("Click on a saved analysis to edit it (if it has not been run yet)"),
            tags$li(HTML("Click <strong>remove</strong> to permanently delete an analysis and all of its files"))
        )
    )    

    # return the UI contents
    standardSequentialTabItem(options$longLabel, leaderText,
        fluidRow( box(width = 12,
            title = textOutput(ns('boxTitle')),
            status = 'primary',
            solidHeader = TRUE,
            fluidRow(
                # top level options selectors for sampleSetId and analysisType
                column(width = 6,           
                    uiOutput(ns('universalOptions')),
                ),
                
                # analysis schema controls and save action
                column(width = 4,           
                    bsButton(ns("saveRecord"), "Save Analysis Schema", style = "success", class = "margin-5"),
                    actionLink(ns("resetEditPanel"), ' Reset Form'),
                    uiOutput(ns('saveRecordFeedback'))
                )                       
            ),
            fluidRow(
                # top level options selectors for sampleSetId and analysisType
                column(width = 12,           
                    uiOutput(ns('specificOptions')),
                )
            )
        )),

        # table of analysis schema with Launch Job inputs
        conditionalPanel( paste0("window['", ns('schema-count'), "'] > 0"),
            summaryTableUI(ns('schema'), 'Analysis Schema', width = 12)
        )
    )    
}
