#----------------------------------------------------------------------
# static components for a sample assignment grid with up to two category levels
#       e.g. category1=group/source and category2=condition/type etc.
# module is flexible to allow only 1 category level (e.g. just groups)
# or just one big bucket (zero categories) for sample selection without assignment
#----------------------------------------------------------------------
# module follows the archetype of upper edit panel + lower summary table
#----------------------------------------------------------------------

# module ui function
assignSamplesUI <- function(id, options) {
    
    # initialize namespace
    ns <- NS(id)    

    # customize based on the number of requested groups and conditions
    parseNLevels <- function(nLevels){
        if(is.character(nLevels)) eval(parse(text = nLevels)) else nLevels
    }
    isCategory <- c(FALSE, FALSE)
    nLevelChoices <- c(1, 1)
    isLevelsSelector <- c(FALSE, FALSE)
    for(i in 1:2){
        isCategory[i] <- !is.null(options$categories[[i]])
        if(isCategory[i]){
            options$categories[[i]]$nLevels <- parseNLevels(options$categories[[i]]$nLevels)
            nLevelChoices[i] <- length(options$categories[[i]]$nLevels)
            isLevelsSelector[i] <- nLevelChoices[i] > 1
        }
    }

    # override missing options to defaults
    leaderTail <- "is up to you and your experimental design; examples might include genotypes, treatment conditions, or tissue sources." # nolint
    stepModuleInfo$assignSamples$defaultLeaderText <- if(isLevelsSelector[2]) paste(
            "Please <strong>drag and drop</strong> your samples as needed to create a table of <strong>",
            options$categories[[1]]$plural, "</strong> and <strong>",
            options$categories[[2]]$plural, "</strong>. What comprises a",
            options$categories[[1]]$singular, "or a",
            options$categories[[2]]$singular, leaderTail
        ) else if(isLevelsSelector[1]) paste(
            "Please <strong>drag and drop</strong> your samples as needed to assign them to <strong>",
            options$categories[[1]]$plural, "</strong>. What comprises a",
            options$categories[[1]]$singular, leaderTail
        ) else
            "Please <strong>drag and drop</strong> your samples to select those you would like to include in your analysis." # nolint
    options <- setDefaultOptions(options, stepModuleInfo$assignSamples)

    # incorporate options text into templates
    leaderText <- tagList(
        tags$p(HTML(options$leaderText)),
        tags$ul(
            if(isLevelsSelector[1]){
                tags$li(paste("Use the shaded boxes to give names to each", options$categories[[1]]$singular,
                               if(isLevelsSelector[2]) paste("or", options$categories[[2]]$singular) else ""))
            } else "",
            tags$li(HTML("After you <strong>save your work</strong>, you may assign a name to each Sample Set")),
            tags$li("Click on a saved Sample Set to edit it"),
        )
    )
    
    # function for creating the nLevels selectors, e.g. how many groups the user needs
    nLevelsSelector <- function(categoryN){
        opt <- options$categories[[categoryN]]
        id <- ns(paste0('nLevels', categoryN))
        label <- paste('#', opt$plural)
        if(is.null(opt$default)) opt$default <- min(opt$nLevels)
        if(is.null(opt$step))    opt$step <- 1
        numericInput(id, label, opt$default,
                     min = min(opt$nLevels), max = max(opt$nLevels), step = opt$step)   
    }
    
    # return the UI contents
    controlColWidth <- if(isLevelsSelector[2]) 4 else 6
    standardSequentialTabItem(options$longLabel, leaderText,

        # sample grid controls and save/reset/fill actions
        fluidRow( if(isLevelsSelector[1]){
            box(width = if(isLevelsSelector[2]) 6 else 4, 
                fluidRow(
                    column(width = controlColWidth, nLevelsSelector(1)),
                    if(isLevelsSelector[2]) column(width = controlColWidth, nLevelsSelector(2)) else "",
                    column(width = controlColWidth, selectInput(ns("autofillDelimiter"), label = 'Name Delimiter',
                            c('fixed width', '- (dash)', '_ (underscore)', ': (colon)', ' (whitespace)')))
                ) 
            )} else "",
            box(width = 5,
                bsButton(ns("saveRecord"), "Save Sample Set", style = "success", class = "margin-5"),
                actionLink(ns("resetEditPanel"), 'Reset Grid',    class = "record-actions-link"),
                actionLink(ns("autofillGrid"),   'Autofill Grid', class = "record-actions-link"),
                uiOutput(ns('saveRecordFeedback'))
            )
        ),
        fluidRow(column(width = 12, conditionalPanel( paste0("window['", ns('sampleSets-count'), "'] > 0"),
            summaryTableUI(ns('sampleSets'), 'Sample Sets', width = 12)
        ))),

        # sample selection inputs and sample sets summary table
        fluidRow(
            column(
                width = 3,
                style = "padding: 0;",
                box(
                    width = 12,
                    style = "padding: 0;",
                    title = 'Available Samples',
                    status = 'primary',
                    solidHeader = TRUE,
                    uiOutput(ns('sampleSelector'))
                )                   
            ),
            column(
                width = 9,
                style = "padding: 0;",
                uiOutput(ns('sampleGrid'))
            )
        )
    )    
}
