#----------------------------------------------------------------------
# reactive components for a sample assignment grid with up to two category levels
#       e.g. category1=group/source and category2=condition/type etc.
# module is flexible to allow only 1 category level (e.g. just groups)
# or just one big bucket (zero categories) for sample selection without assignment
#----------------------------------------------------------------------
# module follows the archetype of upper edit panel + lower summary table
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# BEGIN MODULE SERVER
#----------------------------------------------------------------------
assignSamplesServer <- function(id, options, bookmark, locks) {
    moduleServer(id, function(input, output, session) {
#----------------------------------------------------------------------
module <- 'assignSamples' # for reportProgress tracing
if(serverEnv$IS_DEVELOPER) activateMdiHeaderLinks(
    session,
    url = getDocumentationUrl("shiny/shared/session/modules/appSteps/assignSamples/README", 
                              framework = TRUE),
    baseDirs = getAppStepDir(module, framework = TRUE),
    envir = environment()
)

#----------------------------------------------------------------------
# define session-level and module-level variables
#----------------------------------------------------------------------

# initialize sampleSets
summaryTemplate <- data.frame(
    Remove      = character(),
    Name        = character(),
    N_Samples   = integer(),    
        stringsAsFactors = FALSE
)
isCategory <- setAssignmentCategories(options)
for(i in 1:2){
    if(isCategory[i]) summaryTemplate[[options$categories[[i]]$plural]] <- character()
}
data <- summaryTableServer(
    id = 'sampleSets', # NOT ns(id) when nesting modules!
    parentId = id,
    stepNumber = options$stepNumber,
    stepLocks = locks[[id]],
    sendFeedback = sendFeedback,
    template = summaryTemplate,
    type = 'shortList',
    remove = list(
        message = "Remove this sample set?",
        name = getSampleSetName
    ),
    names = list(
        get = getSampleSetNames,
        source = id
    ),
    clearLocks = function(sampleSetId) changeLocks(sampleSetId, clearRecordLocks)
)

#----------------------------------------------------------------------
# how to lock and unlock sources from sample sets
#----------------------------------------------------------------------
changeLocks <- function(sampleSetId, lockFn){
    sourceIds <- data$list[[sampleSetId]]$assignments$Source_ID
    lockFn(locks[[options$source]], module, sampleSetId, sourceIds)   
}
   
#----------------------------------------------------------------------
# helper functions for sample drag and drop construction
#----------------------------------------------------------------------

# each set has one target per category1+category2 ...
getSortableGridId <- function(parentId, categoryN1, categoryN2, useNS = TRUE) {
    if(useNS) parentId <- session$ns(parentId)
    paste(parentId, categoryN1, categoryN2, sep = "_")
}

# ... provided as a sortable::rank_list
sampleRankList <- function(parentId, width, categoryN1, categoryN2, initVals){ 
    rankListId <- getSortableGridId(parentId, categoryN1, categoryN2)
    column(width = width, rank_list( 
        "",
        initVals, # non-null when loading a known set to edit it
        rankListId,
        css_id = rankListId,
        options = sortable_options(
            group = session$ns(parentId),
            multiDrag = TRUE 
        ), # all rank_lists are in the same group to allow moving between them
        class = "default-sortable"
    )) 
}

# support for naming category1 and category2 levels via edit boxes
getSampleCategoryClass <- function(categoryN) {
    category <- paste0('category', categoryN)
    paste("sample", category, "name", sep = "-")
}
sampleRowColName <- function(width, categoryN, index, value){
    class <- getSampleCategoryClass(categoryN)
    column(width = width,
        tags$div(class = class, textInput(
            session$ns(paste(class, index, sep = "-")),
            if(categoryN == 1) paste0(options$categories[[categoryN]]$singular, " #", index, " Name") else NULL,
            value
        ))
    ) 
}
setColumnWidth <- function(ep){
    category2NameCount <- if(ep$nLevels[2] > 1) 1 else 0
    ep$columnWidth <- floor(12 / (ep$nLevels[1] + category2NameCount))
    ep
}

#----------------------------------------------------------------------
# construct a drag-and-drop UI grid of category1 (columns) by category2 (rows)
#   top (input) portion of archetypal two-part appStep module
#----------------------------------------------------------------------

# render the grid reactively as input sample list or grid size changes
assignSamplesId <- "assignSamples"
workingId <- NULL # set to a sample set id when editing a previously saved set
updateEditPanel <- reactiveVal(0)
autofillData <- NULL
editPanelData <- reactiveVal(NULL)
observeEvent({
    app[[options$source]]$outcomes$samples() # when new sample data are available
    app[[options$source]]$outcomes$sampleNames() # so that names stay fresh
    updateEditPanel() # when user clicks reset or similar action
    data$selected() # when user clicks an existing sample set for editing
}, {
    reportProgress('initializeEditPanel', module)
    ep <- list()
    selected <- data$selected()

    # collect information on the initial state of the grid
    names <- getSampleNames(makeUnique = TRUE) # ensure that sortable returns a unique name
    spans <- lapply(names, function(name) tags$span(name) )[order(names)]
    ep$boxTitle <- "Selected Samples"
    isCategory <- sapply(1:2, function(i) !is.null(input[[paste0('nLevels', i)]]) )
    if(is.na(selected)){ # a new sample set, blank grid
        ep$nLevels <- sapply(1:2, function(i) if(isCategory[i]) as.integer(input[[paste0('nLevels', i)]]) else 1 )
        ep$nLevels <- ifelse(is.na(ep$nLevels), 1, ep$nLevels) # in case user deletes the number
        if(!is.null(autofillData)){
            ep$initVals      <- autofillData$samples
            ep$categoryNames <- autofillData$categoryNames
            autofillData <<- NULL
        } else {
            ep$initVals <- lapply(1:ep$nLevels[1], function(i) lapply(1:ep$nLevels[2], function(j) list()))
            ep$categoryNames <- lapply(1:2, function(i) rep("", ep$nLevels[i]) )
        }
        workingId <<- NULL       
    } else { # loading an existing set for editing
        sampleSet <- data$list[[selected]]
        ep$nLevels <- sampleSet$nLevels       
        allUIDs <- getSampleUniqueIds()[order(names)]
        ep$initVals <- lapply(1:ep$nLevels[1], function(i) lapply(1:ep$nLevels[2], function(j){
            samples <- sampleSet$assignments[sampleSet$assignments$Category1 == i &
                                             sampleSet$assignments$Category2 == j, ]
            UIDs <- paste(samples$Project, samples$Sample_ID, sep = ":")
            spans[which(allUIDs %in% UIDs)]
        }))
        ep$categoryNames <- sampleSet$categoryNames
        workingId <<- names(data$list)[selected]
        ep$boxTitle <- paste(ep$boxTitle, getSampleSetName(workingId), sep = " - ")
    }
    ep$initValsSource <- spans[spans %notin% unlist(unlist(ep$initVals, recursive = FALSE), recursive = FALSE)]
    
    # size out the grid, i.e., contingency table
    ep <- setColumnWidth(ep)
    
    # send results to our dependents
    ep$force <- sample(1e8, 1)
    editPanelData(ep)
})

# add the drag source with all samples at left
output$sampleSelector <- renderUI({
    reportProgress('output$sampleSelector', module)
    ep <- editPanelData()
    req(ep)
    fluidRow( sampleRankList(assignSamplesId, 12, 0, 0, ep$initValsSource) )
})

# fill the table of drop targets
output$sampleGrid <- renderUI({
    reportProgress('output$sampleGrid', module)
    ep <- editPanelData()
    req(ep)
    selected <- isolate({ data$selected() })
    fluidRow(box(
        width = 12,
        title = ep$boxTitle,
        status = 'primary',
        solidHeader = TRUE,
        
        # header with category1 names 
        if(ep$nLevels[1] > 1) fluidRow( 
            lapply(1:ep$nLevels[1], function(i){
                sampleRowColName(ep$columnWidth, 1, i, ep$categoryNames[[1]][i])
            }),
            if(ep$nLevels[2] > 1){
                column(width = ep$columnWidth,
                    tags$div(paste(options$categories[[2]]$singular, "Names"),
                             class = "samples-category2-names-label")
                )    
            } else ""
        ) else "",                
    
        # one row of category1 per category2, with category2 names
        lapply(1:ep$nLevels[2], function(j){
            fluidRow(
                lapply(1:ep$nLevels[1], function(i){
                    sampleRankList(assignSamplesId, ep$columnWidth, i, j, ep$initVals[[i]][[j]])
                }),
                if(ep$nLevels[2] > 1){
                    sampleRowColName(ep$columnWidth, 2, j, ep$categoryNames[[2]][j])
                } else ""
            )
        }),
        if(!is.na(selected)) tagList(
            span(style = "margin-left: 10px;", actionLink(session$ns('addGridColumn'), 'Add Column')),            
            span(style = "margin-left: 10px;", actionLink(session$ns('addGridRow'), 'Add Row'))
         ) else ""
    ))
})

# allow for expansion of previously saved sample sets without starting over
expandSampleSet <- function(categoryI){
    ep <- editPanelData()
    ep$nLevels[categoryI] <- ep$nLevels[categoryI] + 1
    ep$categoryNames[[categoryI]] <- append(ep$categoryNames[[categoryI]], "")
    setColumnWidth(ep)
}
observeEvent(input$addGridColumn, {
    ep <- expandSampleSet(1)
    ep$initVals[[ep$nLevels[1]]] <- sapply(seq_len(ep$nLevels[2]), function(i) list())
    editPanelData(ep)
})
observeEvent(input$addGridRow, {
    ep <- expandSampleSet(2)
    for(colI in seq_len(ep$nLevels[1])) ep$initVals[[colI]][[ep$nLevels[2]]] <- list()
    editPanelData(ep)
})

#----------------------------------------------------------------------
# use code intelligence to attempt to fill the grid based on sample name shared prefixes
#----------------------------------------------------------------------

# respond to the Autofill Grid click
observeEvent(input$autofillGrid, {
    startSpinner(session, paste(module, 'input$autofillGrid'))

    # collect needed information for parsing
    names <- sort(getSampleNames(makeUnique = TRUE))
    spans <- lapply(names, function(name) tags$span(name) )
    inputs <- sapply(1:2, function(i) {
        x <- input[[paste0('nLevels', i)]]
        if(is.null(x)) NA else x
    })
    nLevels <- sapply(1:2, function(i) if(is.na(inputs[i])) 1 else as.integer(inputs[i]) )
    isCategory <- sapply(1:2, function(i) nLevels[i] > 1 )
    nLevelsNeeded <- sum(isCategory)

    # if only one sample bucket, just fill it with all samples
    if(nLevelsNeeded == 0) return( commitAutofill( parseAutofill(spans) ) )
    
    # otherwise attempt to parse sample names by two distinct approaches
    split <- strsplit(input$autofillDelimiter, '\\s')[[1]][1]
    x <- if(split == 'fixed'){
        fillByPrefix(   names, spans, isCategory, nLevels, nLevelsNeeded)
    } else {
        if(split == "") split <- 'whitespace'        
        fillByDelimiter(names, spans, isCategory, nLevels, nLevelsNeeded, split)
    }
    if(!x) commitAutofill(message = "sample names do not conform to grid dimensions")
})

# find the possible set of all common prefixes from the beginning of sample names
fillByPrefix <- function(names, spans, isCategory, nLevels, nLevelsNeeded){

    # infer whether the first common prefix is likely to be category1 or category2
    # abort if can't cleanly fill either axis
    prefix1 <- commonPrefixGroups(names)
    matchPrefix1 <- sapply(1:2, function(i) {
        isCategory[i] && !is.na(prefix1$lengthAtGroupSize[nLevels[i]])
    })
    if(sum(matchPrefix1) == 0) return(FALSE)
    gridCategories <- if(matchPrefix1[1]) 1:2 else 2:1 # fill category1 first if possible
    nLevels_ <- nLevels[gridCategories]
    prefix1 <- parseMatchingPrefix(names, nLevels_[1], prefix1)
    if(nLevelsNeeded == 1) {
        return( commitAutofill( parseAutofill(spans, gridCategories[1] != 1, prefix1) ) )
    }

    # if two axes are needed, attempt to fill the second
    if(prefix1$length == prefix1$minLength) return(FALSE) # no more characters left for 2nd prefix
    names <- substring(names, prefix1$length + 1)
    prefix2 <- commonPrefixGroups(names)
    matchPrefix2 <- !is.na(prefix2$lengthAtGroupSize[nLevels_[2]])
    if(!matchPrefix2) return(FALSE)
    prefix2 <- parseMatchingPrefix(names, nLevels_[2], prefix2)
    commitAutofill( parseAutofill(spans, gridCategories[1] != 1, prefix1, prefix2) )
}
parseMatchingPrefix <- function(names, nLevels, prefix){ # extract the prefix that generates a specific number of levels
    length <- prefix$lengthAtGroupSize[nLevels]
    list(
        minLength = prefix$minLength,
        length = length,
        uniquePrefixes = prefix$uniquePrefixes[[length]],
        prefixes = substr(names, 1, length)          
    )
}

# fill by splitting names on an inferred delimiter
fillByDelimiter <- function(names, spans, isCategory, nLevels, nLevelsNeeded, split){
 
    # infer whether the first common prefix is likely to be category1 or category2
    # abort if can't cleanly fill either axis        
    prefix1 <- commonSplitElementGroups(names, split, 1, require.suffix = TRUE)
    matchPrefix1 <- sapply(1:2, function(i) {
        isCategory[i] && prefix1$nUniquePrefixes == nLevels[i]
    })
    if(sum(matchPrefix1) == 0) return(FALSE)        
    gridCategories <- if(matchPrefix1[1]) 1:2 else 2:1 # fill category1 first if possible        
    nLevels_ <- nLevels[gridCategories]
    if(nLevelsNeeded == 1) {
        return( commitAutofill( parseAutofill(spans, gridCategories[1] != 1, prefix1) ) )
    }        

    # if two axes are needed, attempt to fill the second
    prefix2 <- commonSplitElementGroups(names, split, 2, require.suffix = FALSE)
    matchPrefix2 <- prefix2$nUniquePrefixes == nLevels_[2]
    if(!matchPrefix2) return(FALSE)
    commitAutofill( parseAutofill(spans, gridCategories[1] != 1, prefix1, prefix2) )        
}

# send our parsed data to the grid
parseAutofill <- function(spans, flip = FALSE, prefix1 = NULL, prefix2 = NULL){
    if(is.null(prefix1)) prefix1 <- list(uniquePrefixes = '', prefixes = '')
    if(is.null(prefix2)) prefix2 <- list(uniquePrefixes = '', prefixes = '')
    if(flip){
        list(
            samples = lapply(prefix2$uniquePrefixes, function(p2)
                      lapply(prefix1$uniquePrefixes, function(p1) spans[
                          prefix1$prefixes == p1 & 
                          prefix2$prefixes == p2
                      ] )),
            categoryNames = list(prefix2$uniquePrefixes, prefix1$uniquePrefixes)
        )            
    } else {
        list(
            samples = lapply(prefix1$uniquePrefixes, function(p1)
                      lapply(prefix2$uniquePrefixes, function(p2) spans[
                          prefix1$prefixes == p1 & 
                          prefix2$prefixes == p2
                      ] )),
            categoryNames = list(prefix1$uniquePrefixes, prefix2$uniquePrefixes)
        )     
    }
}
commitAutofill <- function(autofillData = NULL, message = NULL){
    autofillData <<- autofillData
    resetEditPanel(message)
    stopSpinner(session)
    TRUE
}

#----------------------------------------------------------------------
# commit the grid when user clicks the Save button
#----------------------------------------------------------------------

# customize the save button feedback
sendFeedback <- recordFeedbackFunction(output, 'saveRecordFeedback')

# assemble/edit a record
observeEvent(input$saveRecord, {

    # suppress error message on first load
    req(input$saveRecord)
    if(input$saveRecord == 0) return("")
    reportProgress('input$saveRecord', module)
        
    # initialize a new sample set object
    d <- list()
    d$nLevels <- editPanelData()$nLevels
    d$nSamples <- 0
    sourceSamples <- app[[options$source]]$outcomes$samples()
    d$assignments <- data.frame(Source_ID  = sourceSamples$Source_ID,
                                Project    = sourceSamples$Project,   # lock in the current set of samples
                                Sample_ID  = sourceSamples$Sample_ID, # same as used to generate the grid
                                Category1  = rep(NA, nrow(sourceSamples)),
                                Category2  = rep(NA, nrow(sourceSamples)),
                                    stringsAsFactors = FALSE)

    # fill in the grid assignments (not all grid cells need to be assigned)
    names <- getSampleNames(makeUnique = TRUE)
    hasData <- array(TRUE, d$nLevels)
    for(i in 1:d$nLevels[1]){
        for(j in 1:d$nLevels[2]){        
            id_ <- getSortableGridId(assignSamplesId, i, j, useNS = FALSE)
            samplesIs <- which(names %in% input[[id_]])
            N <- length(samplesIs)
            if(N == 0) {
                hasData[i, j] <- FALSE
                next
            }
            d$nSamples <- d$nSamples + N
            d$assignments[samplesIs, 'Category1'] <- i # numeric category indices, i.e. factor levels
            d$assignments[samplesIs, 'Category2'] <- j
        }
    }

    # prepared to finish saving the sample set if complete or approved (see below)
    finishSave <- function(...){
        
        # remove the rows for samples not assigned to any category1+category2
        # thus, sample set is independent of the samples list in force at the time of its creation
        d$assignments <- d$assignments[!is.na(d$assignments$Category1), ]

        # create a ~unique identifying signature to prevent record duplicates
        #   defining values:
        #       Project, Sample_ID, category1 and category2 index 
        r <- initializeRecordEdit(d, workingId, data$list, 'Sample Set', 'sample set', sendFeedback)
        # continue filling non-defining record values
        d$name <- r$name
        nullNames <- c('Column', 'Row')
        d$categoryNames <- lapply(1:2, function(categoryN){
            sapply(1:d$nLevels[categoryN], function(i){
                id_ <- paste(getSampleCategoryClass(categoryN), i, sep = "-")
                x <- if(is.null(input[[id_]])) "" else input[[id_]]
                if(x == "") {
                    singular <- if(isCategory[categoryN]) options$categories[[categoryN]]$singular 
                                else nullNames[categoryN]
                    paste0(singular, " #", i) 
                } else x              
            })
        })

        # if requested by the calling app, validate the sample assignment set
        if(!validateSampleAssignments__(options$validationFn, d, sendFeedback)) {
            workingId <<- NULL
            return()
        }

        # save our work
        saveEditedRecord(d, workingId, data, r)

        # place a lock on the parent source of all samples in the set
        if(r$isEdit) changeLocks(workingId, clearRecordLocks)
        changeLocks(r$id, placeRecordLocks)        
        
        # report success
        workingId <<- NULL
        sendFeedback("sample set saved")    
    }

    # check for missing data and reject or prompt as appropriate
    if(all(!hasData)) sendFeedback('the grid is empty', TRUE)
    if(d$nLevels[1] > 1 && any(rowSums(hasData) == 0)) sendFeedback('one or more grids columns are empty', TRUE)
    if(d$nLevels[2] > 1 && any(colSums(hasData) == 0)) sendFeedback('one or more grids rows are empty',    TRUE)
    if(d$nLevels[1] > 1 && d$nLevels[2] > 1 && any(!hasData)){
        if(is.null(options$allowEmptyCells)) options$allowEmptyCells <- FALSE
        if(!options$allowEmptyCells) sendFeedback('one or more grids cells are empty', TRUE)
        showUserDialog(
            "Confirm Empty Cells", 
            tags$p("One or more grid cells are empty."), 
            tags$p("Click OK to accept the partially filled grid."), 
            callback = finishSave
        )
        return()
    }
    finishSave() 
})

#----------------------------------------------------------------------
# clear any selected summary table rows on form reset or grid structure change
#----------------------------------------------------------------------
resetEditPanel <- addResetObserver(c(
    'resetEditPanel',
    'nLevels1',
    'nLevels2'
), input, module, data, sendFeedback, updateEditPanel)

#----------------------------------------------------------------------
# reactively update the sampleSets summary table
#   the response to user action on archetypal pattern inputs
#----------------------------------------------------------------------
addDataListObserver(module, summaryTemplate, data, function(r, id){
    df <- data.frame(
        Remove = rep('', length(r$nSamples)),
        Name   = '',
            stringsAsFactors = FALSE
    )
    for(i in 1:2){
        if(isCategory[i]) df[[options$categories[[i]]$plural]] <- if(r$nLevels[i] > 1)
            paste(r$categoryNames[[i]], collapse = "<br>") else 'NA'        
    }
    df$N_Samples <- r$nSamples # put at end for nicer on screen formatting
    df
})

#----------------------------------------------------------------------
# define bookmarking actions
#----------------------------------------------------------------------
observe({
    bm <- getModuleBookmark(id, module, bookmark, locks)
    req(bm)
    data$list  <- bm$outcomes$sampleSets
    data$names <- bm$outcomes$sampleSetNames
})

#----------------------------------------------------------------------
# set return values as reactives that will be assigned to app$data[[stepName]]
#----------------------------------------------------------------------
list(
    outcomes = list(
        sampleSets     = reactive(data$list),
        sampleSetNames = reactive(data$names)      
    ),
    isReady = reactive({ getStepReadiness(options$source, data$list) })
)

#----------------------------------------------------------------------
# END MODULE SERVER
#----------------------------------------------------------------------
})}
#----------------------------------------------------------------------
