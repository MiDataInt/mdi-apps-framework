#----------------------------------------------------------------------
# load sample source data from the disk
# because functions are nearly always used in launched jobs, disallow reactives
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# event counts, one or more samples
#----------------------------------------------------------------------

# return a matrix of feature counts by sample
# format is that of DESeq2 but adaptable to other purposes
getSampleCountData <- function(job){

    # extract and tweak the sample assignments = DESeq2 colData
    colData <- job$parameters$sampleSets[[job$schema$Sample_Set]]$assignments
    colData$Category1 <- factor(colData$Category1) # DESeq2 demands factors for grouping
    colData$Category2 <- factor(colData$Category2)

    # build the table of counts for all requested samples
    # check for feature list consistency in all incoming samples
    cache <- list()
    featureList <- NULL    
    countData <- apply(colData[, c('Source_ID', 'Project', 'Sample_ID')], 1, function(v){
        sourceId <- v[[1]]
        sampleId <- paste(v[2], v[3], sep = ":")        
        sampleIds <- getSampleUniqueIds(samples = job$parameters$samples, sourceId = sourceId)    
    
        # get counts from source countMatrix file
        # assumes file has one feature column and multiple sample columns in same order as a manifest
        if(is.null(cache[[sourceId]])) cache[[sourceId]] <<- {
            file <- getProjectFileByType(job$parameters$sources[[sourceId]], 'countMatrix')
            dt <- fread(file$path, header = TRUE)
            dt <- dt[order(dt[[1]])] # sort by the featureId column
            as.data.frame(dt)
        }
        is <- c(1, which(sampleIds == sampleId) + 1)
        df <- cache[[sourceId]][,is]
        
        # validate common order of features across all files and return this sample's counts
        if(is.null(featureList)) {
            featureList <<- df[[1]]
        } else if(!identical(featureList, df[[1]])) {
            stop(safeError("samples have different feature lists"))
        }
        pmax(0, round(as.numeric(df[, 2], 0)))
    })
    
    # name the rows and columns of the count matrix
    rownames(countData) <- featureList
    colnames(countData) <- paste(colData$Project, colData$Sample_ID, sep = ":")
    
    # return the matrix
    list(
        colData = colData, # a data frame
        countData = as.matrix(countData)
    )
}

## load (some) samples from a data file with one feature column and multiple sample columns
##   expects genes/features in rows, samples in columns, counts in cells
##   returns the feature column (column 1) plus requested sample columns
#loadSampleFeatureCounts_multisample <- function(projectFileOptions, sampleIs=NULL){
#
#    file <- getProjectFile(projectFileOptions) # must return a single file
#
#    if(is.null(file)) safeError('could not find counts file')
#    if(length(file) > 1) safeError('too many matching counts files')
#    dt <- fread(file$path)
#    if(!is.null(sampleIs)) dt <- dt[,c(1,sampleIs+1)]
#    dt
#}
