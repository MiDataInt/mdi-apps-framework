
#----------------------------------------------------------------------
# load sample source data from the disk
# because functions are nearly always used in launched jobs, disallow reactives here
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# read counts, one or more samples
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
    countData <- apply(colData[,c('Source_ID','Project','Sample_ID')], 1, function(v){
        sourceId <- v[[1]]
        sampleId <- paste(v[2], v[3], sep=":")        
        sampleIds <- getSampleUniqueIds(samples=job$parameters$samples, sourceId=sourceId)    
    
        # get counts from project countMatrix file
        # assumes file has one feature column and multiple sample columns in same order as manifest
        if(is.null(cache[[sourceId]])) cache[[sourceId]] <<- {
            file <- getProjectFileByType(job$parameters$sources[[sourceId]], 'countMatrix')
            dt <- fread(file$path, header=TRUE)
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
        pmax(0, round(as.numeric(df[,2],0)))
    })
    
    # name the rows and columns of the count matrix
    rownames(countData) <- featureList
    colnames(countData) <- paste(colData$Project, colData$Sample_ID, sep=":")
    
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

#List of 4
# $ schemaId    : chr "8aa1fd62ff1c1261ba1f7f234e14dea3"
# $ schema      :List of 6
#  ..$ Sample_Set   : chr "602a7b4d09b1484f72478a5badab3c74"
#  ..$ Analysis_Type: chr "DESeq2"
#  ..$ Analyze_By   : chr "Category1"
#  ..$ optionsHtml  : chr "Analyze_By = Group"
#  ..$ name         : chr "Analysis #1"
#  ..$ status       : num -2
# $ analysisType:List of 6
#  ..$ name    : chr "DESeq2"
#  ..$ jobType : chr "promise"
#  ..$ options :List of 1
#  .. ..$ Analyze_By:List of 3
#  .. .. ..$ type   : chr "selectInput"
#  .. .. ..$ choices: chr "getInvertedAnalyzeByNames"
#  .. .. ..$ value  : chr "Category1"
#  ..$ packages:List of 2
#  .. ..$ R           : NULL
#  .. ..$ Bioconductor: chr "DESeq2"
#  ..$ classes : NULL
#  ..$ modules : NULL
# $ parameters  :List of 6
#  ..$ analysisSetName: chr "QuantSeq.2021-08-14"
#  ..$ sources        :List of 1
#  .. ..$ a2903be5721de792d7670b94efae0963:List of 8
#  .. .. ..$ manifestType: chr "IlluminaDefault"
#  .. .. ..$ nSamples    : int 4
#  .. .. ..$ manifest    :'data.frame':  16 obs. of  13 variables:
#  .. .. .. ..$ Lane                       : int [1:16] 1 1 1 1 2 2 2 2 3 3 ...
#  .. .. .. ..$ Project                    : chr [1:16] "7-LZ" "7-LZ" "7-LZ" "7-LZ" ...
#  .. .. .. ..$ Sample_ID                  : chr [1:16] "7-LZ-1" "7-LZ-2" "7-LZ-3" "7-LZ-4" ...
#  .. .. .. ..$ Description                : chr [1:16] "1D-1WT" "2D-1SnailKO" "3D0WT" "4D0SnailKO" ...
#  .. .. .. ..$ Barcode_sequence           : chr [1:16] "TTAACT+TACGCG" "ATGAAC+ATAATC" "CCTAAG+GGTCAT" "AATCCG+TATCAG" ...
#  .. .. .. ..$ PF_Clusters                : int [1:16] 4125048 4660760 4010316 4699810 3933253 4482881 3869769 4517387 4209223 4756152 ...
#  .. .. .. ..$ Percent_of_thelane         : num [1:16] 6.02 6.8 5.85 6.86 5.96 6.79 5.86 6.84 6.03 6.81 ...
#  .. .. .. ..$ Percent_Perfectbarcode     : num [1:16] 96.7 96.8 96.8 96.1 96.7 ...
#  .. .. .. ..$ Percent_One_mismatchbarcode: num [1:16] 3.26 3.18 3.15 3.91 3.34 3.28 3.23 3.94 3.33 3.34 ...
#  .. .. .. ..$ Yield_Mbases               : int [1:16] 413 466 401 470 393 448 387 452 421 476 ...
#  .. .. .. ..$ Percent_PFClusters         : num [1:16] 100 100 100 100 100 100 100 100 100 100 ...
#  .. .. .. ..$ Percent_GTorEQ2_Q30bases   : num [1:16] 85.9 85 85.7 84.7 85.9 ...
#  .. .. .. ..$ Mean_QualityScore          : num [1:16] 32.8 32.6 32.7 32.5 32.7 ...
#  .. .. ..$ unique      :'data.frame':  4 obs. of  8 variables:
#  .. .. .. ..$ Project          : chr [1:4] "7-LZ" "7-LZ" "7-LZ" "7-LZ"
#  .. .. .. ..$ Sample_ID        : chr [1:4] "7-LZ-1" "7-LZ-2" "7-LZ-3" "7-LZ-4"
#  .. .. .. ..$ Description      : chr [1:4] "1D-1WT" "2D-1SnailKO" "3D0WT" "4D0SnailKO"
#  .. .. .. ..$ PF_Clusters      : num [1:4] 16161590 18369767 15814349 18488010
#  .. .. .. ..$ Yield_Mbases     : num [1:4] 1616 1837 1581 1849
#  .. .. .. ..$ Mean_QualityScore: num [1:4] 32.7 32.5 32.7 32.5
#  .. .. .. ..$ Yield            : num [1:4] 16161590 18369767 15814349 18488010
#  .. .. .. ..$ Quality          : num [1:4] 32.7 32.5 32.7 32.5
#  .. .. ..$ dataDir     : chr "C:/Users/wilso/OneDrive/Documents/magc-portal/magc-portal-data/session/projects/a2/90/a2903be5721de792d7670b94efae0963"
#  .. .. ..$ config      :List of 6
#  .. .. .. ..$ command   : chr "do"
#  .. .. .. ..$ entropy   : chr "393b81456379e311f2d7"
#  .. .. .. ..$ files     :List of 3
#  .. .. .. .. ..$ countMatrix :List of 3
#  .. .. .. .. .. ..$ file     : chr "counts_raw.txt"
#  .. .. .. .. .. ..$ header   : logi TRUE
#  .. .. .. .. .. ..$ separator: chr "tab"
#  .. .. .. .. ..$ manifestFile:List of 2
#  .. .. .. .. .. ..$ file: chr "NextA-469_7-LZ_DemuxStats.txt"
#  .. .. .. .. .. ..$ type: chr "IlluminaDefault"
#  .. .. .. .. ..$ qcReport    :List of 2
#  .. .. .. .. .. ..$ file: chr "multiqc_report.html"
#  .. .. .. .. .. ..$ type: chr "multiqc"
#  .. .. .. ..$ pipeline  : chr "quant-seq"
#  .. .. .. ..$ task      : NULL
#  .. .. .. ..$ uploadType: chr "quant-seq"
#  .. .. ..$ sourceType  : chr "project"
#  .. .. ..$ fileName    : chr "NextA-469.quant-seq.magc.package.zip"
#  ..$ samples        :'data.frame':     4 obs. of  4 variables:
#  .. ..$ Source_ID  : chr [1:4] "a2903be5721de792d7670b94efae0963" "a2903be5721de792d7670b94efae0963" "a2903be5721de792d7670b94efae0963" "a2903be5721de792d7670b94efae0963"
#  .. ..$ Project    : chr [1:4] "7-LZ" "7-LZ" "7-LZ" "7-LZ"
#  .. ..$ Sample_ID  : chr [1:4] "7-LZ-1" "7-LZ-2" "7-LZ-3" "7-LZ-4"
#  .. ..$ Description: chr [1:4] "1D-1WT" "2D-1SnailKO" "3D0WT" "4D0SnailKO"
#  ..$ sampleNames    : list()
#  ..$ sampleSets     :List of 1
#  .. ..$ 602a7b4d09b1484f72478a5badab3c74:List of 5
#  .. .. ..$ nLevels      : int [1:2] 2 1
#  .. .. ..$ nSamples     : num 4
#  .. .. ..$ assignments  :'data.frame': 4 obs. of  5 variables:
#  .. .. .. ..$ Source_ID: chr [1:4] "a2903be5721de792d7670b94efae0963" "a2903be5721de792d7670b94efae0963" "a2903be5721de792d7670b94efae0963" "a2903be5721de792d7670b94efae0963"
#  .. .. .. ..$ Project  : chr [1:4] "7-LZ" "7-LZ" "7-LZ" "7-LZ"
#  .. .. .. ..$ Sample_ID: chr [1:4] "7-LZ-1" "7-LZ-2" "7-LZ-3" "7-LZ-4"
#  .. .. .. ..$ Category1: int [1:4] 1 2 1 2
#  .. .. .. ..$ Category2: int [1:4] 1 1 1 1
#  .. .. ..$ name         : chr "Sample Set #1"
#  .. .. ..$ categoryNames:List of 2
#  .. .. .. ..$ : chr [1:2] "WT" "KO"
#  .. .. .. ..$ : chr "Condition #1"
#  ..$ sampleSetNames :List of 1
#  .. ..$ 602a7b4d09b1484f72478a5badab3c74: chr "WT vs. KO"
# - attr(*, "class")= chr "DESeq2"

