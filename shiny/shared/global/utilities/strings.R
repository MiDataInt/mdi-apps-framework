#----------------------------------------------------------------
# string functions
#----------------------------------------------------------------

# load the entirety of a file as a single character string
loadResourceText <- function(fileName) readChar(fileName, file.info(fileName)$size)
slurpFile <- loadResourceText

# convert first character in word to upper case
ucFirst <- function(y) { 
    c <- strsplit(y, " ")[[1]]
    paste(toupper(substring(c, 1, 1)), substring(c, 2), sep = "", collapse = " ")
}

# convert numbers to character strings with commas
commify <- function(x) format(x, big.mark = ",", scientific = FALSE)

# find the longest common prefix of a string
# modified from Bioconductor::Biobase::lcPrefix
longestCommonPrefix <- function(x, ignore.case = FALSE) {
    if (ignore.case) x <- toupper(x)
    nc <- nchar(x, type = "char")
    for (i in 1:min(nc)) {
        ss <- substr(x, 1, i)
        if (any(ss != ss[1])) return(substr(x[1], 1, i - 1))
    }
    substr(x[1], 1, i)
}

# find common prefixes among a set of strings
# here, prefixes must be of the same length (e.g. 001,002...999)
commonPrefixGroups <- function(x, ignore.case = FALSE) {
    if (ignore.case) x <- toupper(x)
    nc <- nchar(x, type = "char")
    minLength <- min(nc) # i.e. the shortest string
    uniquePrefixes  <- sapply(1:minLength, function(i) unique(substr(x, 1, i)), simplify = FALSE)
    nUniquePrefixes <- sapply(uniquePrefixes, length)
    list(
        minLength = minLength,
        uniquePrefixes  = uniquePrefixes,
        nUniquePrefixes = nUniquePrefixes,
        lengthAtGroupSize = sapply(1:max(nUniquePrefixes), function(n){
            matches <- which(nUniquePrefixes == n)
            length <- if(length(matches) > 0) max(matches) else -Inf
            if(length > 0) length else NA
        })
    )
}

# find common element groups in front of an instance of a delimiter
# similar to commonPrefixGroups, but expects formats like 1-x, 22-y)
commonSplitElementGroups <- function(x, split, position, ignore.case = FALSE, require.suffix = TRUE){
    if (ignore.case) x <- toupper(x)
    isWhitespace <- toupper(split) == "WHITESPACE"
    if(isWhitespace) split <- '\\s'
    y <- strsplit(x, split, fixed = !isWhitespace)
    nElements <- sapply(y, length)
    requiredNElements <- if(require.suffix) position + 1 else position
    if(any(nElements < requiredNElements)) return(list(
        prefixes = character(),
        uniquePrefixes  = character(),
        nUniquePrefixes = 0                                                 
    ))
    prefixes <- unlist(lapply(y, function(v) v[position]))
    uniquePrefixes  <- unique(prefixes)
    list(
        prefixes = prefixes,
        uniquePrefixes  = uniquePrefixes,
        nUniquePrefixes = length(uniquePrefixes)
    )
}
