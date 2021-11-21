#----------------------------------------------------------------------
# define generic functions, i.e., methods, available to S3 classes,
# if the class declares method.class <- function()
#----------------------------------------------------------------------
# new generic declarations take the form:
#     genericName <- function(x, ...) {
#         UseMethod("genericName", x)
#     }
#----------------------------------------------------------------------

# retrieve an XY table for plotting in a scatter plot, etc.
# results expected be returned as a data.table(x=x,y=y)
getXY <- function(x, ...) {
    UseMethod("getXY", x)
}

# retrieve a single vector of values, i.e., one column of a table
getCol <- function(x, ...) {
    UseMethod("getCol", x)
}

# matrix dimensions
nFeatures <- function(x, ...) {
    UseMethod("nFeatures", x)
}
nSamples <- function(x, ...) {
    UseMethod("nSamples", x)
}

# initialize and use an HMM object from a class object; typically call HMM::initHMM, HMM::viterbi
initHMM <- function(x, ...) {
    UseMethod("initHMM", x)
}
viterbi <- function(x, ...) {
    UseMethod("viterbi", x)
}
keyedViterbi <- function(x, ...) {
    UseMethod("keyedViterbi", x)
}

# probablity generics
cumprob <- function(x, ...) {
    UseMethod("cumprob", x)
}
