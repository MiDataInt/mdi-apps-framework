#----------------------------------------------------------------------
# utility functions to help apply standard statistical functions
#----------------------------------------------------------------------

# fit a trendline to a set of input data by one of several methods
# data object must have x and y members
fitTrendline <- function(d, method='loess', forceOrigin=FALSE){
    O <- forceOrigin # if TRUE, fit curve must pass through origin, i.e. intercept forced to zero
    switch(
        method,
        loess = {
            loess(y ~ x, d)
        },
        linear = {
            if(O) lm(y ~ 0 + x, d)
            else  lm(y ~     x, d)
        },
        quadratic = {
            d$x2 <- d$x ^ 2
            if(O) lm(y ~ 0 + x + x2, d)
            else  lm(y ~     x + x2, d)
        },    
        cubic = {
            d$x2 <- d$x ^ 2
            d$x3 <- d$x ^ 3
            if(O) lm(y ~ 0 + x + x2 + x3, d)
            else  lm(y ~     x + x2 + x3, d)
        }
    )
}

# use functions from the zoo package for rolling values (e.g. zoo::rollmean)
# however, we extend them here to include an option for circular permutation
circular_permute_vector <- function(x, k){
    nSide <- floor(as.integer(k) / 2)
    len <- length(x)
    c(x[(len - nSide + 1):len], x, x[1:nSide])    
}
rollmean_permute <- function(x, k, ...) {
    rollmean( circular_permute_vector(x, k), k, ... ) # already trims the NA bins on the flanks
}
rollmedian_permute <- function(x, k, ...) {
    rollmedian( circular_permute_vector(x, k), k, ... )
}
rollsum_permute <- function(x, k, ...) {
    rollsum( circular_permute_vector(x, k), k, ... )
}

# identify outliers based on inter-quartile range method
outliers_IQR <- function(x, foldIQR=1.5){ # x is a vector of values
    padding <- foldIQR * IQR(x)
    x < quantile(x, 0.25) - padding | # return boolean where TRUE = outlier
    x > quantile(x, 0.75) + padding
}

# correlation-based distance of a set of already-centered Z scores
pearson.dist <- function(m) { # m is a matrix
  m <- m / sqrt(rowSums(m^2))
  m <-  tcrossprod(m)
  m <- as.dist(m)
  0.5 - m / 2
}
pearson.matrix <- function(m) { # m is a matrix
  m <- m / sqrt(rowSums(m^2))
  m <-  tcrossprod(m)
  0.5 - m / 2 # return the full matrix, not a dist formatted object
}

# fit points within a plot axis plus a little padding
paddedRange <- function(v, paddingFrac = 0.05){
    range <- range(v)
    width <- range[2] - range[1]
    if(width == 0) return(range)
    padding <- width * paddingFrac
    c(range[1] - padding, range[2] + padding)
}

# get the peak of a distribution
peakValue <- function(x){
    x <- x[!is.na(x)]
    if(length(x) == 0) return(NA)
    d <- density(x)
    d$x[which.max(d$y)]
}
