
#----------------------------------------------------------------------
# utility functions to help apply standard statisical functions
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

# the Portal uses functions from the zoo package for rolling value (e.g. zoo::rollmean)
# however, we extend them here to include an option for circular permutation
circular_permute_vector <- function(x, k){
    nSide <- floor(as.integer(k) / 2)
    len <- length(x)
    c(x[(len-nSide+1):len], x, x[1:nSide])    
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
    x < quantile(x,0.25) - padding | # return boolean where TRUE = outlier
    x > quantile(x,0.75) + padding
}

