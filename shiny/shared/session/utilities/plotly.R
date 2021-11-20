#----------------------------------------------------------------------
# plotly plotting helpers
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# setting axis ranges
# these function may be passed to interactivePlotServer as xrange or yrange
#----------------------------------------------------------------------

# place the bulk of one-sided data into the initial view
range_pos <- function(d, axis, foldIQR = 1.5){
    padding <- foldIQR * IQR(d[[axis]])
    c(0, quantile(d[[axis]], 0.75) + padding) # a bit more generous than standard outlier criteria
}

# place the bulk of two-sided data into the initial view
range_both <- function(d, axis, foldIQR = 1.5){ 
    padding <- foldIQR * IQR(d[[axis]])
    c(quantile(d[[axis]], 0.25) - padding,
      quantile(d[[axis]], 0.75) + padding)
}

# place all data into view with a bit of blank padding on each side
range_pad <- function(d, axis, padding = 0.025){
    if(length(d[[axis]]) < 1) return(NULL)
    r <- range(d[[axis]], na.rm = TRUE)
    w <- diff(r)
    p <- w * padding
    c(r[1] - p, r[2] + p)
}
