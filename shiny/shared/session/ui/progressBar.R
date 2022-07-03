#----------------------------------------------------------------------
# create MDI-styled progress bars
#----------------------------------------------------------------------
# "inderminate" means there is no meanignful way to know how much of a task is completed
#----------------------------------------------------------------------

# indetermine progress bar
mdiIndeterminateProgressBar <- function(width = 400, height = 15, duration = 4){
    param <- list(
        width_      = width,
        height_     = height,
        duration_   = duration,     
        barWidth_   = width / 5,
        offset_     = max(width / 5 - 10, 0)
    )
svg <- '<svg viewBox="0 0 width_ height_" style="width: width_px; height: height_px;" xmlns="http://www.w3.org/2000/svg">
  <rect width="width_" height="height_" rx="5" style="fill: #ddd;"></rect>
  <rect width="barWidth_" height="height_" rx="5" style="fill: #3c8dbc">
    <animate attributeName="x" values="-offset_;390;-offset_" dur="4s" repeatCount="indefinite" />
  </rect>
</svg>'
    for(x in names(param)) svg <- gsub(x, param[[x]], svg)
    HTML(svg)
}
