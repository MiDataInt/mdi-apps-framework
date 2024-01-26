#----------------------------------------------------------------------
# server components for plotting one or more frequency distributions
#----------------------------------------------------------------------
mdiDensityPlotBoxServer <- function(
    id,
    #----------------------------------------------------------------------  
    data, # a data.table with columns x, trackCols and groupingCols, or a reactive that returns it
    groupingCols, # column(s) that define the groups to summarize as distinct distributions, or a reactive that returns it; can be NULL
    xlab, # x axis label, or a reactive that returns it 
    #----------------------------------------------------------------------  
    aggFn  = NULL, # the aggregate function used to establish density counts, passed to dcast, defaults to length
    aggCol = NULL, # the column to which fun.aggregate is applied
    groupLabels = NULL, # set to force these, and only these, groups, regardless of the values in data[[groupingCols]]
    trackCols = NULL,   # optional columns used to group data into tracks, or a reactive that returns it; can be NULL and usually is
    trackLabels = NULL, # set to force these, and only these, tracks, regardless of the values in data[[trackCols]]
    trackSameXLim = TRUE, # logical or reactive to force all tracks to use the same X axis values
    trackSameYLim = TRUE, # logical or reactive to force all tracks to use the same Y axis values.
    #----------------------------------------------------------------------  
    xlim = NULL, # vector of xlim values, or a list named with trackLabels, or a reactive that returns it
    #----------------------------------------------------------------------   
    defaultSettings = mdiDensityPlotSettings, # settings, possibly with updated default values and ranges
    defaultBinSize = 1, # starting bin resolution on the X axis
    eventPlural = "events", # name of the thing being counted for the plot title
    plotFrameReactive = NULL, # optional reactive that overrides the plot frame as list(frame, mar)
    plotTitle = NULL, # optional reactive to override the automated title
    legendSide = c(4, 3), # side on which to plot the legend
    underscoresToSpaces_ = TRUE,
    vShade = NULL, # vector of two X-axis values between which to shade the plot background, or a list named with trackLabels, or a reactive that returns it
    v = NULL, # X-axis values at which to place line rules, or a list named with trackLabels, or a reactive that returns it
    h = NULL, # Y-axis values at which to place line rules, or a list named with trackLabels, or a reactive that returns it
    groupV = NULL, # a named list of group-specific X-axis values at which to place line rules, or a list named with trackLabels, or a reactive that returns it
    groupH = NULL, # a named list of group-specific Y-axis values at which to place line rules, or a list named with trackLabels, or a reactive that returns it
    vColor = "grey60", # the color(s) used to draw v rules, or a list named with trackLabels, or a reactive that returns it
    hColor = "grey60", # the color(s) used to draw h rules, or a list named with trackLabels, or a reactive that returns it
    innerMar = c(2.1, 0.1), # the bottom and top plot margins applied between multiple tracks
    linesPerInch = 8.571429, # plotting lines per inch for converting mar to mai
    justification = c("center","left","right"), # where data points are positioned within the span of each bin
    pregrouped = FALSE, # set to true if data is a pre-calculated list(X_Bin_Size, groupLabels, trackLabels, totalN, dt = data.table(track,group,x,y))
    ylab = NULL, # override the default Y axis label; required if pregrouped is TRUE
    trackLabelPosition = "left", # where to place track labels, (left","center","none"), or a reactive that returns a value
    ... # additional options passed to mdiXYPlot()
) { 
#----------------------------------------------------------------------
# collect anything with the potential to change the plotted data
pd <- reactive({
    if(pregrouped){
        pgd <- if(is.function(data)) data() else data
        aggFn  <<- rep
        aggCol <<- "y"
        list(
            dt              = pgd$dt,
            trackCols       = "track",
            trackLabels     = pgd$trackLabels,
            groupingCols    = "group",
            groupLabels     = pgd$groupLabels,
            X_Bin_Size      = pgd$X_Bin_Size,
            Y_Axis_Value    = "pregrouped",
            trackSameXLim   = if(is.function(trackSameXLim)) trackSameXLim() else trackSameXLim,
            trackSameYLim   = if(is.function(trackSameYLim)) trackSameYLim() else trackSameYLim,
            totalN          = pgd$totalN
        )
    } else {
        list(
            dt              = if(is.data.table(data)) data else data(),
            trackCols       = if(is.function(trackCols)) trackCols() else trackCols,
            trackLabels     = if(is.function(trackLabels)) trackLabels() else trackLabels,
            groupingCols    = if(is.function(groupingCols)) groupingCols() else groupingCols,
            groupLabels     = if(is.function(groupLabels)) groupLabels() else groupLabels,
            X_Bin_Size      = plot$settings$get("Density_Plot","X_Bin_Size"),
            Y_Axis_Value    = plot$settings$get("Density_Plot","Y_Axis_Value"),
            trackSameXLim   = if(is.function(trackSameXLim)) trackSameXLim() else trackSameXLim,
            trackSameYLim   = if(is.function(trackSameYLim)) trackSameYLim() else trackSameYLim
        )        
    }
})
#----------------------------------------------------------------------
# collect and group the plotted data
collapseGroupingCols <- function(dt, cols){
    if(!is.null(cols) && length(cols) > 0) {
        droppedCols <- character()
        for(col in cols){
            if(length(unique(dt[[col]])) == 1) droppedCols <- c(droppedCols, col)
        }
        cols[!(cols %in% droppedCols)]
    } else character()
}
addValueVar <- function(x, pd){
    if(!is.null(aggCol)) x$dt[[aggCol]] <- pd$dt[[aggCol]]
    x
}
parseGroupingCols <- function(pd){
    startSpinner(session, message = "parsing density")
    trackCols     <- collapseGroupingCols(pd$dt, pd$trackCols)
    groupingCols  <- collapseGroupingCols(pd$dt, pd$groupingCols)
    nTrackCols    <- length(trackCols)
    nGroupingCols <- length(groupingCols)
    hasTracks <- nTrackCols > 0
    hasGroups <- nGroupingCols > 0
    if(!hasTracks && !hasGroups) {
        return(list(
            trackCols = NA,
            nTrackCols = 0,
            tracks = "singleTrack",
            trackLabels = "singleTrack",
            nTracks = 1,
            hasTracks = hasTracks,
            groupingCols = NA,
            nGroupingCols = 0,
            groups = "singleGroup",
            groupLabels = NULL,
            nGroups = 1,
            hasGroups = hasGroups,
            groupCounts = data.table(trackGroup = "singleTrack", N = nrow(pd$dt)),
            dt = pd$dt[, .(x = x, track = "singleTrack", group = "singleGroup", trackGroup = "singleTrack::singleGroup")]
        ) %>% addValueVar(pd))
    }
    pd$dt[, ":="(
        track = if(hasTracks) {
            if(nTrackCols > 1) .SD[, apply(.SD, 1, paste, collapse = ", "), .SDcols = trackCols]
            else .SD[[trackCols]]
        } else "singleTrack",
        group = if(hasGroups) {
            if(nGroupingCols > 1) .SD[, apply(.SD, 1, paste, collapse = ", "), .SDcols = groupingCols]
            else .SD[[groupingCols]]
        } else "singleGroup"
    )]
    pd$dt[, trackGroup := paste(track, group, sep = "::")]
    tracks <- sort(unique(pd$dt$track))
    groups <- sort(unique(pd$dt$group))
    nTracks <- length(tracks)
    nGroups <- length(groups)
    trackGroups <- sort(unique(pd$dt$trackGroup)) 
    groupCounts <- pd$dt[, .N, by = .(trackGroup)]
    setkey(groupCounts, trackGroup)
    list(
        trackCols = trackCols,
        nTrackCols = nTrackCols,
        tracks = tracks,
        trackLabels = if(hasTracks && !is.null(pd$trackLabels)) pd$trackLabels else tracks, 
        nTracks = nTracks,
        hasTracks = nTracks > 1,
        groupingCols = groupingCols,
        nGroupingCols = nGroupingCols,
        groups = groups,
        groupLabels = if(hasGroups && !is.null(pd$groupLabels)) pd$groupLabels else NULL,
        nGroups = nGroups,
        hasGroups = nGroups > 1,
        trackGroups = trackGroups,
        groupCounts = groupCounts,
        dt = pd$dt[, .SD, .SDcols = c("x", "track", "group", "trackGroup")]
    ) %>% addValueVar(pd)
}
fillTrackGroups <- function(dt, pd){
    # dcast trackGroups to columns, counting the number of matching bin occurrences
    dt <- dcast(
        dt,
        "x ~ trackGroup", 
        fun.aggregate = if(is.null(aggFn)) length       else aggFn,
        value.var     = if(is.null(aggFn)) "trackGroup" else aggCol, 
        fill = 0
    )

    # if requested, convert trackGroup counts to frequencies
    tgs <- names(dt)
    tgs <- tgs[tgs != "x"]
    if(pd$Y_Axis_Value == "Frequency") for(tg in tgs) {
        dt[[tg]] <- dt[[tg]] / sum(dt[[tg]], na.rm = TRUE)
    }

    # while dcasted, add any missing X-axis bins and, if requested, fill with 0 values
    allBins <- min(dt$x):max(dt$x, na.rm = TRUE)
    missingBins <- allBins[!(allBins %in% dt$x)]
    if(length(missingBins) > 0){
        missingBins <- data.table(x = missingBins)
        dt <- rbind(dt, missingBins, fill = TRUE)
        if(plot$settings$get("Density_Plot","Missing_Bins_To_Zero")) dt[is.na(dt)] <- 0
        setorderv(dt, "x", order = 1L)
    }

    # reverse the dcast, i.e., melt, so each trackGroup has a simple xy data.table
    melt(
        dt, 
        id.vars = "x", 
        measure.vars = tgs,
        variable.name = "trackGroup", 
        value.name = "y",
        variable.factor = FALSE
    )[, ":="(
        track = sapply(trackGroup, function(x) strsplit(x, "::")[[1]][1]),
        group = sapply(trackGroup, function(x) strsplit(x, "::")[[1]][2]),
        x = x * pd$X_Bin_Size # expand back to the proper numeric scale
    )] 
}
fillAllTrackGroups <- function(pd, grouping){ # ensure that all groups have a value, even if 0, for all X axis bins
    startSpinner(session, message = "casting density")
    if(pregrouped) grouping$dt[, x := as.integer(floor(
        x / pd$X_Bin_Size
    ))] else grouping$dt[, x := as.integer(floor(
        switch(
            justification[1],
            left   = x,
            center = x + pd$X_Bin_Size / 2,
            right  = x + pd$X_Bin_Size
        ) / pd$X_Bin_Size
    ))] # thus, all bins are left-referenced, and integers (for now, see melt above)
    dt <- if(!grouping$hasTracks || pd$trackSameXLim){
        fillTrackGroups(grouping$dt, pd)
    } else {
        do.call(rbind, lapply(grouping$tracks, function(track_){
            fillTrackGroups(grouping$dt[track == track_], pd) 
        }))
    }
}
plotData <- reactive({
    pd <- pd()
    req(pd$dt, nrow(pd$dt) > 0)
    grouping <- parseGroupingCols(pd)
    list(
        grouping = grouping,
        dt = fillAllTrackGroups(pd, grouping),
        pd = pd
    )
})
#----------------------------------------------------------------------
defaultSettings$Density_Plot$X_Bin_Size$value <- defaultBinSize
getUserLim_ <- function(settingKey, fn, v, trackLabel, i){
    x <- trimws(plot$settings$get("Density_Plot", settingKey))
    if(!isTruthy(x) || x == "" || x == "auto") {
        if(is.null(xlim)) fn(v, na.rm = TRUE) # auto-scaling has lowest priority
        else {
            xlim <- if(is.function(xlim)) xlim() else xlim
            if(is.list(xlim)) xlim <- xlim[[trackLabel]]
            xlim[i] # track-level xlim set by calling code had middling priority
        }
    } else as.numeric(x) # user override has highest priority
}
getUserXLim <- function(x, trackLabel = NULL) c(
    getUserLim_("Min_X_Value", min, x, trackLabel, 1),
    getUserLim_("Max_X_Value", max, x, trackLabel, 2)
)  
plot <- staticPlotBoxServer(
    id,
    margins = is.null(plotFrameReactive),
    title = is.null(defaultSettings$Plot$Title),
    lines = TRUE,
    points = TRUE,
    settings = defaultSettings, 
    size = "m",
    Plot_Frame = if(is.null(plotFrameReactive)) NULL else reactive(plotFrameReactive()$frame),
    create = function() {
        d <- plotData()
        plotTitle <- if(is.null(plotTitle)) {
            pft <- trimws(plot$settings$get("Plot_Frame", "Title", NULL))
            pt  <- trimws(plot$settings$get("Plot",       "Title", NULL))
            if(!isTruthy(pft)) pft <- NULL
            if(!isTruthy(pt))  pt  <- NULL
            paste(c(
                pft,
                pt,
                if(is.function(titleSuffix)) titleSuffix() else NULL
            ), collapse = ", ") 
        } else plotTitle()
        totalN <-  paste(trimws(commify(
            if(is.null(d$pd$totalN)) sum(d$grouping$groupCounts$N) else d$pd$totalN
        )), eventPlural)
        plotTitle <- if(is.null(plotTitle) || length(plotTitle) == 0 || plotTitle == "") totalN 
                     else paste0(plotTitle, " (", totalN, ")")

        trackLabelPosition <- if(is.function(trackLabelPosition)) trackLabelPosition() else trackLabelPosition
        yPadding <- if(trackLabelPosition == "center") 1.2 else 1.05
        xlim <- if((!d$grouping$hasTracks || d$pd$trackSameXLim) && is.null(xlim)) getUserXLim(d$dt$x) else NULL
        ymax <- if(!d$grouping$hasTracks || d$pd$trackSameYLim) ymax <- d$dt[, max(y, na.rm = TRUE) * yPadding] else NULL

        pf <- if(is.null(plotFrameReactive)) list(
            frame = list(
                Width_Inches  = 3, # TODO: these values should be determined from settings 
                Height_Inches = 3,
                Font_Size = 7
            ),
            mar = c(4.1, 4.1, 2.1, 0.5),
            insideHeightPerTrack = 1
        ) else plotFrameReactive()
        trackMar <- lapply(1:d$grouping$nTracks, function(i){
            mar_ <- pf$mar
            if(i != d$grouping$nTracks) mar_[1] <- innerMar[1]
            if(i != 1) mar_[3] <- innerMar[2]   
            mar_         
        })
        layout(
            matrix(1:d$grouping$nTracks, ncol = 1), 
            heights = if(d$grouping$nTracks == 1) 1 else sapply(1:d$grouping$nTracks, function(i){
                pf$insideHeightPerTrack + sum(trackMar[[i]][c(1, 3)]) / linesPerInch
            }) / pf$frame$Height_Inches
        )

        vShade <- if(is.function(vShade)) vShade() else vShade
        v      <- if(is.function(v))      v()      else v
        h      <- if(is.function(h))      h()      else h
        groupV <- if(is.function(groupV)) groupV() else groupV
        groupH <- if(is.function(groupH)) groupH() else groupH
        vColor <- if(is.function(vColor)) vColor() else vColor
        hColor <- if(is.function(hColor)) hColor() else hColor

        for(i in seq_along(d$grouping$trackLabels)){
            trackLabel <- d$grouping$trackLabels[i]
            dt <- d$dt[track == trackLabel]
            if(!is.null(d$grouping$groupLabels)){
                dt <- dt[
                    group %in% d$grouping$groupLabels
                ][
                    order(match(group, d$grouping$groupLabels))
                ]
            }
            if(nrow(dt) == 0) next
            xlim_ <- if(is.null(xlim)) getUserXLim(dt$x, trackLabel) else xlim
            ylim_ <- c(0, if(is.null(ymax)) dt[, max(y, na.rm = TRUE) * yPadding] else ymax)
            par(mar = trackMar[[i]], cex = 1)
            plot$initializeFrame(
                xlim = xlim_,
                ylim = ylim_,
                xlab = if(i != d$grouping$nTracks) "" else if(is.function(xlab)) xlab() else xlab,
                ylab = if(is.null(ylab)) d$pd$Y_Axis_Value else ylab,
                xaxs = "i",
                yaxs = "i",
                title = if(legendSide[1] == 3) NULL else plotTitle, # plot title is part of the legend if legend is at the top
                cex.main = 0.95
            )
            mdiXYPlot(
                plot,
                dt,
                groupingCols = "group",
                # groupColors = groupColors,
                xlim = xlim_,
                ylim = ylim_,
                plotAs = plot$settings$get("Density_Plot","Plot_As"),
                histogramSpacing = d$pd$X_Bin_Size,
                underscoresToSpaces_ = underscoresToSpaces_,
                legendSide = legendSide,
                legendTitle = if(legendSide[1] == 4) NULL else plotTitle,
                showLegend = i == 1,
                showSingleGroupLegend = TRUE,
                vShade = if(is.list(vShade)) vShade[[trackLabel]] else vShade,
                v      = if(is.list(v))      v[[trackLabel]]      else v,
                h      = if(is.list(h))      h[[trackLabel]]      else h,
                groupV = if(is.list(groupV) && !is.null(groupV[[trackLabel]])) groupV[[trackLabel]] else groupV,
                groupH = if(is.list(groupH) && !is.null(groupH[[trackLabel]])) groupH[[trackLabel]] else groupH,
                vColor = if(is.list(vColor)) vColor[[trackLabel]] else vColor,
                hColor = if(is.list(hColor)) hColor[[trackLabel]] else hColor,
                ...
            )
            if(d$grouping$hasTracks) switch(
                trackLabelPosition,
                left = text(
                    xlim_[1] + diff(xlim_) * 0.0125, 
                    ylim_[2] * 0.75, 
                    trackLabel, 
                    pos = 4, offset = 0, cex = 0.85
                ),
                center = text(
                    mean(xlim_), 
                    ylim_[2], 
                    gsub("\n", ", ", trackLabel), 
                    pos = 1, offset = 0.5, cex = 0.85
                ),
                none = NULL
            )
        }
        stopSpinner(session)
    }
)

# return the plot
plot 
#----------------------------------------------------------------------
}
#----------------------------------------------------------------------
