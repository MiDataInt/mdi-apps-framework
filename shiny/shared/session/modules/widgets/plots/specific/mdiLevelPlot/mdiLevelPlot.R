#----------------------------------------------------------------------
# render a non-interactive level plot with MDI formatting
#----------------------------------------------------------------------
mdiLevelPlot <- function(
    dt,     # a data.table with at least columns x, y, and the column named by z.column
    xlim,   # the plot X-axis limits
    xinc,   # the regular increment of the X-axis grid
    ylim,   # the plot Y-axis limits 
    yinc,   # the regular increment of the Y-axis grid 
    z.fn,   # function applied to z.column, per grid spot, to generate the output color
    z.column, # the column in dt passed to z.fn, per grid spot
    settings, # a settings object from the enclosing staticPlotBox, or any list compatible with mdiLevelPlotSettings
    legendTitle, # header for the color legend
    h = NULL, # Y-axis values at which to place line rules
    v = NULL, # X-axis values at which to place line rules
    border = NA, # passed to rect, suppresses border by default
    ... # additional arguments passed to rect()
){ 
#----------------------------------------------------------------------
if(!is.null(settings$all_)) settings <- settings$all()
if(!is.null(settings$Level_Plot)) settings <- settings$Level_Plot
palettes <- mdiLevelPalettes()
levelPalette <- strsplit(settings$Level_Palette$value, " ")[[1]]
nColors <- palettes[paletteName == levelPalette[2], maxcolors]
palette <- brewer.pal(nColors, levelPalette[2])
nColorsPerSide <- if(levelPalette[1] == "seq") 9 else 6
dt <- as.data.table(dt)[
    x >= xlim[1] & x <= xlim[2] &
    y >= ylim[1] & y <= ylim[2], 
    .( 
        z = switch(
            z.column, 
            x = z.fn(rep(x, .N)), 
            y = z.fn(rep(y, .N)),
            z.fn(.SD[[z.column]])
        )
    ), 
    keyby = .(x, y)
]
maxZ <- trimws(settings$Max_Z_Value$value)
if(maxZ == "auto" || maxZ == "") maxZ <- dt[, max(z)]
maxZ <- as.numeric(maxZ)
map <- dt[, .(
    x = x,
    y = y,
    color = sapply(z, function(z) switch(
        levelPalette[1],
        seq = palette[ceiling(min(abs(z), maxZ) / maxZ * nColorsPerSide)], # sequential go from 0 to maxZ
        div = ifelse( # divided go from -maxZ to maxZ
            z < 0,
            palette[nColorsPerSide + 1 - floor(  max(z, -maxZ) / -maxZ * nColorsPerSide)],
            palette[nColorsPerSide - 1 + ceiling(min(z,  maxZ) /  maxZ * nColorsPerSide)]
        )
    ))
)]
rect(
    map$x - xinc / 2, 
    map$y - yinc / 2, 
    map$x - xinc / 2 + xinc, 
    map$y - yinc / 2 + yinc, 
    col = map$color, 
    border = border, 
    ...
)    
ruleColor <- addAlphaToColor(palette[nColors], 0.2)
if(!is.null(h)) abline(h = h, col = ruleColor)
if(!is.null(v)) abline(v = v, col = ruleColor)

#----------------------------------------------------------------------
# add the margin legend
#----------------------------------------------------------------------
par(xpd = TRUE)
xspan <- diff(xlim)
yspan <- diff(ylim)
x <- xlim[2] * 1.1
y <- ylim[1] + yspan * 0:(nColors - 1) / nColors
xinc <- xspan / 20
yinc <- yspan / nColors
z <- switch(
    levelPalette[1],
    seq = {
        z <- as.character(round(maxZ * 1:nColors / nColors, settings$Legend_Digits$value))
        z[nColors] <- paste(">=", z[nColors])
        z
    },
    div = {
        zinc <- round(maxZ * 1 / nColorsPerSide, settings$Legend_Digits$value)
        zhgh <- round(maxZ * 2:nColorsPerSide / nColorsPerSide, settings$Legend_Digits$value)
        z <- as.character(c(rev(zhgh * -1), 0, zhgh))
        z[nColors] <- paste(">=",  maxZ)
        z[nColorsPerSide] <- paste(-zinc, "to", zinc)
        z[1]       <- paste("<=", -maxZ)
        z
    }
)
rect( # legend color blocks
    x, 
    y, 
    x + xinc, 
    y + yinc, 
    col = palette, 
    border = border,
    ...
)    
text( # legend z value labels
    x + xinc * 1.5,
    y + yinc / 2,
    z,
    adj = 0,
    las = 2,
    cex = 0.85
)
text( # legend title
    x,
    ylim[2] + yinc / 2,
    legendTitle,
    adj = 0,
    las = 2,
    cex = 0.9
)
par(xpd = FALSE)
#----------------------------------------------------------------------
}
