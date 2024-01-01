#----------------------------------------------------------------------
# construct settings expected by specific mdi plot widgets (order is important)
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# mdiLevelPlot
#----------------------------------------------------------------------
mdiLevelPalettes <- function(){
    palettes <- brewer.pal.info
    palettes$paletteName <- row.names(palettes)
    #          maxcolors category colorblind
    # BrBG            11      div       TRUE
    # Blues            9      seq       TRUE
    as.data.table(palettes)[category != "qual"]
}
mdiLevelPlotSettings <- list(
    Level_Plot = list(
        Max_Z_Value = list(
            type = "textInput",
            value = "auto"
        ),
        Level_Palette = list(
            type = "selectInput",
            choices = mdiLevelPalettes()[, paste(category, paletteName)],
            value = "seq Blues"
        ),
        Legend_Digits = list(
            type = "numericInput",
            value = 1,
            min = 0,
            max = 10,
            step = 1
        )
    )
)

#----------------------------------------------------------------------
# mdiXYPlot
#----------------------------------------------------------------------
mdiXYPlotSettings <- list(
    XY_Plot = list(
        Point_Order = list(
            type = "radioButtons",
            choices = c("random","group"),
            value = "random"
        ),
        Color_Alpha = list(
            type = "numericInput",
            value = 1,
            min = 0,
            max = 1,
            step = 0.05
        ),
        X_Jitter_Amount = list(
            type = "textInput",
            value = ""
        ),
        Group_Order = list(
            type = "selectInput",
            choices = c("alphabetical","as encountered"),
            value = "alphabetical"
        ),
        Y_Jitter_Amount = list(
            type = "textInput",
            value = ""
        ),
        Reverse_Group_Order = list(
            type = "checkboxInput",
            value = FALSE
        )
    )
)

#----------------------------------------------------------------------
# mdiDensityPlot
#----------------------------------------------------------------------
mdiDensityPlotSettings <- c(list(
    Density_Plot = list(
        Min_X_Value = list(
            type = "textInput",
            value = "auto"
        ),
        Max_X_Value = list(
            type = "textInput",
            value = "auto"
        ),
        X_Bin_Size = list(
            type = "numericInput",
            value = 1
        ),
        Y_Axis_Value = list(
            type = "radioButtons",
            choices = c("Frequency","Count"),
            value = "Frequency"
        ),
        Plot_As = list(
            type = "radioButtons",
            choices = c("lines","area","histogram"),
            value = "lines"
        )
    )
), mdiXYPlotSettings)
