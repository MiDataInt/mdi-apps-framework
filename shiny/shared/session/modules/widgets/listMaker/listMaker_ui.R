#----------------------------------------------------------------------
# static components for a widget that enables users to construct an
# arbitrarily long list of an element class defined by the caller
#----------------------------------------------------------------------

# module ui function
listMakerUI <- function(id, itemName, columnHeader="", sortable_options=NULL) {
    if(is.null(sortable_options)) sortable_options <- sortable_options()
    
    # initialize namespace
    ns <- NS(id)

    # return the UI contents
    tags$div(
        tags$p(bsButton(ns('addItem'), paste('Add', itemName), style = "info")),
        columnHeader,
        rank_list(
            text = "",
            NULL,
            ns('list'),
            css_id = ns('list'),
            options = sortable_options,
            class = "default-sortable"
        )
    )
}
