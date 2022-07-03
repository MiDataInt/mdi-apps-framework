#----------------------------------------------------------------------
# custom inputs for config file editing
# mimic Shiny inputs but with additional features
#----------------------------------------------------------------------

# generic form for "text" input used for strings, integers and doubles
mdiBoxInputHtml <- function(id, type, Type, step, value, placeholder, onchangeFn){
    paste0(
        '<input ',
            'id="', id, '" ',
            'type="', type, '" ',
            if(!is.null(step)) paste0('step="', step, '"') else "",
            'class="form-control shiny-bound-input shinyjs-resettable" ',
            if(!is.null(value)) paste0('value="', value, '"') else "",
            'placeholder="', placeholder, '" ',
            'data-shinyjs-resettable-id="', id,  '" ',
            'data-shinyjs-resettable-type="', Type, '" ',
            'data-shinyjs-resettable-value="', if(is.null(value)) "" else value, '" ',
            if(!is.null(onchangeFn)) paste0('onchange="', onchangeFn, '(this)" ') else " ",
        '>'
    )     
}
mdiBoxInput <- function(id, label, value = NULL, 
                            placeholder = "", onchangeFn = NULL,
                            type, Type, step = NULL, indexed = TRUE){
    if(is.null(value) || length(value) == 0) value <- ""
    tags$div(
        class = "form-group shiny-input-container",
        # create one label ...
        tags$label(
            class = "control-label",
            id = paste(id, "label", sep = "-"),
            'for' = id,
            label
        ),
        # ... for an array of one or more similar inputs
        HTML(if(indexed || length(value) > 1) paste0(sapply(seq_along(value), function(i){
            mdiBoxInputHtml(paste(id, i, sep = "_"), type, Type, step, value[i], placeholder, onchangeFn)   
        })) 
        else mdiBoxInputHtml(id, type, Type, step, value, placeholder, onchangeFn))   
    )
}

# string inputs
mdiTextInput <- function(id, label, value = NULL, placeholder = "", onchangeFn = NULL, indexed = TRUE){
    mdiBoxInput(id, label, value, placeholder, onchangeFn, "text",   "Text", indexed = indexed)
}
# integer inputs
mdiIntegerInput <- function(id, label, value = NULL, placeholder = "", onchangeFn = NULL, indexed = TRUE){
    mdiBoxInput(id, label, value, placeholder, onchangeFn, "number", "Numeric", step = 1, indexed = indexed)
} 
# double inputs
mdiDoubleInput <- function(id, label, value = NULL, placeholder = "", onchangeFn = NULL, indexed = TRUE){
    mdiBoxInput(id, label, value, placeholder, onchangeFn, "number", "Numeric", step = 0.01, indexed = indexed)
} 

# logical inputs
mdiCheckboxGroupInputDiv <- function(id, onchangeFn, checked){
    tags$div(
        class = "shiny-options-group",
        tags$div(
            class = "checkbox",
            tags$label(
                HTML(paste0(
                    '<input type="checkbox" name="', id, '" value="" ', 
                    if(!is.null(onchangeFn)) paste0('onchange="', onchangeFn, '(this)" ') else " ",
                    checked, 
                    '>'
                ))
            )
        )
    )    
}
mdiCheckboxGroupInput <- function(id, label, value = 0, onchangeFn = NULL, indexed = TRUE){
    if(is.null(value) || length(value) == 0) value <- FALSE
    labelId <- paste(id, "label", sep = "-")
    checked <- function(i) if(as.logical(as.integer(value[i]))) 'checked="checked"' else ""
    tags$div(
        id = id,
        class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input shinyjs-resettable",
        role = "group",
        'aria-labelledby' = labelId,
        'data-shinyjs-resettable-id' = id,
        'data-shinyjs-resettable-type' = "CheckboxGroup",
        'data-shinyjs-resettable-value' = "[]",
        tags$label(
            class = "control-label",
            id = labelId,
            'for' = id,
            label
        ),
        if(indexed || length(value) > 1) lapply(seq_along(value), function(i){
            mdiCheckboxGroupInputDiv(paste(id, i, sep = "_"), onchangeFn, checked(i))   
        })
        else mdiCheckboxGroupInputDiv(id, onchangeFn, checked(value))  
    )
}
