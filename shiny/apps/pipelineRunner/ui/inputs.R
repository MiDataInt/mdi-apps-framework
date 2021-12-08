#----------------------------------------------------------------------
# custom inputs for config file editing
# mimic Shiny inputs but with additional features
#----------------------------------------------------------------------
customTextInput <- function(id, label, value = NULL, placeholder = "", onchangeFn = NULL){
    tags$div(
        class = "form-group shiny-input-container",
        tags$label(
            class = "control-label",
            id = paste(id, "label", sep = "-"),
            'for' = id,
            label
        ),
        HTML(paste0(
            '<input ',
                'id="', id, '" ',
                'type="text" ',
                'class="form-control shiny-bound-input shinyjs-resettable" ',
                if(!is.null(value)) paste0('value="', value, '"') else "",
                'placeholder="', placeholder, '" ',
                'data-shinyjs-resettable-id="', id,  '" ',
                'data-shinyjs-resettable-type="Text" ',
                'data-shinyjs-resettable-value="" ',
                if(!is.null(onchangeFn)) paste0('onchange="', onchangeFn, '(this)" ') else " ",
            '>'
        ))      
    )
}
customCheckboxGroupInput <- function(id, label, value = 0, onchangeFn = NULL){
    labelId <- paste(id, "label", sep = "-")
    checked <- if(as.logical(as.integer(value))) 'checked="checked"' else ""
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
    )
}
