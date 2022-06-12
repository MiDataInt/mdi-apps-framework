---
title: YAML Declarations
parent: Settings Panels
has_children: false
nav_order: 20
---

## {{page.title}}

Settings declarations can be made in most YAML configuration 
files, typically in _module.yml_.

### Creating settings definitions in module.yml

Settings popups are organized into input families, where
each named family is shown on its own tab in the modal panel.
Each input has its own unique ID.

The syntax for defining inputs will be familiar to anyone
creating R Shiny pages, as it captures the names
and most important options of Shiny inputs into YAML format:

```yml
settings:
    <settingsTabName>:
        <settingName>:
            type:   textInput
            value:  abc  
        <settingName>:
            type:   numericInput
            value:  1
            min:    1
            max:    100
            step:   1 
        <settingName>:
            type: selectInput
            choices:
                - abc
                - xyz
            value: abc    
        <settingName>:
            type: radioButtons
            choices:
                - abc
                - xyz
            value: abc    
        <settingName>:
            type: checkboxGroupInput
            choices:
                - abc
                - xyz
            value: abc 
        <settingName>:
            type: checkboxInput
            value: true 
        <settingName>:
            type:   fileInput
            accept: 
                - ".csv"
                - ".txt"
```

### Naming settings inputs and input families

The `settings` widget uses a convenience shortcut to prevent
you from having to enter both names and ids for settings.
The family/tab and input keys in YAML 
are used directly as the on-screen labels in the settings popup,
after replacing all underscores with spaces. 
This typically means you should use
title case for family and settings keys, e.g., "Plot_Parameters"
and "X_Axis".
