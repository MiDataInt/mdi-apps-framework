---
title: Interactive Settings
has_children: false
nav_order: 40
---

## {{page.title}}

The MDI apps framework encourages (but never demands) an uncluttered
UI for your app. In part, this means not placing potentially dozens 
of option inputs onto the main UI page. We encourage you to expose
inputs to users as they need them in an organized fashion.

We help you achieve this through a relatively simple YAML-based
"settings" module, whose resulting "gear" icon can be attached to 
an appStep page or any widget in your UI. Users will quickly become
accustomed to looking under that icon for context-relevant settable options.



### Step-level settings

Many appStep modules define setting, i.e., option inputs, that
are relevant to most or all of the output elements on its page.

```yml
settings:
    <settingsTabName>:
        <settingName>:
            type:   checkboxInput
            value:  false
        <settingName>:
            type:  selectInput
            choices: 
                - abc
                - 123
            value: abc      
        <settingName>:
            type:   textInput
            value:  abc123
        # etc.
```


