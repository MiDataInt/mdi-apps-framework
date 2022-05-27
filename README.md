# Michigan Data Interface

The [Michigan Data Interface](https://midataint.github.io/) (MDI) 
is a framework for developing, installing and running 
HPC data analysis pipelines and interactive visualization 
applications within a standardized design interface.

The MDI is separated into 
[two stages of code execution](https://midataint.github.io/docs/analysis-flow/) 
called Stage 1 HPC **pipelines** and Stage 2 web applications (i.e., **apps**).
Collectively, pipelines and apps are known as **tools**.

## Repository contents

This is the repository for the **MDI apps
framework**. It contains R Shiny code that
creates the modular graphical user interface (GUI) via
a web server and runs individual data analysis apps. 
The framework thus provides a common access point to many data apps.

The apps framework does not encode the data analysis apps themselves, 
which are found in other code repositories called 'tool suites'
created from our suite repository template:

- tool suite template: <https://github.com/MiDataInt/mdi-suite-template>

## Installation and use

This repository is not used directly. Instead, it is cloned
and managed by the MDI installer and manager utilities found here:

- MDI installation script: <https://github.com/MiDataInt/mdi>
- MDI manager R package: <https://github.com/MiDataInt/mdi-manager>
