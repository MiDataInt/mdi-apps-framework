# Michigan Data Interface

The [Michigan Data Interface](https://midataint.github.io/) (MDI) is a framework for developing,
installing and running a variety of HPC data analysis pipelines
and interactive R Shiny data visualization applications
within a standardized design and implementation interface.

## Organization and contents

### MDI code stages

Data analysis in the MDI is logically separated
into 
[two stages of code execution](https://midataint.github.io/docs/analysis-flow/) 
called Stage 1 HPC **pipelines**
and Stage 2 web applications (i.e., **apps**).

### Repository contents

This is the repository for the **MDI apps
framework**. It contains R Shiny-based code that
creates the modular graphical user interface (GUI) via
a web server and runs individual data analysis apps
associated with user data files. The framework thus provides
a common access point to many data apps.

For the most part, the apps framework does not encode
the data analysis apps themselves, which are found in other
code repositories called 'apps suites'. The main exception
is that the framework holds the Pipeline Runner app, which allows
Stage 1 pipelines to be configured and run via the web interface.

### Related repositories

Code developers are directed to this repository for a template to
**create your own apps suites**:

- <https://github.com/MiDataInt/mdi-apps-suite-template>

## Prerequisites

**R** is required to install the MDI manager, which is in turn used 
to install the MDI apps, and also to run the Shiny web server.

<https://www.r-project.org/>

## Installation and usage

### Apps framework and suites installation

This repository is not used directly. Instead, it is cloned
and managed by the MDI manager utility found here:

<https://github.com/MiDataInt/mdi-manager>

Please follow the manager installation instructions, being sure
to update the apps suites you wish to install in 'mdi/config.yml':

```
# mdi/config.yml
suites:
    apps:
        - https://github.com/GIT_USER/SUITE_NAME-mdi-apps.git
```

and then running' 'mdi::install()' a second time, or calling 
'mdi install' on the command line. 
You will also be able install new apps suites from within the 
MDI web page.
