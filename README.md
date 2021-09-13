
# Michigan Data Interface

The Michigan Data Interface (MDI) is a framework for developing,
installing and running a variety of HPC data analysis pipelines
and interactive R Shiny data visualization applications
within a standardized design and implementation interface.

## Organization and contents

### MDI code stages

Data analysis in the MDI is logically separated
into two stages of code execution called Stage 1 HPC **pipelines**
and Stage 2 web applications (i.e., **apps**).

### Repository contents

This is the repository for the **MDI apps
framework**. It contains R Shiny-based code that
creates the modular graphical user interface (GUI) via
a web server and runs individual data analysis apps
associated with user data files. The framework thus provides
a common access point into many data apps.

For the most part, the apps framework does not encode
the data analysis apps themselves, which are found in other
code repositories called 'apps suites'. The main exception
is that the framework hosts the Pipeline Runner app, which allows
Stage 1 pipelines to be configured and run via the web interface.
It also includes a template to start development of a new app.

## Usage

This repository is never accessed directly. It is installed and 
managed by the MDI manager utility found here:

https://github.com/MiDataInt/mdi

