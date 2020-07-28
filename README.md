Project 3
================
Yu Jiang
7/27/2020

This repo is for the final project for Summer20 ST558 and two R scripts,
ui.R and server.R are included.

Before run this app, please install these libraries below and then check
whether they have been installed completely by loading them,

``` r
# install.packages(c("shiny", "shinydashboard", "shinythemes", "dplyr", "ggplot2", "data.table",
#                    "caret",  "rpart", "gridExtra", "lattice", "e1071", "randomForest", "plotly"))

library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(data.table)
library(caret)
library(rpart)
library(gridExtra)
library(lattice)
library(e1071)
library(randomForest)
library(plotly)
```

After all the libraries have been loaded, this shiny APP can be run
directly from gitHub using RStudio by running
**shiny::runGitHub(“ST558-Summer20-Project-3”, “jiangyu1208”)** in the
console.
