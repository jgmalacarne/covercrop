# run.R
library(shiny)

# Example R code to install packages if not already installed
#
my_packages = c("bslib", "broom", "cli", "conflicted", "dbplyr", 
                "dplyr", "dtplyr", "forcats", "ggplot2", "gplots", 
                "haven", "heatmaply", "hms", "htmlwidgets", "httr",
                "jsonlite", "lubridate", "magrittr", "modelr", "pillar", 
                "plotly", "purrr", "ragg", "readr", "readxl","rmarkdown", "reprex",
                "rlang", "rstudioapi", "rvest", "shiny","shinyalert","shinyBS",
                "shinydashboard","shinydashboardPlus","shinyjs","shinyWidgets",
                "shinyvalidate","stringr", "tibble", "tidyr","kableExtra", "xml2", "tidyverse")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))


port <- Sys.getenv('PORT')
shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)