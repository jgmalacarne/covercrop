# init.R
#
# Example R code to install packages if not already installed
#
my_packages = c("bslib", "dplyr", "ggplot2", "gplots", "heatmaply","htmlwidgets",
                "plotly","readxl","rmarkdown","shiny","shinyalerts","shinyBS",
                "shinydashboard","shinydashboardPlus","shinyjs","shinyWidgets","shinyvalidate","stringr","tidyverse","kableExtra")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}
invisible(sapply(my_packages, install_if_missing))