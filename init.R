# init.R
#
# Install packages if not already installed
#
my_packages = c("bslib",
                "dplyr",
                "forcats",
                "ggplot2",
                "gplots",
                "heatmaply",
                "htmlwidgets",
                "plotly",
                "purrr",
                "readr",
                "readxl",
                "rmarkdown",
                "shiny",
                "shinyalert",
                "shinyBS",
                "shinydashboard",
                "shinydashboardPlus",
                "shinyjs",
                "shinyWidgets",
                "shinyvalidate",
                "stringr",
                "tibble",
                "tidyr",
                "kableExtra")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
}
invisible(sapply(my_packages, install_if_missing))

