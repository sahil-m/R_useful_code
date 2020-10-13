library(utils)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages_always_required <- c("tictoc", "readr", "readxl", "skimr", "Hmisc", "zoo", "plotly", "plyr", "reshape2", "magrittr", "assertthat", "tidyverse", "glue", "stringdist", "ggthemes", "RColorBrewer", "lubridate", "janitor", "assertr", "grid", "ggforce", "knitr", "DT")

packages_hierarchy <- c("treemap", "data.tree", "networkD3")

packages_models_basic <- c("caret", "rpart", "rpart.plot", "Metrics", "h2o")

packages_time_series <- c("manipulate", "xts", "timetk", "tidyquant", "dygraphs", "forecast", "stR", "prophet")

packages_database <- c("ROracle", "DBI")

packages <- c(packages_always_required)

ipak(packages)

options("scipen"=100, "digits"=4) #### Use this to not display no. in exponent format in R
