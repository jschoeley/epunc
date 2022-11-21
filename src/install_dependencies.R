# Install dependencies

dependencies <- c(
  "doParallel", "dplyr", "foreach",
  "gamlss", "ggplot2", "lubridate",
  "MASS", "mgcv", "scales", "showtext",
  "tidyverse", "yaml"
)

install.packages(
  dependencies, dep = TRUE
)
