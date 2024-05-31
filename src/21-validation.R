# Validate the prediction intervals

# Init ------------------------------------------------------------

library(yaml)
library(dplyr)
library(tidyr)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  config = 'cfg/config.yaml',
  prediction_intervals = 'out/20-prediction_intervals.rds'
)
paths$output <- list(
  validation_table = 'out/21-validation_table.rds',
  fig = 'out'
)

# global configuration
config <- read_yaml(paths$input$config)

# global functions
source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  cv_series_calibration = 1:5
  cv_series_validation = 6:7
  cv_series_application = 8
  colors = list(
    cv_series = c(test = '#f51883', calibration = '#117396')
  )
  example_country = 'DE'
})

fig <- list()

# Load prediction intervals ---------------------------------------

prediction_intervals <- readRDS(paths$input$prediction_intervals)

# Calculate calibration scores ------------------------------------

validation <- list()

validation$by_season <-
  prediction_intervals |>
  dplyr::select(-predicted_quantiles,
                -predicted_parameters) |>
  unnest(data, prediction) |>
  filter(cv_id %in% cnst$cv_series_validation) |>
  group_by(model_id, season) |>
  mutate(
    logobserved = log(observed),
    logPIq1 = log(PIq1),
    logPIq6 = log(PIq6)
  ) |>
  summarise(
    COV = Coverage(logobserved, logPIq1, logPIq6, na.rm = TRUE),
    MIS = MIS(logobserved, logPIq1, logPIq6, alpha = 0.05, na.rm = TRUE)
  ) |>
  ungroup()

validation$total <-
  prediction_intervals |>
  dplyr::select(-predicted_quantiles,
                -predicted_parameters) |>
  unnest(data, prediction) |>
  filter(cv_id %in% cnst$cv_series_validation) |>
  group_by(model_id) |>
  mutate(
    logobserved = log(observed),
    logPIq1 = log(PIq1),
    logPIq6 = log(PIq6)
  ) |>
  summarise(
    COV = Coverage(logobserved, logPIq1, logPIq6, na.rm = TRUE),
    MIS = MIS(logobserved, logPIq1, logPIq6, alpha = 0.05, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(season = 'Annual') |>
  select(model_id, season, everything())

validation$table <-
  bind_rows(
    validation$by_season,
    validation$total
  ) |>
  arrange(model_id, season)

xtabs(
  cbind(COV, MIS) ~ model_id + season,
  data = validation$table
)

# Validation plots ------------------------------------------------

fig$model_id_labs <- c(
  NB = '(a) Negative Bin. PIs',
  SNO = '(b) Empirical SN PIs',
  EQ = '(c) Empirical RQ PIs'
)

fig$validation$data <-
  prediction_intervals |>
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) |>
  unnest(prediction) |>
  filter(cv_id %in% cnst$cv_series_validation, region_iso == cnst$example_country) |> 
  filter(model_id %in% c('SNO', 'NB', 'EQ')) |>
  mutate(model_id = factor(model_id, names(fig$model_id_labs), fig$model_id_labs))

fig$validation$plot <-
  fig$validation$data |>
  ggplot() +
  aes(x = weeks_since_test_start, y = predicted) +
  geom_ribbon(
    aes(x = weeks_since_test_start, ymin = PIq1, ymax = PIq6),
    color = NA, fill = 'grey70'
  ) +
  geom_point(aes(y = observed), color = cnst$colors$cv_series['test'],
             size = 0.2) +
  geom_line(color = '#860644') +
  facet_grid(
    cv_id ~ model_id,
  ) +
  scale_y_continuous(labels = ~scales::comma(., scale = 1e-3)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'Weekly deaths (×1,000)', x = 'Weeks into forecasting period')
fig$validation$plot

# Export ----------------------------------------------------------

saveRDS(validation$table, paths$output$validation_table)

ExportFigure(
  fig$validation$plot,
  paths$output$fig,
  filename = '21-validation',
  device = 'pdf',
  width = config$figure$width,
  height = config$figure$width*0.5
)
