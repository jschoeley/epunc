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
  calibration_table = 'out/21-calibration_table.rds',
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
  colors = list(
    cv_series = c(test = '#f51883', calibration = '#117396')
  )
})

# Load prediction intervals ---------------------------------------

prediction_intervals <- readRDS(paths$input$prediction_intervals)

# Calculate calibration scores ------------------------------------

calibration <- list()

calibration$by_season <-
  prediction_intervals %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters) %>%
  unnest(data, prediction) %>%
  filter(cv_id %in% cnst$cv_series_validation) %>%
  group_by(model_id, season) %>%
  summarise(
    COV = Coverage(observed, PIq1, PIq4, na.rm = TRUE),
    MIS = MIS(observed, PIq1, PIq4, alpha = 0.1, na.rm = TRUE)
  ) %>%
  ungroup()

calibration$total <-
  prediction_intervals %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters) %>%
  unnest(data, prediction) %>%
  filter(cv_id %in% cnst$cv_series_validation) %>%
  group_by(model_id) %>%
  summarise(
    COV = Coverage(observed, PIq1, PIq4, na.rm = TRUE),
    MIS = MIS(observed, PIq1, PIq4, alpha = 0.1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(season = 'Annual') %>%
  select(model_id, season, everything())

calibration$table <-
  bind_rows(
    calibration$by_season,
    calibration$total
  ) %>%
  arrange(model_id, season)

calibration$table

# Validation plots ------------------------------------------------

validation <- list()

validation$model_id_labs <- c(
  NB = '(a) Model based PIs (Negative Binominal)',
  SNO = '(b) Empirical PIs (SNO)',
  EQ = '(c) Empirical PIs (raw quantiles)'
)

validation$data <-
  prediction_intervals %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(cv_id %in% cnst$cv_series_validation, region_iso == 'DE') %>% 
  filter(model_id %in% c('SNO', 'NB', 'EQ')) %>%
  mutate(model_id = factor(model_id, names(validation$model_id_labs)))

validation$observed_vs_predicted_validation <-
  validation$data %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = predicted) +
  geom_ribbon(
    aes(x = weeks_since_test_start, ymin = PIq1, ymax = PIq4),
    color = NA, fill = 'grey70'
  ) +
  geom_point(aes(y = observed), color = cnst$colors$cv_series['test'],
             size = 0.2) +
  geom_line(color = '#860644') +
  facet_grid(
    cv_id ~ model_id,
    labeller = labeller(model_id = validation$model_id_labs)
  ) +
  scale_y_continuous(labels = ~scales::comma(., scale = 1e-3)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'Weekly deaths (×1,000)', x = 'Weeks into forecasting period')
validation$observed_vs_predicted_validation

validation$relative_errors_validation <-
  validation$data %>%
    ggplot() +
    aes(x = weeks_since_test_start, y = score) +
    geom_ribbon(
      aes(x = weeks_since_test_start, ymin = q1, ymax = q4),
      color = NA, fill = 'grey70', alpha=0.5
    ) +
    geom_hline(yintercept = 0, color = 'grey50') +
    geom_point(color = cnst$colors$cv_series['test'], size = 0.2) +
    facet_wrap(
      ~model_id, labeller = labeller(model_id = validation$model_id_labs)
    ) +
    scale_y_continuous() +
    scale_color_manual(
      values = cnst$colors$cv_series
    ) +
    figspec$MyGGplotTheme(axis = 'xy') +
    guides(color = 'none') +
    labs(y = 'log(observed/predicted)', x = 'Weeks into forecasting period')
validation$relative_errors_validation

# Export ----------------------------------------------------------

saveRDS(calibration$table, paths$output$calibration_table)

ExportFigure(
  validation$relative_errors_validation,
  paths$output$fig,
  filename = '21-relative_errors_validation',
  device = 'pdf',
  width = config$figure$width,
  height = config$figure$width*0.5
)

ExportFigure(
  validation$observed_vs_predicted_validation,
  paths$output$fig,
  filename = '21-observed_vs_predicted_validation',
  device = 'pdf',
  width = config$figure$width,
  height = config$figure$width*0.5
)
