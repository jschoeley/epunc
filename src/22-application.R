# Apply empirical prediction intervals and derive metrics of interest

# Init ------------------------------------------------------------

library(yaml)
library(dplyr)
library(ggplot2)
library(tidyr)

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
  figure = 'out'
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

# Load ------------------------------------------------------------

prediction_intervals <- readRDS(paths$input$prediction_intervals)

# Germany application ---------------------------------------------

fig$application <- list()

fig$application$model_id_labs <- c(
  NB = '(a) Negative Bin. PIs',
  SNO = '(b) Empirical SN PIs',
  EQ = '(c) Empirical RQ PIs'
)

fig$application$data <-
  prediction_intervals |>
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) |>
  unnest(prediction) |>
  filter(cv_id %in% cnst$cv_series_application, region_iso == cnst$example_country) |> 
  filter(model_id %in% c('SNO', 'NB', 'EQ')) |>
  mutate(
    model_id = factor(model_id, names(fig$application$model_id_labs),
                      fig$application$model_id_labs)
  )

fig$application$plot <-
  fig$application$data |>
  ggplot() +
  aes(x = weeks_since_test_start, y = predicted) +
  geom_linerange(
    aes(x = weeks_since_test_start, ymin = PIq1, ymax = PIq6),
    color = 'grey70', size = 1.5
  ) +
  geom_point(aes(y = observed), color = cnst$colors$cv_series['test'],
             size = 0.2) +
  geom_line(color = '#860644') +
  facet_wrap(
    ~ model_id, nrow = 1
  ) +
  scale_y_continuous(labels = ~scales::comma(., scale = 1e-3)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'Weekly deaths (×1,000)', x = 'Weeks into forecasting period')

fig$application_by_country$data <- 
  prediction_intervals |>
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) |>
  unnest(prediction) |>
  filter(cv_id %in% cnst$cv_series_application,
         region_iso %in% config$countries$analysis) |> 
  filter(model_id %in% c('SNO')) |>
  mutate(model_id = factor(model_id, names(fig$model_id_labs), fig$model_id_labs))

fig$application_by_country$plot <-
  fig$application_by_country$data |>
  ggplot() +
  aes(x = weeks_since_test_start, y = predicted) +
  geom_linerange(
    aes(x = weeks_since_test_start, ymin = PIq1, ymax = PIq6),
    color = 'grey70', size = 1.5
  ) +
  geom_point(aes(y = observed), color = cnst$colors$cv_series['test'],
             size = 0.2) +
  geom_line(color = '#860644') +
  facet_wrap(
    ~region_name, ncol = 4, scales = 'free_y'
  ) +
  scale_y_continuous(labels = ~scales::comma(., scale = 1e-3)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'Weekly deaths (×1,000)', x = 'Weeks into forecasting period')

# 10% P-Score -----------------------------------------------------

# What's the probability to observe a P-Score of 10% in the
# second fall of the COVID-19 pandemic, under the Null
# Hypothesis of continuing of pre-COVID trends and associated
# distribution of mortality fluctuations?

pvalue10p <-
  prediction_intervals %>%
  dplyr::select(-predicted_parameters, -prediction) %>%
  unnest(predicted_quantiles) %>%
  filter(
    model_id %in% c('NB', 'SNO'),
    region_iso == cnst$example_country
  ) %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = pscore_10p, color = model_id) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.05,
            color = NA, fill = 'grey90') +
  geom_step(
    #   color = '#860644'
  ) +
  #scale_color_manual(
  #  values = c('#117396', '#f51883'),
  #  labels = c("Negative Binomial PI", "Empirical PI")
  #) +
  facet_wrap(~region_name) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 0.20)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.65),
        legend.text = element_text(size = 8)) +
  #guides(color = 'none') +
  labs(y = 'p-value', x = 'Weeks into forecasting period',
       title = 'Weekly probability of observing 10% excess deaths under pre-pandemic mortality')
pvalue10p

fig$pvalue10p_byregion <-
  prediction_intervals %>%
  dplyr::select(-predicted_parameters, -prediction) %>%
  unnest(predicted_quantiles) %>%
  filter(
    model_id %in% c('NB', 'SNO'),
    region_iso %in% config$countries$analysis
  ) %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = pscore_10p, color = model_id) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.05,
            color = NA, fill = 'grey90') +
  geom_step(
    #   color = '#860644'
  ) +
  scale_color_manual(
   values = c('#117396', '#f51883'),
   labels = c("Negative Binomial PI", "Empirical PI")
  ) +
  facet_wrap(~region_name, ncol = 4) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 0.251)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.65),
        legend.text = element_text(size = 8)) +
  #guides(color = 'none') +
  labs(y = 'P(excess > 10%)', x = 'Weeks into forecasting period',
       title = 'Weekly probability of exceeding 10% excess deaths under pre-pandemic mortality')
fig$pvalue10p_byregion

# 90% Power P-Score -----------------------------------------------

# How high must excess deaths be so that they fall outside of the 95%
# prediction interval?
powerp95 <-
  prediction_intervals %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(cv_id %in% cnst$cv_series_calibration) %>%
  filter(region_iso %in% c('FI'), model_id %in% c('NB', 'SNO')) %>%
  group_by(model_id, region_iso, weeks_since_test_start) %>%
  summarise(
    threshold = mean(q4, na.rm = TRUE)
  ) %>%
  ggplot() +
  aes(x = weeks_since_test_start, y = threshold, color = model_id) +
  geom_line(
    #   color = '#860644'
  ) +
  facet_wrap(~ region_iso) +
  scale_y_continuous(labels = scales::percent, n.breaks = 10,
                     limits = c(0, 0.30)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(
    y = '% excess deaths at 95% prediction interval of expected',
    x = 'Weeks into forecasting period',
    title = 'Detection limits of excess deaths vary by season'
  )
powerp95

# Export ----------------------------------------------------------

# export results of analysis
ExportFigure(
  fig$application$plot, paths$output$figure,
  filename = '22-application',
  device = 'pdf',
  width = config$figure$width,
  height = config$figure$width*0.5
)

ExportFigure(
  fig$application_by_country$plot, paths$output$figure,
  filename = '22-applicationbycountry',
  device = 'pdf',
  width = config$figure$width,
  height = config$figure$width*1.3
)

ExportFigure(
  fig$pvalue10p_byregion, paths$output$figure,
  filename = '22-pvalue10p_byregion',
  device = 'pdf',
  width = config$figure$width,
  height = config$figure$width*1.3
)
