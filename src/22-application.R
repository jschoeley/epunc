# What does the script do?
#
# How does the script do it?

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

# Load ------------------------------------------------------------

prediction_intervals <- readRDS(paths$input$prediction_intervals)

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
    region_iso == 'DE'
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
  #facet_wrap(~ region_iso) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 0.20)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.65),
        legend.text = element_text(size = 8)) +
  #guides(color = 'none') +
  labs(y = 'p-value', x = 'Weeks into forecasting period',
       title = 'p-value of 10% excess deaths given H0: "continuation of past trends"')
pvalue10p

pvalue10p_byregion <-
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
  facet_wrap(~ region_iso) +
  scale_y_continuous(labels = scales::comma,
                     limits = c(0, 0.20)) +
  figspec$MyGGplotTheme(axis = 'xy') +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.65),
        legend.text = element_text(size = 8)) +
  #guides(color = 'none') +
  labs(y = 'p-value', x = 'Weeks into forecasting period',
       title = 'p-value of 10% excess deaths given H0: "continuation of past trends"')
pvalue10p_byregion

# 90% Power P-Score -----------------------------------------------

# How high must excess deaths be so that they fall outside of the 95%
# prediction interval?
fig$powerp95 <-
  forecasting_error$fitted %>%
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) %>%
  unnest(prediction) %>%
  filter(cv_id %in% cnst$cv_series_calibration) %>%
  filter(region_iso %in% c('DE'), model_id %in% c('NB', 'SNO')) %>%
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
fig$powerp95

ExportFigure(
  fig$powerp95, paths$output$figure,
  filename = 'power_gam_nb_de',
  device = 'pdf', width = 150
)

# Export ----------------------------------------------------------

ExportFigure(
  fig$pvalue10p, paths$output$figure,
  filename = 'pvalue_nb_de',
  device = 'pdf', width = 150
)

ExportFigure(
  fig$pvalue10p_appendix, paths$output$figure,
  filename = 'pvalue_nb_appendix',
  device = 'pdf', width = 150, height = 150
)

# export results of analysis
