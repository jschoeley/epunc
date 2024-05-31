# Calibrate prediction intervals
#
# 1) Calibrate prediction intervals on calibration series
# 2) Calculate calibration scores on the validation series
# 3) Contruct prediction intervals around the application series

# Init ------------------------------------------------------------

library(yaml)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gamlss)
library(doParallel)

# Constants -------------------------------------------------------

set.seed(1987)

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  config = 'cfg/config.yaml',
  deathcounts_cv = 'tmp/10-deathcounts_cv.rds'
)
paths$output <- list(
  fig = 'out',
  log = 'tmp/log.txt',
  prediction_intervals = 'out/20-prediction_intervals.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# global functions
source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  quantiles = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)
  # how many threads used to fit models?
  cpu_nodes = 14
  cv_series_calibration = 1:5
  cv_series_validation = 6:7
  cv_series_application = 8
  colors = list(
    cv_series = c(test = '#f51883', calibration = '#117396')
  )
  example_country = 'DE'
})

# list containers for analysis artifacts
fig <- list()

# setup parallel computation
cnst$cl <- makeCluster(cnst$cpu_nodes, outfile = paths$output$log)
registerDoParallel(cnst$cl)

# Nonconformity scores --------------------------------------------

# this functions allows for implements different nonconformity scores
# (observed, predicted) -> score and their inverse
# (score, predicted) -> observed
NonconformityScore <- function (type = 'logratio') {
  switch (type,
    'logratio' = list(
      Score = function (observed, predicted) {
        log(observed) - log(predicted)
      },
      InverseScore = function (score, predicted) {
        exp(score + log(predicted))
      }
    ),
    'residual' = list(
      Score = function (observed, predicted) {
        observed - predicted
      },
      InverseScore = function (score, predicted) {
        score + predicted
      }
    )
  )
}

# Load cross validation series ------------------------------------

deathcounts_cv <- readRDS(paths$input$deathcounts_cv)

# Subset cross validation data ------------------------------------

data_cv_sub <-
  deathcounts_cv |>
  filter(
    region_iso %in% config$countries$analysis,
    sex == 'Total', age_group == 'Total', cv_id != 0
  ) |>
  mutate(
    loop_strata = interaction(cv_id, region_iso)
  )

# Predict deaths over cv series -----------------------------------

predictions <- list()

predictions$cv <-
  CountGAM(
    data_cv_sub,
    observed ~
      1 + origin_weeks + s(epi_week, bs = 'cp') +
      offset(log(exposure)),
    family = nb(link = 'log'),
    col_sample = 'cv_sample',
    col_stratum = 'loop_strata',
    nsim = 500
  )

# Plot cv series and predictions ----------------------------------

fig$cv <-
  predictions$cv |>
  mutate(facet_label = case_when(
    cv_id %in% cnst$cv_series_calibration ~
      paste0('CV series ', formatC(cv_id, width = 2), ' (Calibration)'),
    cv_id %in% cnst$cv_series_validation ~
      paste0('CV series ', formatC(cv_id, width = 2), ' (Validation)'),
    cv_id %in% cnst$cv_series_application ~
      paste0('CV series ', formatC(cv_id, width = 2), ' (Application)')
  )) |>
  filter(region_iso == 'FI') |>
  ggplot() +
  aes(x = date, y = observed*1e-3, color = cv_sample) +
  geom_vline(
    aes(xintercept = date),
    data =
      . %>% group_by(facet_label) %>% summarise(date = origin_date_test[1]),
    color = 'grey', linewidth = 0.2
  ) +
  geom_vline(
    aes(xintercept = date),
    data =
      . %>% group_by(facet_label) %>% summarise(date = min(date)),
    color = 'grey', linewidth = 0.2
  ) +
  geom_vline(
    aes(xintercept = date),
    data =
      . %>% group_by(facet_label) %>% summarise(date = max(date)),
    color = 'grey', linewidth = 0.2
  ) +
  geom_point(size = 0.5, stroke = 0, alpha = 0.3) +
  geom_line(aes(y = predicted*1e-3)) +
  facet_wrap(~facet_label, ncol = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  scale_y_continuous(breaks = seq(0, 1e2, 5)) +
  scale_color_manual(
    values = cnst$colors$cv_series
  ) +
  figspec$MyGGplotTheme(axis = 'xy', panel_border = FALSE) +
  guides(color = 'none') +
  labs(y = 'Weekly deaths (×1000)', x = NULL)
fig$cv

# Calibrate, validate, apply --------------------------------------

forecasting_error <- list()

# test series
forecasting_error$error_series <-
  predictions$cv |>
  filter(cv_sample == 'test')

# error model specifications
forecasting_error$specs <- tribble(
  ~model_id, ~model_spec,
  'SNO', list(
    type = 'gamlss',
    family = gamlss.dist::SN1(sigma.link = 'log'),
    formula = as.formula(score~1),
    sigma.formula =
      as.formula(~ 1 + pbc(epi_week)),
    nu.formula = as.formula(~ 1 + pbc(epi_week)),
    tau.formula = as.formula(~ 1),
    score = 'logratio'
  ),
  'NB', list(
    type = 'gam',
    score = 'logratio'
  ),
  'EQ', list(
    type = 'empirical',
    score = 'logratio'
  )
)

# merge data with model definitions
forecasting_error$for_fit <-
  forecasting_error$error_series |>
  nest(data = c(-region_iso)) |>
  expand_grid(forecasting_error$specs)

# iterate in parallel model, region
forecasting_error$fitted <- foreach(
  x = iter(forecasting_error$for_fit, by = 'row'),
  .combine = bind_rows,
  .packages = c('dplyr', 'tidyr', 'gamlss', 'data.table')
) %dopar% {suppressPackageStartupMessages({
  
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Fit ", x$model_id,  " in ", x$region_iso, "\n",
      sep = ""
  )
  
  # input data
  input_data <- x[, "data"][[1]][[1]]
  
  # model parametrization
  model_para <- x$model_spec[[1]]
  score_type <- model_para[['score']]
  
  # add score
  input_data$score  <-
    NonconformityScore(score_type)$Score(input_data$observed,
                                         input_data$predicted)
  
  # the calibration data from which to learn the error distribution
  calibration <- input_data |>
    filter(cv_id %in% cnst$cv_series_calibration)
  # the predictors over which to construct the prediction intervals
  # needs to have the same length as the test series in each cv split
  X <- input_data |>
    filter(cv_id %in% cnst$cv_series_validation[1]) |>
    dplyr::select(weeks_since_test_start, month, epi_week, season)
  
  # fit models and capture errors
  result <- tryCatch({
    
    # model out-of-sample error by forecasting horizon
    if (identical(model_para$type, 'gamlss')) {
      
      fit <- gamlss(
        formula = eval(model_para$formula),
        sigma.formula = eval(model_para$sigma.formula),
        nu.formula = eval(model_para$nu.formula),
        tau.formula = eval(model_para$tau.formula),
        family = eval(model_para$family),
        data = calibration,
        control = gamlss.control(n.cyc = 400)
      )
      
      # reconstruct the call to gamlss because the package
      # messes up the storage of that call evaluation when
      # in an foreach environment
      fit$call <-
        call('gamlss', model_para$formula, model_para$sigma.formula,
             model_para$nu.formula, model_para$tau.formula,
             model_para$family, calibration)
      
      predicted_time_varying_params <-
        predictAll(
          object = fit, data = calibration,
          newdata = X,
          type = 'response', output = 'list'
        )
      
      predicted_time_varying_params[['y']] <- NULL
      
      # get the name of the quantile function corresponding
      # to the distribution of our fitted model
      quantile_name <- paste0('q', model_para$family$family[1])
      distribution_name <- paste0('p', model_para$family$family[1])
      
      # get the quantiles of the empirical error distribution
      # over forecasting horizon
      predicted_quantiles_of_error_distribution <- bind_cols(
        X,
        q1 = do.call(quantile_name, c(p = cnst$quantiles[1],
                                      predicted_time_varying_params)),
        q2 = do.call(quantile_name, c(p = cnst$quantiles[2],
                                      predicted_time_varying_params)),
        q3 = do.call(quantile_name, c(p = cnst$quantiles[3],
                                      predicted_time_varying_params)),
        q4 = do.call(quantile_name, c(p = cnst$quantiles[4],
                                      predicted_time_varying_params)),
        q5 = do.call(quantile_name, c(p = cnst$quantiles[5],
                                      predicted_time_varying_params)),
        q6 = do.call(quantile_name, c(p = cnst$quantiles[6],
                                      predicted_time_varying_params)),
        pscore_10p = do.call(
          distribution_name, c(q = log(1.1),
                               predicted_time_varying_params,
                               lower.tail = FALSE))
      )
      
      predicted_quantiles_of_forecast_distribution <-
        input_data |>
        dplyr::select(weeks_since_test_start, cv_id, observed, predicted, score) |>
        left_join(
          predicted_quantiles_of_error_distribution |>
            dplyr::select(weeks_since_test_start, q1, q2, q3, q4, q5, q6),
          by = 'weeks_since_test_start'
        ) |>
        # apply predicted quantiles of error distribution to the point
        # forecasts to derive the prediction intervals
        mutate(across(
          c(q1, q2, q3, q4, q5, q6),
          ~NonconformityScore(score_type)$InverseScore(., predicted),
          .names = 'PI{.col}'))
      
    }
    
    # model out-of-sample error by forecasting horizon
    if (identical(model_para$type, 'gam')) {
      
      predicted_time_varying_params <- NA
      
      # get the quantiles of the empirical error distribution
      # over forecasting horizon
      simu_cols <- grepl('^simulated.+', names(input_data))
      simulated_errors <- apply(
        input_data[,simu_cols], 2,
        function (x) NonconformityScore(score_type)$Score(
          observed = x,
          predicted = input_data[,'predicted'][[1]]
        )
      )
      
      # get the quantiles of the empirical error distribution
      # over forecasting horizon
      predicted_quantiles_of_error_distribution <- bind_cols(
        input_data,
        q1 = apply(simulated_errors, 1, quantile, p = cnst$quantiles[1]),
        q2 = apply(simulated_errors, 1, quantile, p = cnst$quantiles[2]),
        q3 = apply(simulated_errors, 1, quantile, p = cnst$quantiles[3]),
        q4 = apply(simulated_errors, 1, quantile, p = cnst$quantiles[4]),
        q5 = apply(simulated_errors, 1, quantile, p = cnst$quantiles[5]),
        q6 = apply(simulated_errors, 1, quantile, p = cnst$quantiles[6]),
        pscore_10p = apply(simulated_errors, 1, function (x) 1-ecdf(x)(log(1.1)))
      ) |>
        group_by(weeks_since_test_start) |>
        summarise(across(c(q1, q2, q3, q4, q5, q6, pscore_10p), mean))
      
      predicted_quantiles_of_forecast_distribution <-
        input_data |> 
        dplyr::select(weeks_since_test_start, cv_id, observed, predicted, score) |>
        left_join(
          predicted_quantiles_of_error_distribution |>
            dplyr::select(weeks_since_test_start, q1, q2, q3, q4, q5, q6),
          by = 'weeks_since_test_start'
        ) |>
        # apply predicted quantiles of error distribution to the point
        # forecasts to derive the prediction intervals
        mutate(across(
          c(q1, q2, q3, q4, q5, q6),
          ~NonconformityScore(score_type)$InverseScore(., predicted),
          .names = 'PI{.col}'))
      
    }
    
    # empirical quantiles
    if (identical(model_para$type, 'empirical')) {
      
      predicted_time_varying_params <- NA
      
      # get the quantiles of the empirical error distribution
      # over forecasting horizon
      predicted_quantiles_of_error_distribution <-
        calibration |>
        transmute(
          cv_id, weeks_since_test_start, month,
          observed, predicted,
          score,
          months_since_test_start = floor(weeks_since_test_start / 4)
        ) |>
        group_by(month) |>
        summarise(
          q1 = quantile(score, cnst$quantiles[1], na.rm = TRUE),
          q2 = quantile(score, cnst$quantiles[2], na.rm = TRUE),
          q3 = quantile(score, cnst$quantiles[3], na.rm = TRUE),
          q4 = quantile(score, cnst$quantiles[4], na.rm = TRUE),
          q5 = quantile(score, cnst$quantiles[5], na.rm = TRUE),
          q6 = quantile(score, cnst$quantiles[6], na.rm = TRUE)
        )
      
      predicted_quantiles_of_forecast_distribution <-
        input_data |>
        dplyr::select(weeks_since_test_start, month, cv_id, observed, predicted, score) |>
        #mutate(months_since_test_start = floor(weeks_since_test_start/4)) |>
        left_join(
          predicted_quantiles_of_error_distribution,
          by = 'month'
        ) |>
        # apply predicted quantiles of error distribution to the point
        # forecasts to derive the prediction intervals
        mutate(across(
          c(q1, q2, q3, q4, q5, q6),
          ~NonconformityScore(score_type)$InverseScore(., predicted),
          .names = 'PI{.col}'))# |>
        #select(-months_since_test_start)
      
    }
    
    # return result if fitting succeeded
    result_if_no_error <- bind_cols(
      x,
      tibble(
        prediction = list(predicted_quantiles_of_forecast_distribution),
        predicted_quantiles = list(predicted_quantiles_of_error_distribution),
        predicted_parameters = list(predicted_time_varying_params)
      ),
      error_while_fit = FALSE,
      error_message = NA
    )
    
    return(result_if_no_error)
    
  },
  
  # return result if fitting did not succeed
  error = function(e) {
    cat(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " Error on ", x$model_id, " in ", x$region_iso, ": ",
      geterrmessage(), "\n"
    )
    # return same object as fitted model, but with NA predictions
    input_data$q1 <- NA; input_data$q2 <- NA
    input_data$q3 <- NA; input_data$q4 <- NA
    input_data$q5 <- NA; input_data$q6 <- NA
    input_data$PIq1 <- NA; input_data$PIq2 <- NA
    input_data$PIq3 <- NA; input_data$PIq4 <- NA
    input_data$PIq5 <- NA; input_data$PIq6 <- NA
    result_if_error <- bind_cols(
      x,
      tibble(
        prediction = list(input_data),
        predicted_quantiles = NA,
        predicted_parameters = NA
      ),
      error_while_fit = TRUE,
      error_message = geterrmessage()
    )
    return(result_if_error)
  }
  ) # end of tryCatch()
  
  return(result)
  
})} # end of dopar(suppressPackageStartupMessages)

stopCluster(cnst$cl)

# Plot estimated errors over calibration series -------------------

country_names <-
  forecasting_error$fitted |>
  unnest(data) |>
  select(region_iso, region_name) |>
  unique()
prediction_intervals <-
  left_join(forecasting_error$fitted, country_names)  

fig$calibration <- list()

fig$calibration$model_id_labs <- c(
  NB = '(a) Negative Bin. PIs',
  SNO = '(b) Empirical SN PIs',
  EQ = '(c) Empirical RQ PIs'
)

fig$calibration$data <-
  prediction_intervals |>
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) |>
  unnest(prediction) |>
  filter(cv_id %in% cnst$cv_series_calibration, region_iso == cnst$example_country) |> 
  filter(model_id %in% c('SNO', 'NB', 'EQ')) |>
  mutate(model_id = factor(model_id, names(fig$calibration$model_id_labs)))

fig$calibration$plot <-
  fig$calibration$data |>
  ggplot() +
  aes(x = weeks_since_test_start, y = score) +
  geom_linerange(
    aes(x = weeks_since_test_start, ymin = q1, ymax = q6),
    color = 'grey70', size = 1.5
  ) +
  geom_hline(yintercept = 0, color = 'grey50') +
  geom_point(color = cnst$colors$cv_series['test'], size = 0.2) +
  facet_wrap(
    ~model_id, labeller = labeller(model_id = fig$calibration$model_id_labs)
  ) +
  scale_y_continuous() +
  scale_color_manual(
    values = cnst$colors$cv_series
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'log(observed/predicted)', x = 'Weeks into forecasting period')
fig$calibration$plot

fig$calibration_by_country$data <- 
  prediction_intervals |>
  dplyr::select(-predicted_quantiles,
                -predicted_parameters, -data) |>
  unnest(prediction) |>
  filter(cv_id %in% cnst$cv_series_calibration,
         region_iso %in% config$countries$analysis) |> 
  filter(model_id %in% c('SNO')) |>
  mutate(model_id = factor(model_id, names(fig$model_id_labs), fig$model_id_labs))

fig$calibration_by_country$plot <-
  fig$calibration_by_country$data |>
  ggplot() +
  aes(x = weeks_since_test_start, y = score) +
  geom_linerange(
    aes(x = weeks_since_test_start, ymin = q1, ymax = q6),
    color = 'grey70', size = 1.5
  ) +
  geom_hline(yintercept = 0, color = 'grey50') +
  geom_point(color = cnst$colors$cv_series['test'], size = 0.2) +
  facet_wrap(
    ~region_name, ncol = 4
  ) +
  scale_y_continuous() +
  scale_color_manual(
    values = cnst$colors$cv_series
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  guides(color = 'none') +
  labs(y = 'log(observed/predicted)', x = 'Weeks into forecasting period')
fig$calibration_by_country$plot

# Export ----------------------------------------------------------

saveRDS(prediction_intervals, paths$output$prediction_intervals)

ExportFigure(
  fig$cv, paths$output$fig, filename = '20-cv',
  device = 'pdf',
  width = config$figure$width,
  height = config$figure$width*0.8
)

ExportFigure(
  fig$calibration$plot, paths$output$fig, filename = '20-calibration',
  device = 'pdf',
  width = config$figure$width,
  height = config$figure$width*0.5
)

ExportFigure(
  fig$calibration_by_country$plot, paths$output$fig,
  filename = '20-calibrationbycountry',
  device = 'pdf',
  width = config$figure$width,
  height = config$figure$width*1.3
)
