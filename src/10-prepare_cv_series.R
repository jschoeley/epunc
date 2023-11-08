# Prepare the CV data series

# Init ------------------------------------------------------------

library(yaml)
library(dplyr)
library(data.table)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  config = 'cfg/config.yaml',
  deathcounts = 'dat/mocy.csv'
)
paths$output <- list(
  deathcounts_cv = 'tmp/10-deathcounts_cv.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# global functions
source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  # date of Covid start needs to be a Monday
  start_covid = as.Date('2020-01-06')
  # in weeks
  training_length = 261
  test_length = 104
})

# Load time series of death counts --------------------------------

deathcounts <- fread(paths$input$deathcounts)

# Add totals over sex and age group -------------------------------

deathcounts[,age_group := paste0('[',age_start,',',age_start+age_width,')')]

strata_cols <-
  c('region_iso', 'sex', 'year', 'week', 'age_group', 'region_name') 
value_cols <-
  c('deaths', 'personweeks')

deathcounts_totals <-
  groupingsets(
    deathcounts,
    j = lapply(.SD, sum),
    by = strata_cols,
    sets = list(
      strata_cols,
      strata_cols[-which(strata_cols %in% 'sex')],
      strata_cols[-which(strata_cols %in% 'age_group')],
      strata_cols[-which(strata_cols %in% c('sex', 'age_group'))]
    ),
    .SDcols = value_cols
  )

deathcounts_totals[
  ,
  c('age_group', 'sex', 'date') :=
  .(ifelse(is.na(age_group), 'Total', age_group),
    ifelse(is.na(sex), 'Total', sex),
    as.Date(paste(year, week, 1, sep='-'), '%Y-%U-%u')
  )
]

deathcounts_totals <- as_tibble(deathcounts_totals)

# Split death counts into cross-validation series -----------------

CutCVData <- function(start_covid, training_length, test_length, cv_data){
  
  last_observed <-
    start_covid + as.difftime(test_length - 1, unit = 'weeks')
  
  number_of_series <-
    floor(length(seq(from = min(cv_data$date),
                     to = last_observed, by = 'weeks')) / test_length) -
    ceiling(training_length/test_length)
  
  end_dates_of_series <- integer(0)
  class(end_dates_of_series) <- 'Date'
  
  for (i in 1:(number_of_series-1)) {
    end_dates_of_series[i] <-
      last_observed - as.difftime(i * test_length, unit = 'weeks')
  }
  end_dates_of_series <- rev(end_dates_of_series)
  end_dates_of_series[number_of_series] <- last_observed
  
  data_cv_series <- list()
  
  for (i in 1:number_of_series) {
    data_cv_series[[i]] <-
      cv_data %>%
      filter(
        date %in% rev(seq(end_dates_of_series[i],
                          by = '-1 weeks',
                          length = training_length + test_length))
      ) %>%
      mutate(
        origin_date =
          end_dates_of_series[i] -
          as.difftime(training_length +
                        test_length - 1, unit = 'weeks'),
        origin_date_test =
          end_dates_of_series[i] -
          as.difftime(test_length - 1, units = 'weeks')
      ) %>%
      mutate(cv_id = i)
    
    data_cv_series[[i]] <-
      data_cv_series[[i]] %>%
      mutate(origin_weeks =
               as.numeric(difftime(date, origin_date, units = 'weeks')),
             weeks_since_test_start =
               as.numeric(difftime(date, origin_date_test, units = 'weeks'))
      )
    
    data_cv_series[[i]] <-
      data_cv_series[[i]] %>%
      mutate(
        cv_sample = if_else(weeks_since_test_start >= 0, 'test', 'training')
      )
  }
  
  cut_stats <- within(list(), {
    number_of_series = number_of_series
    end_dates_of_series = end_dates_of_series
  })
  
  print(cut_stats)
  return(bind_rows(data_cv_series))
}

deathcounts_cv <- CutCVData(
  start_covid = cnst$start_covid,
  training_length = cnst$training_length,
  test_length = cnst$test_length,
  cv_data = deathcounts_totals
)

# Add additional variables ----------------------------------------

deathcounts_cv <-
  deathcounts_cv |>
  mutate(
    month = lubridate::month(date),
    # shift week of year origin by 26 weeks
    epi_week = (week+26)%%53+1,
    season = case_when(
      month %in% c(12, 1, 2)  ~ 'Dez-Feb',
      month %in% c(3, 4, 5)   ~ 'Mar-May',
      month %in% c(6, 7, 8)   ~ 'Jun-Aug',
      month %in% c(9, 10, 11) ~ 'Sep-Nov'
    )
  )

# Standardize variable names --------------------------------------

deathcounts_cv <-
  deathcounts_cv |>
  rename(
    exposure = personweeks, observed = deaths,
    iso_year = year, iso_week = week
  )

# Export ----------------------------------------------------------

saveRDS(deathcounts_cv, paths$output$deathcounts_cv)
