library(tidyverse)
library(broom)
library(sf)

source('R/functions.R')
# read data base 
data <- readRDS('data/rds/data_timeseries.rds')  %>% 
  # removed according to VIF
  dplyr::select(x, y, name, date, cdr, reg_sst, sst, reg_rhum, rhum, nino12, tni) %>%  
  group_by(reg_sst, reg_rhum, name, x, y)

# create x, y, regions, name identifiers'
ids <- dplyr::group_keys(data) %>%
  dplyr::mutate(id = paste0(row_number(), "_",
                            reg_sst, '_', 
                            reg_rhum, '_', 
                            name, '_',
                            x, '_', 
                            y))

data <- dplyr::group_split(data)

# Run the function for many data sets.
drop <- pwalk(expand_grid(seed = 2:5,
                          tibble(id = ids$id,
                                 data = data)
                          ) %>% 
                # lags come from a ACF
                mutate(lag = case_when(str_detect(id, '_s1_') ~ 5,
                                       str_detect(id, '_s2_') ~ 10,
                                       str_detect(id, '_s3_') ~ 11,
                                       TRUE ~ 11)
                       ),  
              train_models,
              # models
              formulas = list(lm = cdr ~ sst,                             
                              poly = cdr ~ poly(sst, 2, raw = TRUE),
                              step = cdr ~  sst + rhum + nino12 + tni,
                              nls = cdr ~ a * exp(sst * b)),
              
              train_partition = 0.7,
              # outputs
              out_files = c(coefs = "outputs/models/model_coefs.csv",         
                            perfs = "outputs/models/model_performance.csv", 
                            preds = "outputs/models/model_forecast.csv"),
              append = TRUE,  
              control = nls.control(maxiter = 1000)
)             


