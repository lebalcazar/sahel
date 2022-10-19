library(tidyverse)
library(extrafont)
loadfonts(quiet = TRUE)

# best model performance 
performance_best <- read_csv("outputs/models/model_performance.csv") %>% 
  separate(col = id, 
           into = c("group_number", "region_sst", "region_mslp", 
                    "region_rhum", "name", "x", "y"), 
           sep = "_", 
           convert = TRUE) %>%
  group_by(x, y) %>% 
  # filter the polynomial model and region 3
  dplyr::filter(model == "poly", region_sst == "s3") |>  
  filter(p.value < 0.05, R2_test >= 0.2) %>%
  top_n(-1, AIC) %>%
  mutate(which = paste(region_sst, model, sep = ": ") %>% 
           as.factor(),
         pixel_model = as.numeric(which)) %>% 
  dplyr::select(model, region_sst, region_mslp, region_rhum, 
                name, x, y, which, group_number)

# table of coefficients
coef_mpoly <- read_csv("outputs/models/model_coefs.csv") %>%
  separate(col = id, into = c("group_number", "region_sst", "region_mslp", 
                              "region_rhum", "name", "x", "y"), 
           sep = "_", convert = TRUE) %>%
  dplyr::select(model, term, estimate, name, x, y, 
                region_sst, group_number)

# filter coefficients with the best performance
data_c <- left_join(performance_best, 
                    coef_mpoly, 
                    by = c("model", "x", "y", "name", 
                           "region_sst", "group_number")) %>% 
  mutate(term = case_when(term == "(Intercept)" ~ "b0", 
                          term == "poly(sst, 2, raw = TRUE)1" ~ "b1",
                          term == "poly(sst, 2, raw = TRUE)2" ~ "b2", 
                          TRUE ~ paste0(term, "_c"))) %>% 
  pivot_wider(id_cols = c(x, y, which), 
              names_from = c(term), 
              values_from = estimate)

# SST data
tbl_forecast <- readRDS("data/rds/data_timeseries.rds") %>%
  # datos <-  datos0 %>% 
  dplyr::select(date, x, y, name, cdr, 
                mslp, sst, reg_sst, reg_rhum, reg_mslp) %>% 
  filter(reg_rhum == "r1", reg_mslp == "m1", reg_sst == "s3") %>% 
  group_by(x, y, reg_sst) %>% 
  mutate(sst = lag(sst, 11)) %>%   
  left_join(., data_c, by = c("x", "y")) %>% 
  mutate(pred_poly = b0 + b2*sst^2 + b1*sst) %>%  
  mutate(pred_poly = ifelse(pred_poly < 0, 0, pred_poly)) %>% 
  #filter(date >= "2005-01-01") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  group_by(year, month) %>% 
  select(pred_poly) %>% 
  summarise(mean_pred = mean(pred_poly, na.rm = TRUE
  ) %>% 
    round(1)
  ) %>% 
  mutate(mean_pred = ifelse(mean_pred < 10, 0, mean_pred)) %>%
  pivot_wider(names_from = month, values_from = mean_pred) %>%
  bind_cols(., 
            Total = rowSums(.[ ,-1]) %>% round()) 
  names(tbl_forecast) <- c("Year", 
                           as.character(month(1:12, label = TRUE, 
                                              abbr = TRUE
                                              )), 
                           "Total")
  
write_csv(tbl_forecast, file = "outputs/tables/table6.csv")
