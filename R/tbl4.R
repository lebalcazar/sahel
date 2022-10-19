library(tidyverse)
library(extrafont)
library(lubridate)
source("R/functions.R")
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

# data 2021
cdr21 <- read_rds("/home/luisbalcazar/Documentos/sahel/data/rds/cdr_2021.rds")
frc21 <- read_rds("/home/luisbalcazar/Documentos/sahel/data/rds/pronostico_poly_2021.rds") %>% 
  filter(year(date) >= 2021) %>% 
  left_join(cdr21) %>% 
  ungroup() %>% 
  select(date, name, cdr, pred_poly)

# SST data
tbl <- readRDS("data/rds/data_timeseries.rds") %>%
  # datos <-  datos0 %>% 
  dplyr::select(date, x, y, name, cdr, 
                mslp, sst, reg_sst, reg_rhum, reg_mslp) %>% 
  filter(reg_rhum == "r1", reg_mslp == "m1", reg_sst == "s3") %>% 
  group_by(x, y, reg_sst) %>% 
  mutate(sst = lag(sst, 11)) %>%   
  left_join(., data_c, by = c("x", "y")) %>% 
  mutate(pred_poly = b0 + b2*sst^2 + b1*sst) %>%  
  mutate(pred_poly = ifelse(pred_poly < 0, 0, pred_poly)) %>% 
  ungroup() %>% 
  select(date, name, cdr, pred_poly) %>% 
  bind_rows(frc21) %>% 
  filter(name != "other", 
         between(lubridate::month(date), 5, 10),
         lubridate::year(date) >= 2017) %>% 
  group_by(name, month = lubridate::year(date)) %>% 
  summarise(CDR = sum(cdr) %>% round(1), 
            FRC = sum(pred_poly) %>% round(1),
            RE = RE(obs = CDR, cal = FRC) %>% round(1))  
  
write_csv(tbl, "outputs/tables/table4.csv")
