
library(tidyverse)

# data of model performance
mdl_perf  <-  read_csv("outputs/models/model_performance.csv")
met_names <- read_rds("data/rds/met_names.rds")

# forecast point-pixel 
tbl_mdl <-
  mdl_perf %>% 
  separate(col = id, 
           into = c("group_number", "region_sst", "region_mslp", 
                    "region_rhum", "name", "x", "y"), 
           sep = "_", 
           convert = TRUE) %>% 
  dplyr::filter(name != 'other') %>% ungroup() %>% 
  dplyr::select(name, region_sst, model, 
                NSE, p.value, R2_test, AIC
                ) %>% 
  group_by(model, name, region_sst) %>% 
  # filter(R2_test >= 0.5) %>%   
  filter(NSE >= 0.5) %>%   
  top_n(-1, AIC) %>% 
  dplyr::select(name, region_sst, model, NSE) %>% ungroup() %>% 
  pivot_wider(names_from = model, values_from = NSE) %>% 
  group_by(name) %>% 
  summarise(max_lm = max(lm, na.rm = TRUE) %>% round(3), 
            max_poly = max(poly, na.rm = TRUE) %>% round(3), 
            max_nls = max(nls, na.rm = TRUE) %>% round(3), 
            max_step = max(step, na.rm = TRUE) %>% round(3)
  ) %>% 
  bind_rows(tibble(name = "Tidjikja", max_lm = NA, max_poly = NA, 
                   max_nls = NA, max_step = NA)) %>% 
  arrange(factor(name, met_names, met_names))

# save table
write_csv(tbl_mdl, file = "outputs/tables/table3.csv")

