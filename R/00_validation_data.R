
# validation of estimate data 

# requiered packages and functions
library(tidyverse)
source("R/functions.R")

met_names <- reader::read_rds("data/rds/met_names.rds")

# PERSIANN-CDR data
cdr_data <- read_rds("data/rds/data_timeseries.rds") %>% 
  dplyr::filter(name != "other", 
                reg_sst == "s1", 
                reg_mslp == "m1", 
                reg_rhum == "r1"
                ) %>% 
  select(name, date, cdr, prc
  ) %>% 
  group_by(name) %>% 
  rename(obs = prc, cal = cdr) %>% 
  mutate(name = factor(name, levels = met_names)
         ) %>% 
  summarise(
    R2 =     sprintf('%0.3f', round(r2(obs, cal),3)),
    PBIAS =  sprintf("%0.1f", pBias(obs, cal)),
    MAE =    sprintf("%0.1f", round(mae(obs, cal))),
    N_data = nDatos(obs, cal)
  )
#  print in csv
write_csv(x = cdr_data, file = "outputs/tables/table1.csv", col_names = TRUE)


# SST data
sst_data <- readRDS("data/rds/SST_and_buoys.rds") %>% 
  rename(cod = codigo, obs = sst_obs, cal = sstCal) %>% 
  group_by(cod) %>% 
  summarise(
    R2 =     sprintf('%0.3f', round(r2(obs, cal),3)),
    PBIAS =  sprintf("%0.2f", pBias(obs, cal),2),
    MAE =    sprintf("%0.2f", mae(obs, cal), 2),
    N_data = nDatos(obs, cal)
  )
#  print in csv
write_csv(x = sst_data, file = "outputs/tables/table2.csv", col_names = TRUE)




