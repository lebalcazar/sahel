# Script to evaluate the performance of the forecast with
# PERSIANN-CDR. Monthly data evaluation. 

library(tidyverse)
source("R/functions.R")

# Leer los datos pronosticos 2021

pronostico_poly_2021 <- read_rds("data/rds/pronostico_poly_2021.rds")
coorEstCnc <- read_rds("data/rds/coorEstCnc.rds")
cdr_2021 <- read_rds("data/rds/cdr_2021.rds")

# Unir con la ubicación de las coordendas que 
# contienen los nombres de los punto-pixeles

data <- left_join(pronostico_poly_2021, coorEstCnc, by = c("x", "y")) %>%
  left_join(cdr_2021, by = c("date", "x", "y", "name")) %>% 
  dplyr::select(date, name, x, y, pred = pred_poly, cdr) %>% 
  dplyr::filter(name != "other", 
                date >= "2021-01-01", 
                lubridate::month(date) >= 5 & lubridate::month(date) <= 10
                ) %>% 
  # filter(!name %in% c("Tidjikja", "Kedougou", "Kiffa", "Matam")) %>%
  arrange(name)



performance <-
data %>% 
  group_by(name) %>% 
  summarise(mae = mae(cdr, pred) %>% round(1), 
            pbias = pBias(cdr, pred) %>% round(1), 
            r = r(cdr, pred) %>% round(3),
            r2 = r2(cdr, pred) %>% round(3), 
            nse = nse(cdr, pred) %>% round(3),
            n = nDatos(cdr, pred))

performance %>% select(r2) %>% colMeans()

write_csv(x = performance, file = "tablas_resultados/performance.csv", 
          append = FALSE, col_names = TRUE, )



