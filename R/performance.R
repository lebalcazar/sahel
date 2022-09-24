# Script to evaluate the performance of the forecast with
# PERSIANN-CDR. Monthly data evaluation. 

library(tidyverse)
source("R/functions.R")

<<<<<<< HEAD
# funciones 
# error medio
mae <-function(obs, cal) mean(abs(cal - obs), na.rm = T)

# pbias
pBias <- function(obs, cal){
  num = sum((cal - obs), na.rm = T)
  den = sum(ifelse(is.na(obs), NA, obs), na.rm= T)
  bias = round((num/den)*100,1)
  return(bias) 
}

# raiz del error cuadrado medio
rmse <- function(obs, cal){
 dif <- sqrt(mean((obs - cal)^2, na.rm = T))
 return(dif)
}

# error medio
mae <-function(obs, cal) mean(abs(cal - obs), na.rm = T)

# NSE
nse <- function(obs, cal){
  Ns = sum((obs - cal)^2) /sum((obs - mean(obs))^2)
  NS <- 1 - Ns
  return(NS)
}


r2 <- function(obs, cal){
  cor = lm(cal ~ obs) 
   summ <- summary(cor)$r.squared
  return(summ)
}

r <- function(obs, cal){
  cor(obs, 
      cal, 
      method = 'pearson', 
      use = 'na.or.complete')
}

# % error
er <- function(obs, cal){
  e_abs <- abs(mean(obs) - mean(cal))/mean(obs) * 100
  return(e_abs)
}

# Ndatos 
nDatos <- function(obs, cal){
  n = ifelse(!is.na(obs) & !is.na(cal),1,0)
  t = sum(n)
  return(t)
}

# error_relativo
RE <- function(obs, cal){
  num = abs(obs - cal)
  er = (num/cal) * 100
}


# Leer los datos pronosticos 2021 y unir con la ubicación de las coordendas que 
# contienen los nombres de los punto-pixeles
=======
# Leer los datos pronosticos 2021
>>>>>>> 90a8773 (Mover functions a fichero R/Functions.R)

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



