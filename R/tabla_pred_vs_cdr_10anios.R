# tabla de pronóstico 
# 10 años.
# leer coeficientes 
# leer datos históricos hatsa el 2020

library(tidyverse)

# # sst 2020 - 2021
# sst_2020_2021 <- readRDS("data/rds/sst_2020_2021.rds") %>% 
#   filter(reg_sst == "s3")

# sst histórico
data <- readRDS("data/rds/datos_f.rds")
  
# sst <- data %>%
#   dplyr::select(date, sst, reg_sst) %>%
#   dplyr::filter(reg_sst == "s3")

sst <- unique(sst)

# cdr
cdr <- data %>%
  select(date, name, x, y, cdr, ) %>%
  mutate(xy = paste(x, y, sep = "_"))

# cdr <- unique(cdr)

cdr_hist <- 
  cdr %>% 
  mutate(month = lubridate::month(date)) %>% 
  filter(name != "other", dplyr::between(month, 5, 10))

coef_prc_poly <- read.csv("data/csv/coeficientes_de_precipitación.csv", 
                          header = T) %>% 
  mutate(which = str_remove(which, ": poly")) 


# predicción
# datos1 <- left_join(sst, coef_prc_poly, by = c("reg_sst" = "which")) %>%
pronostico <- left_join(sst, coef_prc_poly, by = c("reg_sst" = "which")) %>%
  group_by(x, y) %>% 
  mutate(pred_poly = b0 + b2*lag(sst, 11)^2 + b1*lag(sst, 11)) %>%   # con lag
  mutate(xy = paste(x, y, sep = "_")) %>% 
  mutate(pred_poly = ifelse(pred_poly < 0, 0, pred_poly)) %>% 
  arrange(xy) %>% 
  dplyr::select(date, x, y, pred = pred_poly, xy)
pronostico

# adjuntar pred 2021
pred21 <- readRDS("data/rds/pronostico_poly_2021.rds") %>% 
  dplyr::select(date, x, y, pred = pred_poly) %>% 
  mutate(xy = paste(x, y, sep = "_")) %>% 
  dplyr::filter(date >= "2021-01-01")
pred21

pronostico_todo <- bind_rows(pronostico, pred21)

write_rds(x = pronostico_todo, file = "data/rds/pronostico_todo.rds")


# adjuntar cdr 2021
cdr21 <- read_rds("data/rds/cdr_2021.rds") %>% 
  mutate(xy = paste(x, y, sep = "_")) %>% 
  dplyr::select(date, name, x, y, cdr, xy)

# unir con cdr
cdr_historico_83_21 <- bind_rows(cdr, cdr21) %>% 
  arrange(name)


# pronostico y cdr historico 
cdr_pronostico_historico_83_21 <- left_join(cdr_historico_83_21, pronostico_todo, 
                                            by = c("date", "x", "y", "xy")) %>% 
  arrange(name)

cdr_pronostico_historico_83_21

# guardar
write_rds(x = cdr_pronostico_historico_83_21, file = "data/rds/pronost_cdr_83_21.rds")


data_t <- readr::read_rds("data/rds/pronost_cdr_83_21.rds")

# hacer la tabal de pronostico
data <- 
  data_t %>%  
# cdr_pronostico_historico_83_21 %>% 
  filter(
         # lubridate::month(date) >= 5 & lubridate::month(date) <= 10,
         lubridate::year(date) >= 2000
         # name != "other",
         # name %in% c("Labe", "Segou", "Nioro-Du-Sahel", "Nema")
         ) %>%
  group_by(name, year = lubridate::year(date)
           ) %>%
  summarise(cdr = sum(cdr, na.rm = T),
            pred = sum(pred, na.rm = T)) %>%
  # group_by(name) %>%
  # summarise(cdr = mean(cdr, na.rm = TRUE) %>% round(),
  #           pred = mean(pred, na.rm = TRUE) %>% round()) %>%
  ungroup() %>% 
  arrange(name) 
  # pivot_longer(cols = c(cdr_mean, pred_mean), names_to = "type", values_to = "prc") %>% 
  # pivot_wider(id_cols = year, values_from = prc, names_from = name)

data %>% 
  mutate(error_abs = error_abs(cdr_mean, pred_mean))

data  
 
data %>% 
  ggplot(aes(x = cdr, pred)) +
  geom_point()

write_csv(x = data, file = "data/csv/performance_5anios_4estaciones.csv")

# data_ <- read_csv(file = "tablas_resultados/performance_5anios_4estaciones.csv", col_names = T)

performance <-
  data %>% 
  group_by(name) %>% 
  summarise(r = r(cdr, pred) %>% round(3),
            r2 = r2(cdr, pred) %>% round(3), 
            nse = nse(cdr, pred) %>% round(3),
            mae = mae(cdr, pred) %>% round(1), 
            pbias = pBias(cdr, pred) %>% round(1), 
            n = nDatos(cdr, pred))
performance

performance %>% select(r2) %>% colMeans()

write_csv(x = performance10a, file = "tablas_resultados/performance_10años_por_estacion.csv", 
          append = FALSE, col_names = TRUE, )


tabla <- 
  data %>% 
  mutate(ER = RE(obs = cdr, cal = pred) %>% sprintf("%0.1f", .))

write_csv(tabla, file = "tablas_resultados/tabla_4_esta_errorRelativo")



data <- read_rds("data/rds/pronost_cdr_83_21.rds") %>% 
  filter(date >= "2017-01-01", 
         lubridate::month(date) >= 5 & lubridate::month(date) <= 10) %>% 
  filter(name == "Nioro-Du-Sahel") %>% 
  pivot_longer(cols = c(pred, cdr), 
               names_to = "tipo", values_to = "prc")
  
data %>% 
  ggplot(aes(x = date, y = prc, colour = tipo)) + 
  geom_line()
