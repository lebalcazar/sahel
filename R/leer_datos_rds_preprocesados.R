# se leen los datos preprocesados 
# datos estan en la carpeta data/rds del proyecto Sahel

library(tidyverse)
library(lubridate)
library(fitdistrplus)
library(terra)
library(pROC)

# leer los datos históricos de persiann-cdr desde el archivo pronostico_poly
cdr_hist <- read_rds("data/rds/pronostico_poly_mensual.rds")  %>% 
 dplyr::select(-pred_poly)

# persiann 2021
cdr_2021 <- read_rds("data/rds/cdr_2021.rds")


# pronostico 2021
pronost_2021 <- read_rds("data/rds/pronostico_poly_2021.rds") %>% 
  dplyr::select(date, x, y, pred_poly)


cdr_y_pred <- 
  left_join(cdr_2021, pronost_2021, by = c("x", "y", "date")) %>% 
  dplyr::select(date, x, y, name, cdr, pred_poly) %>% 
  filter(between(lubridate::month(date), 5, 10))
  
cdr_y_pred %>% 
  ggplot(aes(x = cdr, y = pred_poly)) +
  geom_point() #+
  # facet_grid(name~.)
  
cdr_y_pred %>% 
  lm(pred_poly ~ cdr, data = .) %>% 
  summary()


# obetener la normal climática con los datos PERSIANN-CDR 1983-2020
normal_c <- 
  cdr_hist  %>%  
  group_by(name, month = month(date), id_xy) %>%  
  summarise(pmean = mean(cdr, na.rm = TRUE), 
            # calcular n percentiles
            q1 = quantile(cdr, 0.10),         # cuantil1
            q3 = quantile(cdr, 0.90),         # cuantil3
            psd = sd(cdr, na.rm = TRUE),      # desviación estándar
            ci = qnorm(1-0.025) * (psd/n()),  # intervalo de confianza
            se = psd/n()                      # error estandar de la media
  )

# CDR 1 estación 
cdr_2021_1est <- 
  cdr_2021 %>% 
  dplyr::filter(name == "Bakel",
                month(date) >= 5, month(date) <= 10) %>% 
  mutate(id_xy = paste(x, y, sep = "_"), 
         month = lubridate::month(date)) %>% 
  dplyr::select(name, id_xy, month, cdr)

# pronostico 1 estación
pronost_2021_1est <- 
  pronost_2021 %>% 
  dplyr::filter(name == "Bakel", 
                month(date) >= 5, month(date) <= 10) %>% 
  mutate(id_xy = paste(x, y, sep = "_"), 
         month = lubridate::month(date)) %>% 
  dplyr::select(name, id_xy, month, pred_poly)
  



# 
datoDig <- factor(x = c(6,3,1,3,1,3), labels = c("bajo", "cerca", "sobre"), ordered = TRUE)

normal_bakel <- 
  normal_c |> 
  filter(name == "Bakel", 
         month >= 5, month <= 10) |> 
  ungroup() |> 
  left_join(cdr_2021_1est, by = c("name", "id_xy", "month")) %>% 
  left_join(pronost_2021 )
  mutate(pred = cdr_2021_1est, 
         clase = case_when(pred$cdr > q3 ~ 3, 
                           pred$cdr < q1 ~ 1, 
                           TRUE ~ 2) %>%  
           factor(levels = 1:3, labels = c("bajo", "cerca", "sobre"), ordered = TRUE), 
         mapa_extract = datoDig
  ) 


# valor de AUC para un punto
normal_bakel %>% 
  summarise(auc = auc(mapa_extract, clase), 
            id_xy = unique(id_xy))

# plot ROC 
pROC::roc(normal_bakel$mapa_extract, normal_bakel$clase) |> 
  plot(print.auc = TRUE)

pROC::roc(normal_bakel$mapa_extract, normal_bakel$clase) |> 
  ggroc()

# plot 
gplot <- 
  normal_bakel %>% 
  ggplot()+
  geom_ribbon(aes(x = month, 
                  ymin = q1, ymax = q3), fill = "red", alpha = 0.3) +
  geom_ribbon(aes(x = month, 
                  ymin = pmean-psd*1, ymax = pmean+psd*1), fill = "green", alpha = 0.3) + # desviacion estandar
  geom_ribbon(aes(x = month, 
                  ymin = pmean-ci, ymax = pmean+ci), fill = "blue", alpha = 0.3) + # IC
  # geom_ribbon(aes(x = month, 
  #                 ymin = pmean- (3*se), ymax = pmean+(3*se)), fill = "yellow") + #error estandar
  geom_line(aes(x = month, 
                y = pmean)) +
  geom_point(aes(x = month, 
                 y = pred$cdr, colour = clase, 
                 group = 1))

gplot
