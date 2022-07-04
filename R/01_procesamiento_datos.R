# procesar datos de pronóstico 
library(tidyverse)
library(lubridate)

# leer datos de pronostico desde proyeto lbo_doc:
# "/home/luisbalcazar/Documentos/lbo_doc/resultados/datos_preProceso/datos_f.rds"

# datos 1983-2020
da <- read_rds("../lbo_doc/resultados/datos_preProceso/datos_f.rds")

# ecuaciones
# lluvia de 2021-2022 coeficientes para caada pixel -> tener lluvia mensual may-oct
# prc2022 <- c(0, 0, 10, 50, 69,128, 360, 250, 38, 50, 0, 0)
prc2022 <- c(50, 69,128, 360, 250, 38)

# obetener la normal climática con los datos PERSIANN-CDR 1983-2020
normal_c <- 
  pr |> 
  group_by(name, month = month(date), id_xy) |> 
  summarise(pmean = mean(cdr, na.rm = TRUE), 
            # calcular n percentiles
            q1 = quantile(cdr, 0.10), 
            q3 = quantile(cdr, 0.90), 
            psd = sd(cdr, na.rm = TRUE),   
            ci = qnorm(1-0.025) * (psd/n()),  # intervalo de confianza
            se = psd/n() # error estandar de la media
            )

normal_bakel <- 
  normal_c |> 
  filter(id_xy == "-12.625_14.875", 
         month >= 5, month <= 10) |> 
  ungroup() |> 
  mutate(pred = prc2022, 
         clase = case_when(pred > q3 ~ 3, 
                           pred < q1 ~ 1, 
                           TRUE ~ 2) |> 
           factor(levels = 1:3, labels = c("bajo", "cerca", "sobre"), ordered = TRUE), 
         mapa_extract = datoDig
         ) 

# valor de AUC para un punto
normal_bakel |> 
  summarise(auc = auc(mapa_extract, clase), 
            id_xy = unique(id_xy))

# plot ROC 
pROC::roc(normal_bakel$mapa_extract, normal_bakel$clase) |> 
  plot(print.auc = TRUE)

pROC::roc(normal_bakel$mapa_extract, normal_bakel$clase) |> 
ggroc()

# plot 
gplot <- normal_bakel |>
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
                y = pred, colour = clase, 
                group = 1))

gplot


# pronostico
pr <- read_rds("../lbo_doc/resultados/pronostico_prc_rds_por_pixel/pronostico_poly_mensual.rds")






# ROC
library(pROC)

# análisis proc
# digitalizar y obtener datos de mapas
# pasar as corrdendas y extraer los datos

datoDig <- factor(x = c(2,3,1,3,1,3), labels = c("bajo", "cerca", "sobre"), ordered = TRUE)


auc(datoDig, normal_bakel$clase)








