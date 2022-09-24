# Script to forecast data processing 
library(tidyverse)
#library(lubridate)
library(fitdistrplus)
library(terra)
library(sf)
library(raster)

# leer datos de pronostico desde proyeto lbo_doc:
# "/home/luisbalcazar/Documentos/lbo_doc/resultados/datos_preProceso/datos_f.rds"

# datos sst junio-2020 a octubre 2021 
# da <- read_rds("../lbo_doc/resultados/datos_preProceso/datos_f.rds")
# sst_2020 <- da %>% 
#   dplyr::filter(date >= "2020-01-01") %>% 
#   dplyr::filter(reg_sst %in% c("s1", "s2", "s3"), reg_mslp == "m2", reg_rhum == "r3") %>% 
#   dplyr::select(date, sst, reg_sst) %>% 
#   dplyr::distinct(date, reg_sst, .keep_all = TRUE)
  


# datos 1983-2020
# datos_pron <- read_rds("../lbo_doc/resultados/pronostico_prc_rds_por_pixel/pronostico_poly_mensual.rds")

# datos PERSIANN 2021 
lf <- list.files("/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/04SIG/01Rawdata/Prc/cdr_global/mes/PERSIANN-CDR_mensual_global_Tif/CDR_2022-07-04033154pm", 
                 pattern = ".tif$", full.names = TRUE, all.files = TRUE)
rst_21 <- raster::stack(lf)

cnc <- readRDS("../lbo_doc/datos/sig/raster/mask_cnc_est.rds")

rst_crop <- raster::crop(rst_21, cnc)

rst_msk <- raster::mask(rst_crop, cnc)


writeRaster(x = rst_msk, 
            filename = names(rst_msk), bylayer = T, format = "tif")

writeRaster(x = rst_msk, 
            filename = paste0("/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/04SIG/01Rawdata/Prc/cdr_AfrOcc/mes/CDR_AfrOcc_cnc_est/cdr_2021/", 
                              names(rst_msk),
                              ".tif"), 
            bylayer = T)

# estaciones 
coorEstCnc <- readRDS('sahel/data/rds/coorEstCnc.rds') %>% as_tibble()
            
cdr2021_tbl <- rasterToPoints(rst_msk, xyFromCell = TRUE) %>% as_tibble()

# datos y coordenadas
dat <- left_join(coorEstCnc, cdr2021_tbl, by = c("x", "y"))

# tabla de datos cdr diarios 
tbl <- dat %>% 
  pivot_longer(cols = -c(x, y, name), names_to = "date", values_to = "cdr") %>% 
  mutate(date = as.Date(paste(date, 01), "CDR_%Y%m%d"), 
         id = paste(x, y, name, sep = "_")) %>% 
  # group_by(id, year = year(date), month = month(date)) %>% 
  # mutate(cdr = ifelse(is.na(cdr), 
  #                     rnorm(n = ., mean(cdr, na.rm = TRUE)), 
  #                     cdr)) %>% 
  mutate(cdr = ifelse(cdr < 1, 0, cdr)) %>% 
  mutate(cdr = ifelse(is.na(cdr), 0, cdr)) %>% 
  ungroup() %>% 
  dplyr::select(x, y, name, date, cdr)

# guardar
saveRDS(tbl, "/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/sahel/data/rds/cdr2021_por_pixel.rds")


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








