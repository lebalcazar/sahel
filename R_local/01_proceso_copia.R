library(tidyverse)
library(lubridate)
library(broom)
library(raster)
library(caret)
library(sf)

rm(list = ls())
source('R/model_funtions.R')

# leer la base de datos
da <- readRDS('data/rds/datos_f.rds')  

# ejemplo con datos SST para modelos lm
  datos <- da %>% filter(reg_mslp == "m1", reg_rhum == "r3") %>%  
  dplyr::select(-c(ta_era5, cod, prc, tmn, tmx, tmd)) %>%   
  group_by(name, x, y, reg_sst, reg_rhum, reg_mslp) %>% 
  mutate(mslp = lag(mslp, 12),
         rhum = lag(rhum, 12), 
         nino12 = lag(nino12, 11), 
         nino34 = lag(nino34, 11), 
         oni = lag(oni, 11), 
         tni = lag(tni, 11))

# x <- 
#   datos %>% dplyr::select(date, name, cdr, tni) %>% 
#   dplyr::mutate(tni = ifelse(tni == -99.990, NA, tni)) %>%
#   filter(name == "Labe", reg_sst == "s3") %>% 
#     mutate(indice = lag(tni, 14)) 
# x %>% ggplot() +
#   geom_point(aes(indice, cdr))
# lm <- lm(indice ~ cdr, x)
# 
# broom::glance(lm)  


################
# filtra los datos SST de las reg1,2,3
# da %>% 
#   dplyr::filter(reg_mslp == "m1", reg_rhum == "r1") %>% 
#   group_by(name, reg_sst) %>% 
#   dplyr::select(date, x, y, cdr, name, cod, reg_sst, sst) %>% 
#   # dplyr::select(cod, x, y, name, date, cdr, reg_sst, sst, nino12, tni)   #%>%  # se deja sst nino12 y tni (segun VIF)
#   group_by(reg_sst, name, x, y) %>%   #%>% ungroup()
#   summarise(sst = lag(sst, 6), cdr) %>% 
#    
#   map(~.x %>% 
#       group_split(reg_sst, name) %>%
#       lm(sst ~ cdr, data = .x))
#                 # glance(lm)
#               
#       # plot(sst, cdr)

################


  
# datos <- na.omit(datos)

#Obtener ID único a partir de los grupos de datos
# ids <- dplyr::group_keys(datos) %>%
#   dplyr::mutate(id = paste0(row_number(),"_",
#                      regionSST, '_', regionSLP, '_', regionRHUM, 
#                      '_', name, '_', 
#                      x, '_', 
#                      y))

ids <- dplyr::group_keys(datos) %>%
  dplyr::mutate(id = paste0(row_number(), "_",
                            reg_sst, '_', reg_mslp, '_', reg_rhum, '_',
                            name, '_', 
                            x, '_', 
                            y))

datos <- dplyr::group_split(datos)
# prueba ------------------------------------------------------------------
# tmp <- datos[[1]]
# # Prueba con un solo de datos
# 
# map_df(set_names(1:5),
#        ~train_models(seed = .x,
#                      id = ids$id[[1]],
#              data = tmp,
#              out_files = c(coefs = "../lbo_doc/modelos/model_lm_nls_step/prueba_model_coefs.csv",                     # Lista de ficheros de salida de los resultados       #    deben incluir las rutas donde guardar
#                            perfs = "../lbo_doc/modelos/model_lm_nls_step/prueba_model_performance.csv"),
#              append = FALSE,
#              control = nls.control(maxiter = 120))) %>% view

# plot(cdr ~ step, data = drop)


# Ejecutar la función para muchos grupos de datos.
drop <- pwalk(expand_grid(seed = 81:100, #:228, 
                          tibble(id = ids$id,
                                 data = datos)
                          ) %>% 
                mutate(lag = case_when(str_detect(id, '_s1_') ~ 5,
                                       str_detect(id, '_s2_') ~ 10,
                                       str_detect(id, '_s3_') ~ 11,
                                       TRUE ~ 11)
                       ),  
              train_models,
                                             # Lead a aplicar
              formulas = list(lm = cdr ~ sst,                             #  Lista de modelos lineales a entrenar
                              poly = cdr ~ poly(sst, 2, raw = TRUE),
                              step = cdr ~  sst + mslp + rhum + nino12 + tni + nino34 + nino4 + oni,
                              nls = cdr ~ a * exp(sst * b)),
              
              train_partition = 0.7,  #  Proporción "train" para partición de datos
              out_files = c(coefs = "../lbo_doc/modelos/model_lm_nls_step/model_coefs_05.csv",         # Lista de ficheros de salida de los resultados   # deben incluir las rutas donde guardar
                            perfs = "../lbo_doc/modelos/model_lm_nls_step/model_performance_05.csv", 
                            preds = "../lbo_doc/modelos/model_lm_nls_step/predicciones_con30p.csv"),
              append = TRUE,  # TRUE para agregar los datos en el mismo fichero existente
              control = nls.control(maxiter = 1000)
)             
# aqui finaliza el proceso de los datos que 
# obtiene los coeficientes y rendimiento o performance




# leer datos de coeficientes y rendimiento (performance)
coeficientes = read_csv("../lbo_doc/modelos/model_lm_nls_step/model_coefs.csv")
rendimiento = read_csv("../lbo_doc/modelos/model_lm_nls_step/model_performance.csv")

# lm modelos lineales ------------------------------------------------------

## plot lm poly ------------------------------------------------------------


# rendimiento de ml y poly ------------------------------------------------
rendimiento_best <- rendimiento %>%
  separate(col = id, 
           into = c("group_number", "region_sst", "region_mslp", "region_rhum", "name", "x", "y"), 
           sep = "_", 
           convert = TRUE) %>%
  group_by(x, y) %>%    # model, region
  # filter(p.value < 0.05, adj.r.squared >= 0.5) %>%   # adj.r.squared >= 0.5
  filter(p.value < 0.05, R2_test >= 0) %>%   # nse para tomar encuenta nls (non-linear models)
  top_n(-1, AIC) %>%
  # top_n(1, NSE) %>% 
  mutate(which = paste(region_sst, model, sep = ": ") %>% 
           as.factor(),
         pixel_model = as.numeric(which)
  ) 






# probar con esto
train <- train_data(x = datos, 
                    col = rlang::f_lhs(formulas[[1]]), 
                    p = train_partition)

train = map(datos, train_data) 
test = map2(datos, train, anti_join, by = c("x","y","date"))




rendimiento = read_csv("../lbo_doc/modelos/model_lm_nls_step/model_performance.csv")

rend <- 
  rendimiento_best %>% 
  separate(col = id, 
           into = c("num", "regSST", "regMSLP", "regRH", "name", "x", "y"), sep = "_") %>% 
  filter(model == "poly") %>% 
  # select(df.residual) %>% 
  ggplot(aes(x = x, y = NSE, colour = y)) +
  geom_point()

rend
