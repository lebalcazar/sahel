
library(net)

nc <- nc_open("/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/04SIG/01Rawdata/sst/sst_global/daily_nc_raw/2021/adaptor.mars.internal-1656989991.4653413-10335-8-5a9d5e45-7493-4e88-9fdd-84ad4c7b9e59.nc")


# leer datos sst ERA5 2021

lf <- list.files("/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/04SIG/01Rawdata/sst_er5/sst_era5_global/sst_era5_tif/2021", 
                 pattern = ".tif$", all.files = TRUE, full.names = TRUE)

sst_21 <- rast(lf)

sst_poligonos <- st_read("/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/lbo_doc/resultados/PCA_SST_componentes/shp_cluster_poli/sst_recorte.shp")
sst_poligonos_vec <- vect(sst_poligonos)

sst_crop <- terra::crop(sst_21 ,sst_poligonos)

sst_mask <- terra::mask(sst_crop, sst_poligonos_vec)

# extraer la sst para cada zona SST (promedio)
ext <- terra::extract(sst_mask, sst_poligonos_vec, fun = "mean", na.rm = TRUE) %>% 
  mutate(reg_sst = paste0("s",ID)) %>% 
  dplyr::select(reg_sst, everything(), -ID) %>% 
  pivot_longer(cols = -reg_sst, names_to = 'date', values_to = 'sst') %>% 
  dplyr::mutate(date = as.Date(date, 'sst_%Y.%m.%d')) %>% 
  dplyr::select(date, sst, reg_sst)

# guaradar 
saveRDS(ext, '/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/sahel/data/rds/sst_2021.rds')

sst_2021 <- read_rds('/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/sahel/data/rds/sst_2021.rds')


#unir sst2020 y 2021

sst_2020_2021 <- bind_rows(sst_2020, sst_2021)

saveRDS(sst_2020_2021, '/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/sahel/data/rds/sst_2020_2021.rds')


coef_prc_poly <- read.csv("/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/sahel/data/csv/coeficientes_de_precipitación.csv", 
                          header = T) %>% 
  mutate(which = str_remove(which, ": poly"))
  

# sst 2020 - 2021
sst_2020_2021 <- readRDS("/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/sahel/data/rds/sst_2020_2021.rds") %>% 
  filter(reg_sst == "s3")

# cdr
cdr_21 <- readRDS("/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/sahel/data/rds/cdr2021_por_pixel.rds")

# 
pron_mes <- readRDS("/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/sahel/data/rds/pronostico_poly_mensual.rds")




# predicción 2021 
datos1 <- left_join(sst_2020_2021, coef_prc_poly, by = c("reg_sst" = "which")) %>% 
  group_by(x, y) %>% 
  mutate(pred_poly = b0 + b2*lag(sst, 11)^2 + b1*lag(sst, 11)) %>%   #, 
  # pred_sw = b0 + sst*sst_c + nino12*nino12_c + mslp*mslp_c)
  mutate(pred_poly = ifelse(pred_poly < 0, 0, pred_poly))


# CDR historico 
cdr_hist <- read_rds("/media/luis/644bedd8-fbc1-476d-9939-a613c80145d9/luisbalcazar/Documentos/sahel/data/rds/datos_f.rds")

# Matam 
# Labe 
# Bakel"

cdr_hist %>% 
  mutate(month = lubridate::month(date)) %>% 
  filter(name != "other", dplyr::between(month, 5, 10)) %>% 
  
  
  ggplot(aes(x = cdr)) +
  geom_histogram()+
  facet_grid(month ~ name, scales = "free")

plot(cdr)








