# procesar datos de pronóstico 
library(tidyverse)
library(terra)
library(sf)

# estaciones 
coorEstCnc <- readRDS('data/rds/coorEstCnc.rds') %>% as_tibble()
  
tbl <- list.files("data/tif/cdr_2021/", ".tif", full.names = TRUE) |> 
 rast() %>%
 terra::as.data.frame(xy = TRUE) %>% 
 as_tibble() %>%
 left_join(coorEstCnc, ., by = c("x", "y")) %>%  
 pivot_longer(cols = -c(x, y, name), names_to = "date", values_to = "cdr") %>% 
 mutate(date = as.Date(paste(date, 01), "CDR_%Y%m%d"), 
        id = paste(x, y, name, sep = "_")) %>% 
 mutate(cdr = ifelse(cdr < 1, 0, cdr)) %>% 
 mutate(cdr = ifelse(is.na(cdr), 0, cdr)) %>% 
 ungroup() %>% 
 dplyr::select(x, y, name, date, cdr)

# guardar
saveRDS(tbl, "data/rds/cdr_2021.rds")
