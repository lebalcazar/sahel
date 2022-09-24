# Procesamiento de datos SST 
# nc contiene los datos SST de los años 2020-2021 



library(ncdf4)
library(ncdf4.helpers)
library(terra)


# abrir el archivo nc
nc <- ncdf4::nc_open("data/nc/sst_nc/nc_2020_2021.nc")

# # nombrar las variables del nc
# ncdf4.helpers::nc.get.dim.names(nc)
# 
# # variables
# ncdf4.helpers::nc.get.variable.list(nc)

# obtener las variables ta, lon-lat, tiempo
tmp <- ncvar_get(nc, "sst")

lon <- ncvar_get(nc, "longitude")

lat <- ncvar_get(nc, "latitude")

tiempo <- ncvar_get(nc, 'time')

# revisar los metadatos del nc: time  Size:12
time <- as.POSIXct("1900-01-01 00:00:00.0", tz = "UTC") +
  lubridate::hours(tiempo)

# dimensión de cada matriz del array
dim(tmp)

# se traspone el array, del conjunto de matrices
tmp <- arrayhelpers::ta(tmp)

# transformación de unidades, Kelvin (°K) a Celcius (°C)
tmp <- tmp - 273.15

# raster stack de referencia
rst <- raster::raster(tmp[,,1], 
                      crs = sp::CRS("+init=epsg:4326"))
raster::extent(rst) <-  c(range(lon) + c(0, 0.25), range(lat) + c(0, 0.25))
r_stack <- raster::stack(rst)

# llenar los datos en el raster stack
for(i in seq_len(dim(tmp)[3])){
  r_stack[[i]] <- raster::raster(tmp[,,i], 
                                 crs = sp::CRS("+init=epsg:4326")) %>% 
    `extent<-` (c(range(lon) + c(0, 0.25), range(lat) + c(0, 0.25)))
}
plot(r_stack[[1]])

# nombres del stack
names(r_stack) <- paste0("sst_", str_extract(time, "[12]\\d{3}-[0-9]\\d{1}-[0-9]\\d{1}")) #time # 

# rotar raster: 0:360° a -180:180°
rr_stack <- rotate(r_stack)

# recortar para el área de estudio 
# vector obtenido por ACP y análiis cluster
sst_poligonos <- st_read("data/gpkg/vector_layers.gpkg", layer = "Study area")
sst_poligonos_vec <- vect(sst_poligonos)

# recortar 
sst_crop <- terra::crop(rr_stack, sst_poligonos)

# mascara
sst_mask <- terra::mask(sst_crop, sst_poligonos)


# guarda los datos tif en la carpeta recien creada
terra::writeRaster(x = sst_mask, 
                   filename = paste0("data/tif/sst_2020_2021/",  
                                     names(sst_mask),
                                     ".tif"),
                   bylayer = TRUE
)
