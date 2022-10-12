library(terra)
library(purrr)
library(RStoolbox)
source("R/functions.R")

# PCA SST
lf <- list.files(path = "data/tif/sst_1983_2020/", 
                 pattern = ".tif$", full.names = TRUE)
sst_data <- terra::rast(lf)
rasterPCA_SST <- RStoolbox::rasterPCA(img = sst_data, 
                                      nSamples = NULL, nComp = nlyr(sst_data), spca = T) 
# CP
modelPCA_SST <- rasterPCA_SST$model

# ¿número óptimo de PC?
plot(modelPCA_SST, type = 'line', main = "Principal Components")

# resúmenes
summary(rasterPCA_SST$map)
loadings(rasterPCA_SST$model)

summary(modelPCA_SST)

pca_sst <- terra::rast("data/tif/pca/stack_PCA_2Comp_SST_Atlantico.tif") 


pca.smp <- pca_sst |>
  terra::spatSample(size = 15000, method = "regular", na.rm = TRUE, xy=TRUE) |>
  na.omit()

k = best_k(pca.smp)

# cluster para regiones REVISAR!!!
# pca.df <- as.data.frame(pca_sst, xy = TRUE) |> na.omit()
# pca_out <- pca_sst[[1]]
# for(i in seq_len(nrow(pca_sst))){
#   cls <- kmeans(pca.df[, -1], 3)
#   pca_out <- stack(pca_out, 
#                    cbind(pca.df[,1:2], cls$cluster) %>%
#                      rasterFromXYZ(crs = crs(pca_sst)))
#   message("cluster", i, "terminado")
# }

pca <- pca_sst

pca.df <- as.data.frame(pca, xy = TRUE) %>% na.omit()
sample <- pca.df[sample(nrow(pca.df), 15000), ]

pca_out <- pca[[1]]
for(i in seq_len(nrow(pca))){
  cls <- kmeans(pca.df[, -1], 3)
  pca_out <- stack(pca_out, 
                   cbind(pca.df[,1:2], cls$cluster) %>%
                     rasterFromXYZ(crs = crs(pca)))
  message("cluster", i, "terminado")
}

rstPCA <- calc(pca_out, modal)
nlayers(pca_out)
plot(rstPCA)

# change raster labels 
matrix <- matrix(c(
  1, 2,
  2, 1,
  3, 3), byrow = TRUE, ncol = 2)

rstPCA <- reclassify(x = rstPCA, rcl = matrix)
plot(rstPCA)

# converti a polígono
rstPCAPoly <- rasterToPolygons(x = rstPCA, dissolve = TRUE) 
plot(rstPCAPoly, add = TRUE)

# save
writeRaster(x = rstPCA, '../lbo_doc/resultados/PCA_SST_componentes/PCA_2_cluster_3_final.tif')
