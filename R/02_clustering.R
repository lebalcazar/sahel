library(terra)
library(purrr)
library(RStoolbox)
source("R/functions.R")

# SST data 
sst_data <- rast("data/nc/sst_19832020_nc/sst_1983_2020.nc")

# PCA SST
rasterPCA_SST <- RStoolbox::rasterPCA(img = sst_data, 
                                      nSamples = NULL, nComp = nlyr(sst_data), spca = T) 
# CP
modelPCA_SST <- rasterPCA_SST$model

# ¿what's the optimum number of PCs?
plot(modelPCA_SST, type = 'line', main = "Principal Components")

# summary and model of PC
summary(rasterPCA_SST$map)
loadings(rasterPCA_SST$model)

# example of SST
pca_sst <- terra::rast("outputs/tif/pca/stack_PCA_2Comp_SST_Atlantico.tif") 
pca.smp <- pca_sst %>%
  terra::spatSample(size = 15000, method = "regular", na.rm = TRUE, xy=TRUE) %>%
  na.omit()

# number of K
k = best_k(pca.smp)

# change name
pca <- pca_sst

# convert to data frame
pca.df <- as.data.frame(pca, xy = TRUE) %>% na.omit()
sample <- pca.df[sample(nrow(pca.df), 15000), ]

pca_out <- pca[[1]]
for(i in seq_len(nrow(pca))){
  cls <- kmeans(pca.df[, -1], 3)
  pca_out <- stack(pca_out, 
                   cbind(pca.df[,1:2], cls$cluster) %>%
                     rasterFromXYZ(crs = crs(pca)))
  message("cluster", i, "completed")
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

# convert to polygon
rstPCAPoly <- rasterToPolygons(x = rstPCA, dissolve = TRUE) 
plot(rstPCAPoly, add = TRUE)


