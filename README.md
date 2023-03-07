# About repository

Seasonal Precipitation Forecasting `(SPF-Sahel)`

The `SPF-Sahel` repository is a GNU-GPL project. The goal of this repository is to provide scripts and data for reproducible research Development and assessment of seasonal rainfall forecasting models for the Bani and the Senegal basins by identifying the best predictive teleconnection.

`data/gpkg/`: is a spatial data vector file. The location of weather stations are derived from the Bâ et al. (2018) databases. Rivers and boundaries are data from https://www.hydrosheds.org/products, and data for SST, RHUM and MSLP regions that comes from PCA and cluster analysis. The watersheds are obtained from a 90-meter of DEM https://srtm.csi.cgiar.org/.
`data/nc/` contains SST and CDR raster data. 
`data/tif/` includes short rasters data (2000 and 2001).
`data/rds/` contains a table of data base. All data in data_timeseries.rds

`R/`: contains the scripts for data analysis, model development, and the results of tables and figures.

`Outputs/` cotains plot of figures, tables and coefficients models.

# Reproducibility

The repository uses `renv` to recover the versions of packages used in the mayority 
of analyses of this project. To reproduce, follow these steps:

- Download a copy or clone the repository
- If you are using RStudio the environment will automatically be activated. If not
you could run

```r
renv::activate()
renv::restore()
```

If you don't have installed the pacakges required by the project you will see a
installation process displayed.

# References
Bâ, K.; Balcázar, L.; Diaz, V.; Ortiz, F.; Gómez-Albores, M.; Díaz-Delgado, C. Hydrological Evaluation of PERSIANN-CDR Rainfall over Upper Senegal River and Bani River Basins. Remote Sensing 2018, 10, 1884, doi:10.3390/rs10121884.
