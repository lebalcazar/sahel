library(tidyverse)
library(sf)

# data of output models
performance = read_csv("outputs/models/model_performance.csv")

# tidy data
performance_best <- performance %>% 
  separate(col = id,
           into = c("group_number", "region_sst", "regions_mslp", "region_rhum", "name", "x", "y"),
           sep = "_",
           convert = TRUE) %>%
  dplyr::group_by(x, y) %>% 
  # select best modeles
  dplyr::filter(p.value < 0.05, adj.r.squared > 0.5) %>%
  top_n(-1, AIC) %>%
  dplyr::mutate(which = paste(region_sst, model, sep = ": ") %>%  
           as.factor(),
         pixel_model = as.numeric(which)
  ) 

# polygons of basin and rain gauge
basin <- st_read('data/gpkg/vector_layers.gpkg', layer = "basin")  
rain_gauges <- st_read("data/gpkg/vector_layers.gpkg", layer = "ubc_est20")

# Which region delivers the best performance?
ggplot() +
  geom_tile(data = performance_best, 
            aes(x, y, fill = which)) +
  geom_sf(data = basin, fill = NA) +
  geom_sf(data = rain_gauges) +
  geom_text(data = rain_gauges, aes(x = x, y = y, label = name), 
            # position = position_dodge(width = 1),
            vjust = -0.2) +
  scale_fill_brewer(palette = "Dark2",
                    labels = c("SST1: polynomial", "SST3: polynomial", "SST3: stepwise")) +
  labs(x = "Longitude", y = "Latitude", fill = "Region: model") + 
  theme_bw(base_size = 10) +
  theme(legend.position = c(.82,.86),        
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 


ggsave(filename = 'outputs/plots/fig3.png', plot = last_plot(), device = 'png', 
      units = 'cm', height = 18, width = 14, dpi = 300)
