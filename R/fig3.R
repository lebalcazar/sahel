library(tidyverse)
library(sf)
library(extrafont)
#extrafont::ttf_import("data/ttf/Palatino Linotype.ttf")

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
basin <- st_read('data/gpkg/vector_layers.gpkg', layer = "river basins") 
rain_gauges <- st_read("data/gpkg/vector_layers.gpkg", layer = "rain gauges")
pal <- c("s1: poly" = "#ffffb3", "s3: poly" = "#fdb462", "s3: step" = "#fb8072")
 
# Which region delivers the best performance?
ggplot() +
  geom_tile(data = performance_best, 
            aes(x, y, fill = which)) +
  geom_sf(data = basin, fill = NA) +
  geom_sf(data = rain_gauges) +
  geom_text(data = rain_gauges, size = 3,
           aes(x = x, y = y, label = name), 
           nudge_y = c(rep(0.25, 12), -0.25, rep(0.25, 7))) +
 scale_fill_manual(values = pal,
                   labels = c("SST1: polynomial", "SST3: polynomial", "SST3: stepwise")) +
 labs(x = "Longitude", y = "Latitude") +
 theme_classic(base_size = 10, base_family = "Palatino Linotype") +
 theme(panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(), 
       legend.position = c(0.99, 0.99),
       legend.justification = c(1, 1),
       legend.title = element_blank(), 
       legend.key.size = unit(0.04, "npc"))

# save last plot
ggsave(filename = "Fig3.png", path = 'outputs/plots/', device = 'png', 
       units = 'cm', height = 10, width = 10, 
       dpi = 300, scale = 1.25)
