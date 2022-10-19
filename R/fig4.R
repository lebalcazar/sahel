library(tidyverse)
library(sf)

basin <- st_read('data/gpkg/vector_layers.gpkg', layer = "river basins") 
sst_labs <- c("SST1 (5)", "SST2 (10)", "SST3 (11)")
names(sst_labs) <- c("s1", "s2", "s3")

mdl_perf %>%
  separate(col = id, 
           into = c("group_number", "region_sst", "region_mslp", 
                    "region_rhum", "name", "x", "y"), 
           sep = "_", 
           convert = TRUE) %>% 
  filter(model == "poly") %>% 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = NSE)) + 
  geom_sf(data = basin, fill = NA) +
  scale_fill_viridis_c(breaks = seq(0.20, 1, 0.1)) +
  theme_classic(base_size = 12, base_family = "Palatino Linotype") +
  facet_grid(~region_sst, labeller = labeller(region_sst = sst_labs))+
  theme(strip.background = element_rect(fill = "#f2f2f2"), 
        panel.spacing = unit(1, "lines")) +
  labs(x = "Longitude", y = "Latitude")

# save plot
ggsave(filename = "Fig4.png", plot = last_plot(),
       device = "png", path = "outputs/plots/",
       width = 18.5, height = 7, units = "cm", 
       dpi = 300, scale = 1.2)
