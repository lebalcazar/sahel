library(tidyverse)
library(sf)

rendimiento = read_csv("outputs/models/model_performance.csv")

rendimiento_best <- rendimiento %>% 
  separate(col = id,
           into = c("group_number", "region_sst", "region_rhum", "name", "x", "y"),
           sep = "_",
           convert = TRUE) %>%
  dplyr::group_by(x, y) %>%    # model, region
  dplyr::filter(p.value < 0.05, adj.r.squared > 0.5) %>%   # adj.r.squared >= 0.5
  top_n(-1, AIC) %>%
  # top_n(1, NSE) %>%
  dplyr::mutate(which = paste(region_sst, model, sep = ": ") %>%  
           as.factor(),
         pixel_model = as.numeric(which)
  ) 


# basin <- st_read('~/Documentos/04SIG/Wrk/SahelTesis/mask_cuencas.gpkg')
basin <- st_read('data/gpkg/cuencasAfr.gpkg')  #Documentos/04SIG/Wrk/SahelTesis/mask_cuencas.gpkg')
est <- st_read("data/gpkg/vector_layers.gpkg", layer = "ubc_est20")

# ¿Qué región genera el mejor rendimiento? 
ggplot() +
  geom_tile(data = rendimiento_best, 
            aes(x, y, fill = which)) +
  geom_sf(data = basin, fill = NA) +
  geom_sf(data = est) +
  geom_text(data = est, aes(x = x, y = y, label = name), 
            # position = position_dodge(width = 1),
            vjust = -0.2) +
  scale_fill_brewer(palette = "Dark2",
                    labels = c("SST1: polynomial", "SST3: polynomial", "SST3: stepwise")) +
  labs(x = "Longitude", y = "Latitude", fill = "Region") + 
  theme_bw(base_size = 10) +
  
 
  
  theme(legend.position = c(.82,.86),         # .80, .85
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 


pRegn <- plot_grid(pReg0, pReg)
pRegn

ggsave(filename = 'modelo_TaERA5_sin_con_restriccion_02.png', plot = pRegn, device = 'png', 
       path = '../lbo_doc/modelos/', units = 'cm', height = 30, width = 26, dpi = 300)  # height=14, width=12