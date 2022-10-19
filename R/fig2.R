library(sf)
library(ggplot2)
library(extrafont)
loadfonts(quiet = TRUE)

mslp <- st_read("data/gpkg/vector_layers.gpkg", "MSLP zones")
colnames(mslp) <- c("fid_", "layer", "geom")
mslp$variable <- "MSLP"

rhum <- st_read("data/gpkg/vector_layers.gpkg", "RHUM Zones")
colnames(rhum) <- c("fid_", "layer", "geom")
rhum$variable <- "RHUM"
sst <- st_read("data/gpkg/vector_layers.gpkg", "SST zones")
colnames(sst) <- c("fid_", "layer", "geom")
sst$variable <- "SST"

atlantic_zones <- rbind(sst, rhum) %>% 
 rbind(mslp)

atlantic_zones$variable <- factor(atlantic_zones$variable, 
                                  levels = c("SST", "RHUM", "MSLP"))
texts <- data.frame(x = c(-60, 0), 
                    y = c(-10, 20), 
                    label = c("South\nAmerica", "Africa"))


ggplot() +
 geom_sf(data = atlantic_zones, fill = "#6baed6", 
         color = "white", size = 0.3) +
 geom_sf_text(data = atlantic_zones, size = 3, family = "Palatino Linotype",
         aes(label = paste0(variable, layer))) +
 geom_text(data = texts, family = "Palatino Linotype",
           aes(x = x, y = y, label = label), size = 3) +
 facet_grid(~variable) +
 coord_sf(expand = FALSE) +
 theme_classic(base_size = 10, base_family = "Palatino Linotype") +
 theme(strip.text = element_blank(),
       strip.background = element_blank(), 
       panel.background = element_rect(fill = "#f2f2f2"),
       axis.title = element_blank())

ggsave("fig2.png", path = "outputs/plots", device = "png",
       width = 18.5, height = 5, units = "cm", 
       dpi = 300, scale = 1.25)
 
