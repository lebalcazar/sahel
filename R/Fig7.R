library(fitdistrplus)
library(lubridate)
library(tidyverse)
library(sf)
source("R/functions.R")

# Locations of meteorological stations
ubcEst <- st_read("data/gpkg/vector_layers.gpkg", layer = "ubc_est20")

# Historical CDR data
cdr_hist <- read_rds("data/rds/pronostico_poly_mensual.rds")  %>% 
 dplyr::select(-pred_poly) %>% 
  dplyr::filter(lubridate::year(date) >= 1991)  # normal according PRESAO 2021 

#Forecast and CDR Data
forecast_data <- 
 left_join(read_rds("data/rds/cdr_2021.rds"), 
           read_rds("data/rds/pronostico_poly_2021.rds") %>% 
            dplyr::select(date, x, y, pred_poly), 
           by = c("x", "y", "date")) %>% 
 dplyr::select(date, x, y, name, cdr, pred = pred_poly) %>% 
 filter(between(lubridate::month(date), 5, 10))
 
pal <- c(Dry = "#f5686c", Normal = "#f9f33a", Wet = "#5b82ef")

# STEPS TO APPLY THE SAME PROCESS BEFORE TO ALL PIXELS
#1. Calculate thresholds for each pixel
#2. Calculate the mean for the humid period for each pixel in the map
#3. Tables join.
#4. Assign clase ("Dry", "Normal", "Wet") to each pixel
#5. Plot map

# 1. Calculate thresholds between zones of wetness based on probabilities 
probs <- c(0.30, 0.5, 0.70)

bands <- cdr_hist |> 
 filter(between(month(date), 5, 10)) %>%
 group_by(name, x, y, year = year(date)) %>% 
 summarise(cdr = sum(cdr, na.rm = TRUE), .groups = "drop_last") %>% 
 group_modify(~band_limits(.x$cdr, prob = probs) %>% t() %>% as.data.frame())


forecast_data <- 
 forecast_data |>
 group_by(name, x, y) |>
 summarise(pred = sum(pred), cdr = sum(cdr), .groups = "drop") |> 
 left_join(bands,  by = c("name", "x", "y")) |> 
 mutate(clase = case_when(pred > p70 ~ "Wet",
                          pred < p30 ~ "Dry",
                          TRUE ~ "Normal") |> 
         factor(levels = c("Dry", "Normal", "Wet")))


season_map_data <- bands |> 
 left_join(forecast_data |>
            group_by(name, x, y) |>
            summarise(pred = sum(pred),
                      cdr = sum(cdr),
                      .groups = "drop")) |> 
 mutate(clase = case_when(pred > p70 ~ "Wet",
                          pred < p30 ~ "Dry",
                          TRUE ~ "Normal") |> 
         factor(levels = c("Dry", "Normal", "Wet")))




#mapa <-
season_map_data %>% 
 ggplot() +
 aes(x, y) +
 geom_tile(aes(fill = clase)) +
 geom_sf(data = ubcEst) +
 geom_text(data = ubcEst, size = 3,
           aes(x = x, y = y, label = name), 
           nudge_y = c(rep(0.25, 10), -0.25, rep(0.25, 9))) +
 scale_fill_manual(values = pal, 
                   labels = 
                    c("Wet"    = "Over normal", 
                      "Normal" = "Around normal", 
                      "Dry"    = "Below normal")) +
 labs(x = "Longitude", y = "Latitude") +
 theme_classic(base_size = 10) +
 theme(panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(), 
       legend.position = c(0.99, 0.99),
       legend.justification = c(1, 1),
       legend.title = element_blank())


# guardar plot
ggsave(filename = "Fig7.png", path = 'outputs/plots/', device = 'png', 
       units = 'cm', height = 10, width = 10, 
       dpi = 300, scale = 1.25)
