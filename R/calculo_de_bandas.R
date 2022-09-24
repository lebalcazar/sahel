library(fitdistrplus)
library(lubridate)
library(tidyverse)
library(sf)
source("R/functions.R")

# Historical CDR data
cdr_hist <- read_rds("data/rds/pronostico_poly_mensual.rds")  %>% 
 dplyr::select(-pred_poly) %>% 
  dplyr::filter(lubridate::year(date) >= 1991)  # normal according PRESAO 2021 

probs <- c(0.30, 0.5, 0.70)
# estacion <- "Labe"

cdr_test <- cdr_hist |> 
  filter(
    # name == estacion, 
    name != "other", 
    between(month(date), 5, 10), 
    lubridate::year(date) >= 1991)  # normal according PRESAO 2021 

bandas <- group_by(cdr_test, month = month(date), name, x, y) |> 
 # group_map(~band_limits(.x$cdr, prob = probs)) |> 
 group_modify(~band_limits(.x$cdr, prob = probs) %>% 
                t() %>% 
                tibble::as_tibble()) # |> 
 # reduce(bind_rows)

cdr_y_pred <- 
 left_join(read_rds("data/rds/cdr_2021.rds"), 
           read_rds("data/rds/pronostico_poly_2021.rds") %>% 
            dplyr::select(date, x, y, pred_poly), 
           by = c("x", "y", "date")) %>% 
 dplyr::select(date, x, y, name, cdr, pred_poly) %>% 
 filter(between(lubridate::month(date), 5, 10))

cdr_pred21 <- 
  cdr_y_pred |> 
 rename(pred = pred_poly) |> 
 filter(
   # name == estacion, 
   name != "other", 
   between(month(date), 5, 10)) |>
  dplyr::mutate(month = lubridate::month(date)) %>%
  dplyr::select(month, date, name, x, y, cdr, pred) %>% 
 # bind_cols(bandas)
  left_join(bandas, by = c("month", "name", "x", "y"))

# plot_bandas <-
ggplot(cdr_pred21) +
  aes(x = date, y = pred) +
  geom_ribbon(aes(ymin = p30, ymax = p70, fill = "bands 0.30, 0.70"), alpha = 0.3) +
  scale_fill_manual(name = "", values = c("grey")) +
  geom_line(aes(y = normal, color = "Normal 1983-2020")) +
  geom_line(aes(y = p50, color = "Montly median")) +
  # geom_line(aes(y = cdr, color = "PERSIANN-CDR 2021")) +
  geom_point(aes(color = "Prediction")) +
  facet_wrap(~name)
  # annotate(geom = "text", 
  #          x = as.Date(paste0("2021-", cdr_pred21$month, "-01")), 
  #          y = max(cdr_pred21$p70), 
  #          label = "estacion"
  #          ) +
  # scale_color_brewer(type = "qual", palette = "Dark2") +

  theme_bw() +
  theme(legend.position = c(.90, 0.10), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(colour = NA, fill = NA)) +
  labs(x = "", 
       y = "precipitation", 
       color = "") +


# Save plot
ggsave(filename = "plot_bandas.png", 
       plot = plot_bandas, 
       device = 'png', 
       path = 'outputs/plots/', 
       units = 'cm', height = 22, width = 22, dpi = 300) 


  
#   ########
  # accumulate the humid period may-oct: sum_CDR, sum_NORMAL, sum_PRONÓSTICO
  data_sum <-
    cdr_pred21 %>% 
    # dplyr::filter(name == "Tidjikja") %>% 
    dplyr::select(date, name, 'PERSSIAN-CDR' = cdr, Forecast = pred, "Normal" = normal) %>%  
    pivot_longer(cols = -c(date, name),  
                 names_to = "tipo",
                 # names_pattern = "p(.+)",
                 # names_transform = list(prob = as.numeric),
                 values_to = "prc") %>% 
    dplyr::group_by(name, tipo) %>% 
    dplyr::summarise(y = max(prc) |> round(0),
                     prc = sum(prc) |> round(0),
                    .groups = "drop_last") %>%
   dplyr::summarise(y = max(y),
                    date = as.Date("2021-05-01"),
                    label = glue::glue("{a}", a = paste0("", tipo, ": ", prc, 
                                        " [mm] \n", collapse = "")))
    
  
  # data_sum <-  cdr_pred21 %>% 
  #   dplyr::group_by(date, name) %>%
  #   dplyr::summarise(date = date, 
  #                    cdr = sum(cdr),
  #                    pred = sum(pred),
  #                    normal = sum(normal))
# ########
  # 
  
  
  
## Stacked bars plot

plot_col <-
 ggplot(cdr_pred21) +
  aes(x = date,    
      y = pred,
      # xmin = date - 13,
      # xmax = date + 13
      ) +
  geom_col(data = dplyr::select(cdr_pred21, date, name, p30, p50, p70, cdr, pred, normal) |> 
            pivot_longer(cols = -c(date, name, cdr, pred, normal),  
                         names_to = "prob",
                         # names_pattern = "p(.+)",
                         # names_transform = list(prob = as.numeric),
                         values_to = "valor") |> 
            group_by(date, name) |> 
            mutate(valor = c(valor[1], diff(valor)),
                   class = recode_factor(prob, 
                                         "p70" = "Wet", 
                                         "p50" = "Medium", 
                                         "p30" = "Dry")), 
           mapping = aes(date, valor, fill = class), alpha = 0.6) +

   scale_fill_manual(values = 
                         c("Wet"    = "blue", 
                           "Medium" = "yellow", 
                           "Dry"    = "red"), 
                       labels = 
                         c("Wet"    = "Over normal", 
                           "Medium" = "Around normal", 
                           "Dry"    = "Below normal")) +
  geom_point(aes(y = normal, shape = "Normal (1991-2020)")) +
  geom_point(aes(y = cdr, shape = "PERSIANN-CDR")) +
  geom_point(aes(y = pred, shape = "Forecast")) +
  scale_shape_manual(values = c(1,2,4)) +
  # geom_point(aes(y = p50, shape = "Probabilidad 0.5")) +
  # geom_linerange(aes(linetype = "Pronóstico 2021"), key_glyph = draw_key_path) +
  # scale_shape_discrete(guide = guide_legend(override.aes =
  #                                             list(linetype = c(0, 0, 0, 1), 
  #                                                  shape = c(15, 16, 17, NA)))) +
  labs(shape = "", fill = "", linetype = "", 
       x = "", y = expression(paste("Precipitation (mm ", month^-1, ")"))) +
  theme_bw() +
  facet_wrap(~name) + # scales = "free_y"
  # annotate("text", label = "text", x = as.Date("2021-05-01"), y = 400) +
   # geom_text(data = data_sum,
   #           aes(x = as.Date("2021-05-01"), y = prc, label = prc), position = position_dodge(width = 0.9), 
   #           size = 3, vjust = -1, hjust = 0.5, col = "black") +

    geom_text(data = data_sum,
                    aes(x = as.Date("2021-05-01") - days(15), y = 490, label = label), 
                    position = position_identity(), 
                    size = 2, vjust = 1, hjust = 0, col = "black") +
  theme(legend.position = "bottom")
 
  plot_col
  

  # guardar plot
  ggsave(filename = "plot_col_.png", 
         plot = plot_col, device = 'png', 
         path = 'plots/', 
         units = 'cm', height = 18, width = 30, dpi = 300)  # height=14, width=12
  
  
  
  ggplot() +
  annotate("text", 
           x = data_sum$date, 
           y = data_sum$y, 
           label = data_sum$label, 
            size = 3, vjust = 1, hjust = 0, col = "black")
  
 # Save stacked bar plot
 ggsave(filename = "plot_col.png", 
        path = "outputs/plots/",
        plot= plot_col, 
        device = "png", 
        height = 20, width = 25, units = "cm")
 
 
 

# STEPS TO APPLY THE SAME PROCESS BEFORE TO ALL PIXELS
#1. Calculate bands for each pixel
#2. Calculate the mean for the humid period for each pixel in the map
#3. Tables join.
#4. Assign clase ("Low", "Medium", "Over") to each pixel
#5. Plot map

bands <- cdr_hist |> 
 filter(between(month(date), 5, 10)) |> 
 group_by(month = month(date), name, x, y) |> 
 group_modify(~band_limits(.x$cdr, prob = probs) |> t() |> as.data.frame()) 


cdr_pred21map <- bands |> 
 left_join( cdr_y_pred |> 
             filter(between(month(date), 5, 10)) |> 
             group_by(month = month(date), name, x, y) |> 
             summarise(pred = mean(pred_poly),
                       cdr = mean(cdr),
                       .groups = "drop")) |> 
 mutate(clase = case_when(pred > p70 ~ "Wet",
                          pred < p30 ~ "Dry",
                          TRUE ~ "Normal") |> 
         factor(levels = c("Dry", "Normal", "Wet")))
# cdr_pred21map

# 20 meteorological stations
ubcEst <- st_read('data/vector_dpkg/ubc_est20.gpkg')

mapa <- 
ggplot(cdr_pred21map) +
  aes(x, y) +
  geom_tile(aes(fill = clase)) +
  coord_equal() +
  geom_sf(data = ubcEst) +
  geom_text(data = ubcEst, 
            aes(x = x, y = y, label = name), 
            nudge_y = c(rep(0.25, 10), -0.25, rep(0.25, 11))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = c(0.85, 0.85), 
        axis.text = element_text(size = 12)) +
  scale_fill_continuous(
    Dry = "red", 
    Normal = "yellow", 
    Wet = "blue"
  ) +
  labs(x = "Longitude", y = "Latitude", fill = "Legend")
mapa
ggsave(filename = "mapa_clases_pronostico_2.png", 
       plot = mapa, 
       path = "plots/", 
       device = "png", height = 16, width = 16, dpi = 300, units = "cm")
