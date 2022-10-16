library(fitdistrplus)
library(lubridate)
library(tidyverse)
library(sf)
source("R/functions.R")

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


# Calculate thresholds between zones of wetness based on probabilities 
probs <- c(0.30, 0.5, 0.70)

bands <- cdr_hist |> 
 filter(name != "other", 
        between(month(date), 5, 10), 
        lubridate::year(date) >= 1991) %>%  # normal according PRESAO 2021 
 group_by(month = month(date), name, x, y) |> 
 group_modify(~band_limits(.x$cdr, prob = probs) %>% 
               t() %>% 
               tibble::as_tibble())


forecast_data <- 
 forecast_data |> 
 filter(name != "other", between(month(date), 5, 10)) |>
 dplyr::mutate(month = lubridate::month(date)) %>%
 dplyr::select(month, date, name, x, y, cdr, pred) %>% 
 left_join(bands, by = c("month", "name", "x", "y"))



  
#   ########
  # accumulate the humid period may-oct: sum_CDR, sum_NORMAL, sum_PRONÓSTICO
forcast_summary <-
 forecast_data %>% 
 dplyr::select(date, name, 
               'PERSSIAN-CDR' = cdr, 
               Forecast = pred, 
               "Normal" = normal) %>%  
 tidyr::pivot_longer(cols = -c(date, name),  
                     names_to = "tipo",
                     values_to = "prc") %>% 
 dplyr::group_by(name, tipo) %>% 
 dplyr::summarise(y = max(prc) |> round(0),
                  prc = sum(prc) |> round(0),
                  .groups = "drop_last") %>%
 dplyr::summarise(y = max(y),
                  date = as.Date("2021-05-01"),
                  label = paste0("", tipo, ": ", prc, 
                                 " [mm] \n", collapse = ""))


## Stacked bars plot
ggplot(forecast_data) +
 aes(x = date, y = pred) +
 geom_col(data = dplyr::select(forecast_data, 
                               date, name, p30, p50, p70, 
                               cdr, pred, normal) |> 
           pivot_longer(cols = -c(date, name, cdr, pred, normal),  
                        names_to = "prob",
                        values_to = "valor") |> 
           group_by(date, name) |> 
           mutate(valor = c(valor[1], diff(valor)),
                  class = recode_factor(prob, 
                                        "p70" = "Wet", 
                                        "p50" = "Normal", 
                                        "p30" = "Dry")), 
          mapping = aes(date, valor, fill = class)) +
 scale_fill_manual(values = pal, 
                   labels = 
                    c("Wet"    = "Over normal", 
                      "Normal" = "Around normal", 
                      "Dry"    = "Below normal")) +
 geom_point(aes(y = normal, shape = "Normal (1991-2020)")) +
 geom_point(aes(y = cdr, shape = "PERSIANN-CDR")) +
 geom_point(aes(y = pred, shape = "Forecast")) +
 geom_text(data = forcast_summary,
           aes(x = as.Date("2021-05-01") - days(15), y = 490, label = label), 
           position = position_identity(), 
           size = 2.5, vjust = 1, hjust = 0, col = "black") +
 scale_shape_manual(values = c(1,2,4)) +
 facet_wrap(~name) +
 labs(shape = "", fill = "", linetype = "", 
      x = "", y = expression(paste("Precipitation (mm ", month^-1, ")"))) +
 theme_bw(base_size = 10) +
 theme(legend.position = "bottom",
       panel.grid = element_blank())

# guardar plot
ggsave(filename = "Fig6.png", device = 'png', 
       path = 'outputs/plots/', 
       units = 'cm', height = 10, width = 18.5, 
       dpi = 300, scale = 1.8)
 
