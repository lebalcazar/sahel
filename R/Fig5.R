library(tidyverse)




# best model performance 
performance_best <- read_csv("outputs/models/model_performance.csv") %>% 
  separate(col = id, 
           into = c("group_number", "region_sst", "region_mslp", "region_rhum", "name", "x", "y"), 
           sep = "_", 
           convert = TRUE
           ) %>%
  group_by(x, y) %>% 
  # filter the polynomial model and region 3
  dplyr::filter(model == "poly", region_sst == "s3") |>  
  filter(p.value < 0.05, R2_test >= 0.2) %>%
  top_n(-1, AIC) %>%
  mutate(which = paste(region_sst, model, sep = ": ") %>% 
           as.factor(),
         pixel_model = as.numeric(which)
  ) %>% 
  dplyr::select(model, region_sst, region_mslp, region_rhum, name, x, y, which, group_number)

# table of coefficients
coef_mpoly <- read_csv("outputs/models/model_coefs.csv") %>%
  separate(col = id, into = c("group_number", 
                              "region_sst", "region_mslp", "region_rhum", "name", "x", "y"), 
           sep = "_", convert = TRUE
  ) %>%
  dplyr::select(model, term, estimate, name, x, y, region_sst, group_number)


# filter coefficients with the best performance
data_c <- left_join(performance_best, 
                    coef_mpoly, 
                    by = c("model", "x", "y", "name", 
                           "region_sst", "group_number")
) %>% 
  mutate(term = case_when(term == "(Intercept)" ~ "b0", 
                          term == "poly(sst, 2, raw = TRUE)1" ~ "b1",
                          term == "poly(sst, 2, raw = TRUE)2" ~ "b2", 
                          TRUE ~ paste0(term, "_c"))
  ) %>% 
  pivot_wider(id_cols = c(x, y, which), 
              names_from = c(term), 
              values_from = estimate)

# SST data
forescast <- readRDS("data/rds/data_timeseries.rds") %>%
# datos <-  datos0 %>% 
  dplyr::select(date, x, y, name, cdr, 
                mslp, sst, reg_sst, reg_rhum, reg_mslp) %>% 
  filter(reg_rhum == "r1", reg_mslp == "m1", reg_sst == "s3") %>% 
  group_by(x, y, reg_sst) %>% 
  mutate(sst = lag(sst, 11)) %>%   
  left_join(., data_c, by = c("x", "y")) %>% 
  mutate(pred_poly = b0 + b2*sst^2 + b1*sst) %>%  
  mutate(pred_poly = ifelse(pred_poly < 0, 0, pred_poly)) %>% 


# datos1 <- read_rds("../lbo_doc/resultados/pronostico_prc_rds_por_pixel/pronostico_poly_mensual.rds")
# # plot año mes del pronostico espacial
# # coef_filtrado %>%
# plot_prn <-
#   datos %>%
#   # dato_prn %>% 
  filter(date >= "2005-01-01") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) 
map <- 
forescast %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = pred_poly)) +
  facet_grid(year~month, labeller = labeller(month = c(`1` = "Jan", `2` = "Feb", `3` = "Mar", 
                                                       `4` = "Apr", `5` = "May", `6` = "Jun", 
                                                       `7` = "Jul", `8` = "Aug", `9` = "Sep", 
                                                       `10`= "Oct", `11`= "Nov", `12`= "Dec"))
  ) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "GnBu")) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = 'Longitude', y = 'Latitude', fill = "Precipitation (mm)")

# guardar el plot 
ggsave(filename = "outputs/plots/fi5.png",  
       plot = map, device = "png", units = "cm", width = 20, height = 12, dpi = 300)


ggsave(filename = "outputs/plots/fig5.png", 
       plot = last_plot(), device = 'png', units = 'cm', height = 18, width = 14, dpi = 300)

p <- 
iris %>% 
  ggplot(aes(Sepal.Length, Sepal.Width, colour = Species)) +
  geom_point()
p
ggplot2::ggsave(filename = "outputs/plots/ff.png", plot = p,
       device = "png", dpi = 300, height = 18, width = 14)

#







# una tabla con el promedio por mes y el total anual 
tbl_month <- 
  datos1 %>% ungroup() %>% 
  select(date, x, y, pred_poly) %>% 
  group_by(month = lubridate::month(date), 
           year = lubridate::year(date)) %>% 
  summarise(mean_month = round(mean(pred_poly, na.rm = TRUE), 1), 
            mean_month = ifelse(mean_month < 10, 0, mean_month)) %>%  # < 1mm día es 0, un mes 30mm
  pivot_wider(id_cols = year, names_from = month, values_from = mean_month) %>% 
  # ungroup() %>% 
  mutate(total_anual = rowSums(.[, 2:13]))

# guardar la tabla 
write_csv(x = tbl_month, 
          file = "../lbo_doc/resultados/tablas/prec_menaul_total_anual_cuencas.csv", 
          col_names = TRUE)
