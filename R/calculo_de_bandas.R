library(fitdistrplus)
library(lubridate)
library(tidyverse)
library(sf)

band_limits <- function(x, 
                        prob = c(0.25, 0.5, 0.75), 
                        distr = "gamma"){
 method = c("mle", "mme", "qme", "mge", "mse")
 param = "error"
 i = 1
 while("error" %in% param){
  if(i > length(method)) stop("No fue posible ajustar la función")
  tryCatch(
   param <- fitdistrplus::fitdist(x, distr = distr, method = method[i]),
   error = function(e) "error"
  )
  i = i + 1
 }
 
 if("error" %in% param) {
  lims <- rep(NA, length(prob) + 1)
  return(setNames(lims, c(paste0("p", prob * 100), "normal")))
 }
 
 lims <- match.fun(paste0("q", distr))(
  prob, 
  shape = param$estimate["shape"], 
  rate = param$estimate["rate"])
 setNames(c(lims, mean(x)), c(paste0("p", prob * 100), "normal"))
}
# fin de función

# datos históricos CDR
cdr_hist <- read_rds("data/rds/pronostico_poly_mensual.rds")  %>% 
 dplyr::select(-pred_poly) %>% 
  dplyr::filter(lubridate::year(date) >= 1991)  # nueva normal según PRESAO 2021 

probs <- c(0.30, 0.5, 0.70)
# estacion <- "Labe"

cdr_test <- cdr_hist |> 
  filter(
    # name == estacion, 
    name != "other", 
    between(month(date), 5, 10), 
    lubridate::year(date) >= 1991)  # nueva normal según PRESAO 2021

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

plot_bandas <-
ggplot(cdr_pred21) +
  aes(x = date, y = pred) +
  geom_ribbon(aes(ymin = p30, ymax = p70, fill = "bands 0.30, 0.70"), alpha = 0.3) +
  scale_fill_manual(name = "", values = c("grey")) +
  geom_line(aes(y = normal, color = "Normal 1983-2020")) +
  geom_line(aes(y = p50, color = "Montly median")) +
  # geom_line(aes(y = cdr, color = "PERSIANN-CDR 2021")) +
  geom_point(aes(color = "Prediction")) +
  facet_wrap(~name) +
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
geom_text(data = data_sum, 
          aes(x = tipo, y = prc, label = prc), position = position_dodge(width = 0.9), 
          size = 3, vjust = -1, hjust = 0.5, col = "black")

# guardar plot
ggsave(filename = "plot_bandas.png", 
       plot = plot_bandas, device = 'png', 
       path = 'plots/', 
       units = 'cm', height = 22, width = 22, dpi = 300)  # height=14, width=12

# denscomp(param)
# abline(v = lims, col = "blue")
# abline(v = mean(cdr_test$cdr), col = "orange")
# abline(v = mean(cdr_pred21$cdr), col = "green")


## Grafico de barras apiladas

plot_col <-
 ggplot(cdr_pred21) +
  aes(x = date,    
      y = pred,
      xmin = date - 13,
      xmax = date + 13
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
 
   #   ########
   # dplyr::group_by(name) %>% 
   # summarise(cdr = sum(cdr), 
   #           pred = sum(pred), 
   #           normal = sum(normal))
   # ########
   # 
 
   scale_fill_manual(values = 
                         c("Wet"    = "blue", 
                           "Medium" = "yellow", 
                           "Dry"    = "red"), 
                       labels = 
                         c("Wet"    = "Wet", 
                           "Medium" = "Medium", 
                           "Dry"    = "Dry")) +
  geom_point(aes(y = normal, shape = "Normal 1991-2020")) +
  geom_point(aes(y = cdr, shape = "PERSIANN-CDR")) +
   geom_point(aes(y = pred, shape = "pronostico")) +
  scale_shape_manual(values = c(1,2,4)) +
  # geom_point(aes(y = p50, shape = "Probabilidad 0.5")) +
  # geom_linerange(aes(linetype = "Pronóstico 2021"), key_glyph = draw_key_path) +
  # scale_shape_discrete(guide = guide_legend(override.aes =
  #                                             list(linetype = c(0, 0, 0, 1), 
  #                                                  shape = c(15, 16, 17, NA)))) +
  labs(shape = "", fill = "", linetype = "", 
       x = "", y = expression(paste("Precipitation (mm ", month^-1, ")"))) +
  theme_bw() +
  facet_wrap(~name) +
  theme(legend.position = "bottom")
 
plot_col

 # guardar plot de barras
 ggsave(filename = "plot_col.png", 
        path = "plots/",
        plot= plot_col, 
        device = "png", 
        height = 20, width = 25, units = "cm")
 
 
 
 

 


#1. calcular bandas para todos los pixeles
#2. calcular el promedio humedo para todos los pixeles (mapa)
#3. unir las dos tablas.
#4. calcular clase (alto, medio, bajo)
#5. hacer un mapa

bandas <- cdr_hist |> 
 filter(between(month(date), 5, 10)) |> 
 group_by(month = month(date), name, x, y) |> 
 group_modify(~band_limits(.x$cdr, prob = probs) |> t() |> as.data.frame()) 


cdr_pred21map <- bandas |> 
 left_join( cdr_y_pred |> 
             filter(between(month(date), 5, 10)) |> 
             group_by(month = month(date), name, x, y) |> 
             summarise(pred = mean(pred_poly),
                       cdr = mean(cdr),
                       .groups = "drop")) |> 
 mutate(clase = case_when(pred > p70 ~ "Over",
                          pred < p30 ~ "Low",
                          TRUE ~ "Medium") |> 
         factor(levels = c("Low", "Medium", "Over")))
# cdr_pred21map

# 20 estaciones meteorológicas
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
  labs(x = "Longitude", y = "Latitude", fill = "class")
mapa
ggsave(filename = "mapa_clases_pronostico.png", 
       plot = mapa, 
       path = "plots/", 
       device = "png", height = 9, width = 8, dpi = 300)



