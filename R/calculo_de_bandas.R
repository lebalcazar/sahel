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


cdr_hist <- read_rds("data/rds/pronostico_poly_mensual.rds")  %>% 
 dplyr::select(-pred_poly)

probs <- c(0.30, 0.5, 0.70)
estacion <- "Labe"

cdr_test <- cdr_hist |> 
 filter(name == estacion, between(month(date), 7, 9))

bandas <- group_by(cdr_test, month = month(date), name, x, y) |> 
 group_map(~band_limits(.x$cdr, prob = probs)) |> 
 reduce(bind_rows)

cdr_pred21 <- cdr_y_pred |> 
 rename(pred = pred_poly) |> 
 filter(name == estacion, between(month(date), 7, 9)) |> 
 bind_cols(bandas)

plot_bandas <- 
ggplot(cdr_pred21) +
  aes(x = date, y = pred) +
  geom_ribbon(aes(ymin = p30, ymax = p70, fill = "bands 0.30, 0.70"), alpha = 0.3) +
  scale_fill_manual(name = "", values = c("grey")) +
  geom_line(aes(y = normal, color = "Normal 1983-2020")) +
  geom_line(aes(y = p50, color = "Montly median")) +
  # geom_line(aes(y = cdr, color = "PERSIANN-CDR 2021")) +
  geom_point(aes(color = "Prediction")) +
  annotate(geom="text", 
           x = as.Date("2021-07-5"), 
           y = max(cdr_pred21$p70), 
           label = estacion) +
  # scale_color_brewer(type = "qual", palette = "Dark2") +

  theme_bw() +
  theme(legend.position = c(.60, 0.20), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(colour = NA, fill = NA)) +
  labs(x = "", 
       y = "precipitation", 
       color = "")

# guardar plot
ggsave(filename = paste0("bandas_", estacion, '.png'), 
       plot = plot_bandas, device = 'png', 
       path = 'plots_resultado/', 
       units = 'cm', height = 16, width = 20, dpi = 300)  # height=14, width=12

# denscomp(param)
# abline(v = lims, col = "blue")
# abline(v = mean(cdr_test$cdr), col = "orange")
# abline(v = mean(cdr_pred21$cdr), col = "green")


#1. calcular bandas para todos los pixeles
#2. calcular el promedio humedo para todos los pixeles (mapa)
#3. unir las dos tablas.
#4. calcular clase (alto, medio, bajo)
#5. hacer un mapa

bandas <- cdr_hist |> 
 filter(between(month(date), 7, 9)) |> 
 group_by(month = month(date), name, x, y) |> 
 group_modify(~band_limits(.x$cdr, prob = probs) |> t() |> as.data.frame()) 


cdr_pred21map <- bandas |> 
 left_join( cdr_y_pred |> 
             filter(between(month(date), 7, 9)) |> 
             group_by(month = month(date), name, x, y) |> 
             summarise(pred = mean(pred_poly),
                       cdr = mean(cdr),
                       .groups = "drop")) |> 
 mutate(clase = case_when(pred > p70 ~ "Alto",
                          pred < p30 ~ "Bajo",
                          TRUE ~ "Medio") |> 
         factor(levels = c("Bajo", "Medio", "Alto")))
cdr_pred21map

# 20 estaciones meteorológicas
ubcEst <- st_read('data/vector_dpkg/ubc_est20.gpkg')

# mapa 
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
        axis.text = element_text(size = 12))


