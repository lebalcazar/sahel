library(fitdistrplus)
library(lubridate)
library(tidyverse)

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

pbb <- c(0.25, 0.5, 0.75)
estacion <- "Korhogo"

cdr_test <- cdr_hist |> 
 filter(name == estacion, between(month(date), 7, 9))

bandas <- group_by(cdr_test, month = month(date), name, x, y) |> 
 group_map(~band_limits(.x$cdr)) |> 
 reduce(bind_rows)

cdr_pred21 <- cdr_y_pred |> 
 rename(pred = pred_poly) |> 
 filter(name == estacion, between(month(date), 7, 9)) |> 
 bind_cols(bandas)


ggplot(cdr_pred21) +
 aes(x = date, y = pred) +
 geom_ribbon( aes(ymin = p25, ymax = p75), fill = "red", alpha = 0.3) +
 geom_line(aes(y = normal, color = "Normal 1983-2020")) +
 geom_line(aes(y = p50, color = "Montly median")) +
 geom_line(aes(y = cdr, color = "PERSIANN-CDR 2021")) +
 geom_point(aes(color = "Prediction")) +
 scale_color_brewer(type = "qual", palette = "Dark2") +
 theme_classic()

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
 group_modify(~band_limits(.x$cdr) |> t() |> as.data.frame()) 

cdr_pred21map <- bandas |> 
 left_join( cdr_y_pred |> 
             filter(between(month(date), 7, 9)) |> 
             group_by(month = month(date), name, x, y) |> 
             summarise(pred = mean(pred_poly),
                       cdr = mean(cdr),
                       .groups = "drop")) |> 
 mutate(clase = case_when(pred > p75 ~ "Alto",
                          pred < p25 ~ "Bajo",
                          TRUE ~ "Medio") |> 
         factor(levels = c("Bajo", "Medio", "Alto")))

ggplot(cdr_pred21map) +
 aes(x, y) +
 geom_raster(aes(fill = clase)) +
 coord_equal()
