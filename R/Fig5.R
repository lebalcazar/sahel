

# con los datos del modelo polinomial 
# arreglar los coeficientes

coeficientes = read_csv("../lbo_doc/modelos/model_lm_nls_step/model_coefs.csv")
rendimiento = read_csv("../lbo_doc/modelos/model_lm_nls_step/model_performance.csv")

# mejor rendimiento por pixel
rendimiento_best <- rendimiento %>%
  separate(col = id, 
           into = c("group_number", "region_sst", "region_mslp", "region_rhum", "name", "x", "y"), 
           sep = "_", 
           convert = TRUE) %>%
  group_by(x, y) %>% 
  # filtrar el modelo polinomial para toda la cuenca
  dplyr::filter(model == "poly", region_sst == "s3") |>  # aqui se filtra el modelo polinomial y la region 3
  filter(p.value < 0.05, R2_test >= 0.2) %>%   # utilizar todos los pixeles
  top_n(-1, AIC) %>%
  # top_n(1, NSE) %>% 
  mutate(which = paste(region_sst, model, sep = ": ") %>% 
           as.factor(),
         pixel_model = as.numeric(which)
  ) %>% 
  dplyr::select(model, region_sst, region_mslp, region_rhum, name, x, y, which, group_number)

# preparar la tabla de coeficientes
coef_mpoly <- coeficientes %>%
  separate(col = id, into = c("group_number", 
                              "region_sst", "region_mslp", "region_rhum", "name", "x", "y"), 
           sep = "_", convert = TRUE
  ) %>%
  dplyr::select(model, term, estimate, name, x, y, region_sst, group_number)
coef_mpoly

# filtran los coefecientes con los mejores rendimientos
data_c <- left_join(rendimiento_best, 
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

datos0 <- readRDS('../lbo_doc/resultados/datos_preProceso/datos_f.rds') 
datos <-  datos0 %>% 
  dplyr::select(date, x, y, name, cdr, #nino12, 
                mslp, sst, reg_sst, reg_rhum, reg_mslp) %>%   # ,nino12) %>% 
  filter(reg_rhum == "r3", reg_mslp == "m1", reg_sst == "s3") %>% 
  group_by(x, y, reg_sst) %>% 
  mutate(sst = lag(sst, 11))  #, 
# nino12 = lag(nino12, 11),
# mslp = lag(mslp, 11))

# predicción con todos los datos 
datos1 <- left_join(datos, data_c, by = c("x", "y")) %>% 
  mutate(pred_poly = b0 + b2*sst^2 + b1*sst) %>%   #, 
  # pred_sw = b0 + sst*sst_c + nino12*nino12_c + mslp*mslp_c)
  mutate(pred_poly = ifelse(pred_poly < 0, 0, pred_poly))  
(p1 <- 
    datos1 %>% 
    filter(name == "Goudiry") %>% 
    mutate(pred_poly = ifelse(pred_poly < 0, 0, pred_poly)))  

p1 %>% 
  ggplot(aes(x = cdr, y = pred_poly)) +
  geom_point()

# ml <- lm(pred_poly ~ cdr, data = p1)
# summary(ml)



# preparar los datos de pronostico para exportar  
dato_prn <- datos1 |> 
  ungroup() |>
  group_by(x, y) |>
  dplyr::select(date, name, x, y, cdr, pred_poly) |> 
  mutate(id_xy = paste(x, y, sep = "_"))

saveRDS(dato_prn, "../lbo_doc/resultados/pronostico_prc_rds_por_pixel/pronostico_poly_mensual.rds")

# export los datos de pronostico
i = ""
for (i in unique(dato_prn$id_xy)) {
  write_csv(x = dato_prn[dato_prn$id_xy == i, ], 
            file = paste("../lbo_doc/resultados/pronostico_prc_csv_por_pixel/", 
                         i, ".csv"), 
            append = TRUE) # append = F, no añade el nombre como nueva columna
}
write_csv()



# plot año mes del pronostico espacial
# coef_filtrado %>%
plot_prn <- 
  datos1 %>% 
  filter(date >= "2005-01-01") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = pred_poly)) +
  facet_grid(year~month, labeller = labeller(month = c(`1` = "Ene", `2` = "Feb", `3` = "Mar", 
                                                       `4` = "Abr", `5` = "May", `6` = "Jun", 
                                                       `7` = "Jul", `8` = "Ago", `9` = "Sep", 
                                                       `10`= "Oct", `11`= "Nov", `12`= "Dic"))
  ) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "GnBu")) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = 'Longitud', y = 'Latitud', fill = "Prc (mm)")

# guardar el plot 
ggsave(filename = "../lbo_doc/resultados/figuras/plot_mes_año_pronst_prc.png", 
       plot = plot_prn, device = "png", width = 14, height = 16, units = "cm")

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
