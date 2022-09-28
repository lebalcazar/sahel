# funciones 

# Funciones:


# -------------------------------------------------------------------------

# Partición de los datos para entrenamiento
train_data <- function(x, col = "cdr", p = 0.7, ...) {
  cp <- caret::createDataPartition(y = stats::na.omit(x[[col]]), p = p, list = FALSE, ...)
  x[cp[,1],]
}


# -------------------------------------------------------------------------

# Obtener los coeficientes de los modelos
get_coef <- possibly(broom::tidy, 
                     otherwise = tibble::tibble(term = NA)
)


# -------------------------------------------------------------------------

#Obtener rendimientos de los modelos
get_perf <- possibly(broom::glance,
                     otherwise = tibble::tibble(r.squared = NA))

# -------------------------------------------------------------------------


# Función para entrenar el nls con distintos parametros de inicio 
nls_personal <- function(formula, 
                         data, 
                         start = expand_grid(a = seq(-.8, 2.5, .1) ,    # seq(.1, .9, .1)
                                             b = seq(-.8, 2.8, .1)) ,
                         ...){
  model <- NULL
  cnt <- 1
  while(is.null(model) & cnt <= nrow(start)){
    sv <- start[cnt,]
    model <- tryCatch(
      nls(formula = formula, data = data, start = as.list(sv), ...),
      error = function(e) NULL
    )
    cnt <- cnt + 1
  }
  if(is.null(model)) NA_character_ else model
}


# -------------------------------------------------------------------------

# Calcular Nash
nse <- purrr::possibly(
  .f = function(ym, y0) {
    if(length(ym) == sum(is.na(ym)) | length(y0) == sum(is.na(y0)))
      stop("NSE no calculado!")
    1 - (sum((ym - y0)^2, na.rm = TRUE) /
           sum((y0 - mean(y0, na.rm = TRUE))^2, na.rm = TRUE))
  },
  otherwise = NA_real_
)
  


predict2 <- possibly(predict, otherwise = NA_real_)

# Función para todos los lag deseados  (debe tener los nombres: x, y, date, cdr y sst)
train_models <- function(seed = 1, 
                         id, 
                         data = datos,
                         lag = 11, 
                         formulas = list(lm = cdr ~ sst, 
                                         poly = cdr ~ poly(sst, 2, raw = TRUE), 
                                         step = cdr ~  sst + nino12, #mslp + rhum + tni + nino34 + nino4 + oni,
                                         nls  = cdr ~ a * exp(sst * b)),
                         train_partition = 0.7, 
                         out_files = c(coefs = "model_coef.csv", 
                                       perfs = "model_performance.csv", 
                                       preds = "model_forecast.csv"),
                         append = TRUE,
                         ...) {
  message(paste0("Training models for id = ", id, " and seed = ", seed))
  # variables que se vana modificar
  variables <- map(formulas,~rlang::f_rhs(.x) %>% all.vars()) %>% 
    reduce(c) %>% 
    unique() %>%
    as.character()
  
  # modificar la tabla para aplicar el Lag a todas las variables independientes
  data <- data %>% 
    mutate_at(.vars = vars(any_of(variables)), 
              .funs = ~dplyr::lag(., n = lag))
  
  # Proporción de Datos para entrenamiento
  set.seed(seed)
  train <- train_data(x = data, col = rlang::f_lhs(formulas[[1]]), p = train_partition)
  
  # Proporción de Datos para validación
  test <- anti_join(data, train, by = colnames(train))
  
  train <- na.omit(train)
  test <- na.omit(test)
  # Ajuste de modelos lineales
  fits <- map(formulas[c("lm", "poly")], ~lm(.x, data = train))
  
  # ajuste step wise
  stp <- lm(formulas[["step"]], data = train)
  fits[["step"]] <- step(stp, direction = "backward", trace = 0)
  
  # ajuste nls exponencial
  fits[["nls"]] <- nls_personal(formula = formulas[["nls"]],
                          data = train, ...)
  
  # Extraer coeficientes de los modelos
  coefs <- map_dfr(set_names(fits, names(formulas)), get_coef, .id = "model") %>%
    mutate(id = id, seed = seed)
  
  # Obtener predicciones
  preds_test <-  map_dfc(fits, predict2, newdata = test) %>%  # predicciones para 'test', 'data'
    mutate(cdr = test$cdr, id = id)
  
  # Calcular el rendimiento de los modelos
  NSE <- map2_dbl(.x = preds_test[, seq_along(formulas)],
                  .y = test[, map_chr(formulas, ~rlang::f_lhs(.x) %>% as.character())],
                  .f = nse)
  
  R2 <- map2_dbl(.x = preds_test[, seq_along(formulas)],
                  .y = test[, map_chr(formulas, ~rlang::f_lhs(.x) %>% as.character())],
                  .f = cor)

  perfs <- map_df(set_names(fits, names(formulas)), get_perf, .id = "model") %>%
    mutate(NSE = NSE,
           R2_test = R2 ^ 2,
           id = id, seed = seed)

  #lista de salidas
  outs <- list(coefs = coefs, perfs = perfs, preds = preds_test)
  for(i in names(out_files)) {
    # print(i)
    if(!file.exists(out_files[i])){
      # si el fichero de salida no existe lo crea y escribe nombres de columnas y las primeras filas de modelos
      write_csv(outs[[i]], out_files[i], col_names = TRUE, append = !append)
      message(paste0("file ", out_files[i], " created"))
    } else {
      # si el fichero de salida existe solo agrega las nuevas filas sin nombres de columnas
      write_csv(outs[[i]], out_files[i], col_names = !append, append = append)
    }
  }
  perfs
}






