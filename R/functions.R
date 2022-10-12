#' @title Helper functions to processing data
#' @description This script contains a set of functions required to reproduce 
#' the modeling and performance analysis.
#' @author Luis Balcazar & Gabriel Gaona

# Model traing function ----
#' @param seed seed value to ensure reproducibility and testing
#' @param id Unique indentifier for training outputs 
#' @param data Training data.frame partition. It must contain the columns x, y, 
#'  cdr and sst.
#' @param lag Lag value between CDR and covariates
#' @param formulas A named list of model formulas to train
#' @param train_partition Percent threshold to create train and test partitions
#' @param out_files named vector with output file paths for "coefs" (coefficients),
#' "perfs" (performance results) and "preds" (forecast outputs for test partition)
#' @param append Logical. Will the output be added to existing output files?
#' @param ... aditional parameters passed to \code{nls()} function
train_models <- function(seed = 1, 
                         id, 
                         data = datos,
                         lag = 11, 
                         formulas = list(lm = cdr ~ sst, 
                                         poly = cdr ~ poly(sst, 2, raw = TRUE), 
                                         step = cdr ~  sst + nino12, 
                                         nls  = cdr ~ a * exp(sst * b)),
                         train_partition = 0.7, 
                         out_files = c(coefs = "model_coef.csv", 
                                       perfs = "model_performance.csv", 
                                       preds = "model_forecast.csv"),
                         append = TRUE,
                         ...) {
  message(paste0("Training models for id = ", id, " and seed = ", seed))
  # get variable to modify with lag
  variables <- map(formulas,~rlang::f_rhs(.x) %>% all.vars()) %>% 
    reduce(c) %>% 
    unique() %>%
    as.character()
  
  # Apply lag to each explaining variables
  data <- data %>% 
    mutate_at(.vars = vars(any_of(variables)), 
              .funs = ~dplyr::lag(., n = lag))
  
  # get trainig data partition
  set.seed(seed)
  train <- train_data(x = data, col = rlang::f_lhs(formulas[[1]]), p = train_partition)
  
  # get testing data partition
  test <- anti_join(data, train, by = colnames(train))
  
  # Omiting na rows in traing and testing data 
  train <- na.omit(train)
  test <- na.omit(test)
  
  # Linear models fitting
  fits <- map(formulas[c("lm", "poly")], ~lm(.x, data = train))
  
  # Step-wise regression fitting
  stp <- lm(formulas[["step"]], data = train)
  fits[["step"]] <- step(stp, direction = "backward", trace = 0)
  
  # Non-linear regression fitting
  fits[["nls"]] <- nls_personal(formula = formulas[["nls"]],
                                data = train, ...)
  
  # Get model coefficients
  coefs <- map_dfr(set_names(fits, names(formulas)), get_coef, .id = "model") %>%
    mutate(id = id, seed = seed)
  
  # Get testing predictions
  preds_test <-  map_dfc(fits, predict2, newdata = test) %>%  # predicciones para 'test', 'data'
    mutate(cdr = test$cdr, id = id)
  
  # Model performance measures
  NSE <- map2_dbl(.x = test[, map_chr(formulas, ~rlang::f_lhs(.x) %>% as.character())],
                  .y = preds_test[, seq_along(formulas)],
                  .f = nse)
  
  R2 <- map2_dbl(.x = preds_test[, seq_along(formulas)],
                 .y = test[, map_chr(formulas, ~rlang::f_lhs(.x) %>% as.character())],
                 .f = cor)
  
  perfs <- map_df(set_names(fits, names(formulas)), get_perf, .id = "model") %>%
    mutate(NSE = NSE,
           R2_test = R2 ^ 2,
           id = id, seed = seed)
  
  # Listing output data
  outs <- list(coefs = coefs, perfs = perfs, preds = preds_test)
  for(i in names(out_files)) {
    if(!file.exists(out_files[i])){
      write_csv(outs[[i]], out_files[i], col_names = TRUE, append = !append)
      message(paste0("file ", out_files[i], " created"))
    } else {
      write_csv(outs[[i]], out_files[i], col_names = !append, append = append)
    }
  }
  perfs
}


# Partición de los datos para entrenamiento ----
#' @param x data.frame. A data frame that contains the full data for one station.
#' @param col Character. The colname of variable to use as control for NAs
#' @param p Numeric. A value of percent thresold to select a train partition from the data.frame
#' @param ... Other parameters for the function \code{caret::createDataPartition()}
train_data <- function(x, col = "cdr", p = 0.7, ...) {
  cp <- caret::createDataPartition(y = stats::na.omit(x[[col]]), p = p, list = FALSE, ...)
  x[cp[,1],]
}


# Get model coefficients as tidied tibble ----
#' @param ... parameters from \code{broom::tidy()}
get_coef <- possibly(broom::tidy, 
                     otherwise = tibble::tibble(term = NA)
)



# Get model performance measures ----
#' @param ... parameters from \code{broom::glance()}
get_perf <- possibly(broom::glance,
                     otherwise = tibble::tibble(r.squared = NA))



# Training NLS function varying staring values ----
#' @param formula A formula for a NLS model
#' @param data Data frame to fit model
#' @param start data.frame of starting values for all model coefficients 
#' @param ... aditional parameters passed to \code{nls()} function

nls_personal <- function(formula, 
                         data, 
                         start = expand_grid(a = seq(-.8, 2.5, .1) ,
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


# Calculate thresholds based on a probability distribution
#' @param x Numeric a vector of precipitation data
#' @param prob probabilities break points of CDF
#' @param distr A name of a distribution function
band_limits <- function(x, 
                        prob = c(0.25, 0.5, 0.75), 
                        distr = "gamma"){
 method = c("mle", "mme", "qme", "mge", "mse")
 param = "error"
 i = 1
 while("error" %in% param){
  if(i > length(method)) stop("No function fits the data")
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


# Nash–Sutcliffe model efficiency coefficient ----
#' @param cal Numeric vector of predicted values
#' @param obs Numeric vector of observed values 
nse <- purrr::possibly(
  .f = function(obs, cal) {
    if(length(cal) == sum(is.na(cal)) | length(obs) == sum(is.na(obs)))
      stop("NSE no calculado!")
    1 - (sum((cal - obs)^2, na.rm = TRUE) /
           sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE))
  },
  otherwise = NA_real_
)


# Mean average error ----
#' @param cal Numeric vector of predicted values
#' @param obs Numeric vector of observed values 
mae <-function(obs, cal) mean(abs(cal - obs), na.rm = T)

# Percent bias ----
#' @param cal Numeric vector of predicted values
#' @param obs Numeric vector of observed values 
pBias <- function(obs, cal){
 num = sum((cal - obs), na.rm = T)
 den = sum(obs, na.rm= T)
 # den = sum(ifelse(is.na(obs), NA, obs), na.rm= T)
 bias = round((num/den)*100,1)
 return(bias) 
}

# Root mean square error----
#' @param cal Numeric vector of predicted values
#' @param obs Numeric vector of observed values 
rmse <- function(obs, cal){
 dif <- sqrt(mean((obs - cal)^2, na.rm = T))
 return(dif)
}


# Coefficient of determination (R^2)---
#' @param cal Numeric vector of predicted values
#' @param obs Numeric vector of observed values 
r2 <- function(obs, cal){
 cor = lm(cal ~ obs) 
 summ <- summary(cor)$r.squared
 return(summ)
}

# Pearson coefficient ----
#' @param cal Numeric vector of predicted values
#' @param obs Numeric vector of observed values 
r <- function(obs, cal){
 cor(obs, 
     cal, 
     method = 'pearson', 
     use = 'na.or.complete')
}

# Ndatos 
#' @param cal Numeric vector of predicted values
#' @param obs Numeric vector of observed values 
nDatos <- function(obs, cal){
  sum(!is.na(obs) & !is.na(cal))
}

# Relative error
#' @param cal Numeric vector of predicted values
#' @param obs Numeric vector of observed values 
RE <- function(obs, cal){
 num = abs(obs - cal)
 er = (num/cal) * 100
}

# Safe predict function ----
#' @param ... values passed to predict() function
predict2 <- purrr::possibly(predict, otherwise = NA_real_)

# Calculate the best k for kmeans ----
#' @param x Matrix or Data frame. Variables for Clustering
#' @param kmax Integer. Value for K iterations
best_k <- function(x, kmax = 10){
  if(inherits(x, "data.frame"))
    x <- as.matrix(x)
  dist <- stats::dist(x)
  sil_width = NULL
  for (i in 2:kmax){
    clust <- kmeans(x, i)
    ss <- cluster::silhouette(clust$cluster, dist)
    sil_width[i - 1] <- mean(ss[, 3])  
  }
  return(which.max(sil_width) + 1)
}
