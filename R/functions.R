# Functions

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

# Mean average error
mae <-function(obs, cal) mean(abs(cal - obs), na.rm = T)

# PBias
pBias <- function(obs, cal){
 num = sum((cal - obs), na.rm = T)
 den = sum(ifelse(is.na(obs), NA, obs), na.rm= T)
 bias = round((num/den)*100,1)
 return(bias) 
}

# Root mean square error
rmse <- function(obs, cal){
 dif <- sqrt(mean((obs - cal)^2, na.rm = T))
 return(dif)
}

# Nash–Sutcliffe efficiency coefficient
nse <- function(obs, cal){
 Ns = sum((obs - cal)^2) /sum((obs - mean(obs))^2)
 1 - Ns
}

# Determination coefficient (R^2)
r2 <- function(obs, cal){
 cor = lm(cal ~ obs) 
 summ <- summary(cor)$r.squared
 return(summ)
}

r <- function(obs, cal){
 cor(obs, 
     cal, 
     method = 'pearson', 
     use = 'na.or.complete')
}

# Percent error
er <- function(obs, cal){
 e_abs <- abs(mean(obs) - mean(cal))/mean(obs) * 100
 return(e_abs)
}

# Ndatos 
nDatos <- function(obs, cal){
 n = ifelse(!is.na(obs) & !is.na(cal),1,0)
 sum(n)
}


# error_relativo
RE <- function(obs, cal){
 num = abs(obs - cal)
 er = (num/cal) * 100
}
