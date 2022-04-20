# minmax scaler
minmaxScaler <- function(x, feature_range = c(0,1)){
  x = as.matrix(x)
  scaled = (x-min(x))/(max(x)-min(x))
  scaled = scaled*(feature_range[2]-feature_range[1])+feature_range[1]
  
  return(scaled)
}

Z_scaler <- function(x, mu=mean(x), sigma = sd(x)){4
  x = as.matrix(x)
  mean_mat = matrix(apply(x, 2, mean), nrow(x), ncol(x), byrow=T)
  sd_mat = matrix(apply(x, 2, sd), nrow(x), ncol(x), byrow=T)
  z <- function(x) {(x-mean_mat) / sd_mat}
  scaled  = z(x)
  
  return(scaled)
}

sigmoid_scaler <- function(x){
  x = as.matrix(x)
  sigmoid <- function(x){1/(1+exp(-x))}  
  scaled = apply(x, 2, sigmoid)
  
  return(scaled)
}