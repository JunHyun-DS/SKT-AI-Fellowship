exponential_weight = function(x, y, alpha=0.05){
  Comment = "
  @Description
    Exponential Smoothing weight

  @Param
    x : x
    y : y
    alpha : Degree of exponential weight

  @Return
    weight
  "
  x = as.matrix(x)
  y = as.matrix(y)
  weight_vec = matrix(0, nrow(y), 1)
  weight_vec[1,1] = alpha
  for(i in 2:nrow(y)){
    weight = alpha*(1-alpha)^i
    weight_vec[i,1] = weight
  }
  weight = as.matrix(sort(weight_vec, decreasing=F))
  
  weight_mat = matrix(0, nrow(weight_vec), nrow(weight_vec))
  diag(weight_mat) = weight
  
  ret = weight_mat
  return(ret)
}

# lag=1 만 가능
variation_weight = function(x, y, lag=1){
  library(dplyr)
  x = as.matrix(x)
  y = as.matrix(y)
  weight = as.matrix(abs(y-lag(y,lag)))
  weight[1] = min(weight, na.rm=T)
  weight_mat = matrix(0, nrow(weight), nrow(weight))
  diag(weight_mat) = weight
  
  ret = weight_mat
  return(ret)
}

time_variation_weight = function(x, y, lag=1, alpha=0.05){
  x = as.matrix(x)
  y = as.matrix(y)
  weight_var = diag(variation_weight(x, y, lag=1))
  weight_time = diag(exponential_weight(x, y, alpha=alpha))
  weight_mat = matrix(0, nrow(y), nrow(y))
  diag(weight_mat)  = weight_var * weight_time
  
  ret = weight_mat
  return(ret)
}
