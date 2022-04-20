# WMSE based Hatmatrix (solve)
Weighted_Hatmatrix_solve = function(x, y, weight){
  library(MASS)
  x = as.matrix(x)
  y = as.matrix(y)
  
  w = as.matrix(weight)
  beta0 = matrix(1, nrow(x), 1)
  x = cbind(beta0, x)  
  
  # WMSE based Hatmatrix
  w_beta = solve(t(x)%*%w%*%x) %*% t(x)%*%w %*% y

  ret = w_beta
  return(w_beta)  
}

# WMSE based Hatmatrix (ginv)
# Weighted_Hatmatrix_ginv = function(x, y, weight){
#   library(MASS)
#   x = as.matrix(x)
#   y = as.matrix(y)
#   
#   w = as.matrix(weight)
#   beta0 = matrix(1, nrow(x), 1)
#   x = cbind(beta0, x)  
#   
#   # WMSE based Hatmatrix
#   w_beta = ginv(t(x)%*%w%*%x, tol=sqrt(.Machine$double.eps)^10) %*% t(x)%*%w %*% y
#   
#   ret = w_beta
#   return(w_beta)  
# }


# test_x = 1:nrow(AE_ts_deg$degradation)
# test_x = cbind(test_x, test_x^2)
# 
# a=W_Hatmatrix_ginv(test_x[1:800,], test_y[1:800], w[1:800, 1:800])
# a
# fx <- function(x){
#   a[3]*x^2 + a[2]*x + a[1]
# }
# 
# curve(fx, 1, 1200, add=T, col='skyblue', lwd=2)

