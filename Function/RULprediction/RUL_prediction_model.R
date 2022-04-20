# weighted Linear Regression
WLR_model <- function(test_x, test_y, time, weight, failure_threshold){
  w_beta = Weighted_Hatmatrix_solve(test_x[1:time], test_y[1:time], weight[1:time, 1:time])
  
  # parameter of Weighted Linear Regression
  intercept = w_beta[1]
  slope = w_beta[2]
  
  # predict time
  t_hat = (failure_threshold-intercept) / slope  
  
  ret = list(
    intercept = intercept,
    slope = slope,
    t_hat = t_hat
  )
  return(ret)
}

# weighted Polynomial Regression
WPR_model <- function(test_x, test_y, time, weight, failure_threshold){
  test_x = as.matrix(test_x)
  test_y = as.matrix(test_y)
  w_beta = Weighted_Hatmatrix_ginv(test_x[1:time,], test_y[1:time,], weight[1:time, 1:time])
  
  # parameter of Weighted Linear Regression
  intercept = w_beta[1]
  slope1 = w_beta[2]
  slope2 = w_beta[3]
  
  # predict time
  t_hat = polyroot(c(intercept-failure_threshold, slope1, slope2))
  t_hat = max(as.numeric(t_hat))
  
  ret = list(
    intercept = intercept,
    slope1 = slope1,
    slope2 = slope2,
    t_hat = t_hat
  )
  return(ret)
}

# weighted Exponential Regression
WER_model <- function(test_x, test_y, time, weight, failure_threshold){
  for(i in 1:length(test_y)){
    if(log(test_y[i])==-Inf | log(test_y[i])==Inf){
      test_y[i] = mean(c(test_y[i-1], test_y[i+1]))
    }
  }
  test_y = log(test_y)
  w_beta = Weighted_Hatmatrix_solve(test_x[1:time], test_y[1:time], weight[1:time, 1:time])
  a = exp(w_beta[1])
  b = w_beta[2]
  t_hat = (log(failure_threshold)-log(a)) / b
  
  ret = list(
    a = a, # y = a*exp(b*x)
    b = b,
    t_hat = t_hat
  )
}