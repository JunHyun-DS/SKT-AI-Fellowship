mset_regress <- function(train, test){
  Comment = 
    "
    @Description
      MSET (Multivariate State Estimation Technique) Regression
      MSET Regression is Anomaly Detection method 
      The description of MSET : https://www.ne.anl.gov/codes/mset/
    @Param
      train : train data (In-control data) 
      test : test data
    @Return
      residual_tr : residual of train data
      residual_ts : residual of test data
  "
  
  train = as.matrix(train)
  test = as.matrix(test)
  
  train_intercept = matrix(1, nrow(train), 1)
  test_intercept = matrix(1, nrow(test), 1)
  y_hat_tr = matrix(0, nrow(train), ncol(train))
  
  for(i in 1:ncol(train)){
    y_hat_tr[,i] = cbind(train_intercept, as.matrix(train[,-i])) %*% solve(t(cbind(train_intercept, as.matrix(train[,-i]))) %*% cbind(train_intercept, as.matrix(train[,-i]))) %*% t(cbind(train_intercept, as.matrix(train[,-i]))) %*% as.matrix(train[,i])
  }
  
  y_hat_ts = matrix(0, nrow(test), ncol(test))
  
  for(i in 1:ncol(test)){
    y_hat_ts[,i] = cbind(test_intercept, as.matrix(test[,-i])) %*% solve(t(cbind(train_intercept, as.matrix(train[,-i]))) %*% cbind(train_intercept, as.matrix(train[,-i])))%*% t(cbind(train_intercept, as.matrix(train[,-i]))) %*% as.matrix(train[,i])
  }
  
  # residual mat
  residual_tr_mat = train - y_hat_tr
  residual_ts_mat = test-y_hat_ts
  
  ret = list(residual_tr = residual_tr_mat, residual_ts = residual_ts_mat)
  
  return(ret)
}
