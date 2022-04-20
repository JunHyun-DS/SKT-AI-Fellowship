# Information of RUL
RULmodel <-  function(train_x, train_y, test_x, test_y, time, weight, mean_life, failure_criteria, alpha=0.05, model=model, info=T){
  Comment = "
    @Description
      Degradation based Remaining Useful Life Prediction method
      predictive model
      ● Exponentially Weighted Linear Regression
      ● Exponentially Weighted Polynomial Regression
      ● Exponentially Weighted Exponential Regression

    @Param
      trainX : In-control Train data (time)
      trainY : In-control Train data (degradation)
      testX : Test data (time) 
      testY : Test data (degradation) 
      
      time : current time
      weight : The alpha of exponential smoothing weight
      mean_life : mean lifespan    
      model : 
        ● WLR : Exponentially Weighted Linear Regression
        ● WPR : Exponentially Weighted Polynomial Regression
        ● WER : Exponentially Weighted Exponential Regression
      FT : Failure Threshold
      info : information of the RUL

      @Return
        train_x
        train_y
        test_x
        test_y
        current_time : current time
        predict_time : predicted failure time
        RUL : Remaining Useful Lifecycle
        weight : weight of regression method
        mean_life = mean_life
        failure_criteria = failure threshold,
        alpha = exponential smoothing weight
        model = case of model (WLR, WPR, WER)
        model_param : coefficient of regression method
  "
  train_x = as.matrix(train_x)
  train_y = as.matrix(train_y)
  test_x = as.matrix(test_x)
  test_y = as.matrix(test_y)
  t_hat = 0
  model_param = list()
  CL = bootlimit(train_y, alpha) # Control Limit using bootstrap
  
  # curve model
  if(model=='WLR'){ # Weighted Linear Regression
    WLR = WLR_model(test_x, test_y, time, weight, failure_criteria)
    t_hat = WLR$t_hat
    model_param = WLR
  }else if(model=='WPR'){
    train_x = cbind(train_x, train_x^2) # 다항회귀분석(x변수 제곱형태 자동화)
    test_x = cbind(test_x, test_x^2)
    WPR = WPR_model(test_x, test_y, time, weight, failure_criteria)
    t_hat = WPR$t_hat
    model_param = WPR
  }else if(model=='WER'){
    WER = WER_model(test_x, test_y, time, weight, failure_criteria)
    t_hat = WER$t_hat
    model_param = WER
  }
  predict_time = t_hat
  rul = predict_time - time
  
  if(info==T){
    RUL_information_text(time, round(predict_time, 3), round(rul, 3)) # text code
  }else{}
    ret = list(
    train_x = train_x,
    train_y = train_y,
    test_x = test_x,
    test_y = test_y,
    model_param = model_param,
    CL = CL,
    mean_life = mean_life,
    failure_criteria = failure_criteria,
    current_time = time,
    predict_time = predict_time,
    rul = rul,
    model = model
  )
  return(ret)
}
  
# Visualization of RUL
RULplot <-  function(test_x, test_y, CL, failure_criteria, time, model_param, model, rul, plot_type=TRUE, ...){
  if(plot_type == TRUE){
    plot(test_x[1:time,1], test_y[1:time], xlab='Time', ylab='Degradation',..., type='n', xlim=c(0,1100))
    grid()
    points(test_x[1:time,1], test_y[1:time], xlab='Time', ylab='Degradation', pch=16)
    abline(h=CL, col='blue', lwd=2) # Anomaly line: alpha
    abline(h=failure_criteria, col='red', lwd=2) # faulure criteria
    abline(v=time, lwd=2, col=adjustcolor(2, alpha=0.5), lty=2)
    #text(1000, 0.02, paste('현재시간: ', time), adj=0, cex=.9)
    #text(1000, 0.017, paste('잔여수명: ', round(rul,2)),adj=0, cex=.9)
    if (test_y[time]>CL){
      if(model == 'WLR'){
        abline(a=model_param$intercept, b=model_param$slope, col=adjustcolor(11, alpha=0.7), lwd=3)
      }
      else if(model == 'WPR'){
        fx <- function(x){model_param$slope2*x^2 + model_param$slope1*x + model_param$intercept}
        curve(fx, 1, nrow(test_x)+1000, lwd=3, col=adjustcolor(11, alpha=0.7), add=T)
      }else if(model=='WER'){
      fx <- function(x){model_param$a * exp(model_param$b*x)}
      curve(fx, 1, nrow(test_x)+1000, lwd=3, col=adjustcolor(11, alpha=0.7), add=T)
      }else{}
    }
  }else{}
}

# RUL System (Information + Visualization)
RULsystem <- function(train_x, train_y, test_x, test_y, time=c(701,984), weight, mean_life, failure_criteria, alpha=0.05, model='WLR', info=T, plot_type = TRUE, ...){
  
  train_x = as.matrix(train_x)
  train_y = as.matrix(train_y)
  test_x = as.matrix(test_x)
  test_y = as.matrix(test_y)
  
  # RUL per time
  rul_mat = matrix(0, time[2]-time[1]+1, 1)
  row.names(rul_mat) = time[1]:time[2]
  colnames(rul_mat) = 'RUL'
  
  for(t in time[1]:time[2]){
    rul = RULmodel(train_x, train_y, test_x, test_y[1:t,], time=t, weight=weight[1:t,1:t], mean_life=mean_life, failure_criteria = failure_criteria,alpha=alpha, model=model, info=info)
    RULplot(rul$test_x, rul$test_y, rul$CL, rul$failure_criteria, rul$current_time, rul$model_param, rul$model, rul$rul, plot_type=plot_type, ...)
    rul_mat[t-min(time)+1,1] = rul$rul
  }
  ret = list(
    train_x = rul$train_x,
    train_y = rul$train_y,
    test_x = rul$test_x,
    test_y = rul$test_y,
    current_time = rul$current_time,
    predict_time = rul$predict_time,
    RUL = rul_mat,
    weight = weight,
    mean_life = mean_life,
    failure_criteria = failure_criteria,
    alpha = alpha,
    model = model,
    model_param = rul$model_param
  )
  return(ret)
}
