RUL_information_text <- function(current_time, predict_time, rul){
  cat('--------------------\n','information of RUL\n\n',
      'current time: ', current_time, '\n', 
      'predict time: ', predict_time, '\n',
      'RUL: ', rul, '\n', '--------------------','\n\n' 
      ,sep=''
  )  
}

Mixture_information_text <- function(posterior, rul, result, time, model){
  cat('\n',paste('- Information of Mixture Model -'),'\n\n', sep='')
  for(i in 1:length(model)){
    cat(paste('----------', model[i]  ,'------------'),'\n', sep='')
    cat('percentage: ', round(posterior[i]*100,3), '%','\n',sep='')
    cat('RUL: ', rul[i], '\n\n',sep='')
  } 
  cat(paste('---------- result ----------'),'\n', sep='')
  cat('current time: ', time, '\n',sep='')
  cat('RUL: ', round(result,3), '\n\n\n',sep='')
  
}

bayesian_iteration_text <- function(bayesian_time, posterior, model){
  cat('\n', paste('---------- time ', bayesian_time ,' ------------', sep=''),'\n', sep='')
  for(i in 1:length(model)){
    cat(model[i], ': ', round(posterior[i]*100,3), '%','\n',sep='')
  }
}