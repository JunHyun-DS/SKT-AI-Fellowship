degradation_model = function(residual){
  residual = as.matrix(residual)
  # residual euclidean norm
  residual_ed_norm = matrix(0, nrow(residual), ncol(residual))
  for(i in 1:ncol(residual)){
    residual_ed_norm[,i] =  sqrt(residual[,i]^2)
  }
  # accumulated degradation model
  accum_degradation = matrix(0, nrow(residual), ncol(residual))
  for(i in 1:ncol(residual)){
    accum_degradation[,i] =  cumsum(residual_ed_norm[,i])
  }
  
  # degradation model
  n = 1:nrow(residual)
  degradation = matrix(0, nrow(residual), ncol(residual))
  for(i in 1:ncol(residual)){
    degradation[,i] =  cumsum(residual_ed_norm[,i])/n
  }
  ret = list(
    accum_degradation = accum_degradation,
    degradation = degradation
  )
  return(ret)
}