# 단측 검정
bootlimit = function(stat, alpha=0.05, m=1000){
  set.seed(1000)
  stat = as.vector(stat)
  CL_mat = matrix(0, m, 1)
  
  for(i in 1:m){
    sample_temp = sample(stat, size=length(stat), replace=T, prob=NULL)
    CL = quantile(sample_temp, 1-alpha)
    CL_mat[i,] = CL
  }
  cl = mean(CL_mat)
  
  ret = cl
  return(ret)
}