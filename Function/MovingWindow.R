# window function
MovingWindow = function(data, size=1, type='mean'){
  Comment = 
    "
    @Description
      Moving window 
    @Param
      data : data
      size : window size
      type : type of statistical technique
    @Return
      maMat : moving window stats
      data : original data
  "
  data = as.matrix(data)
  maMat = matrix(0, nrow(data)-size+1, ncol(data))  
  if(size !=1){
    if(type=='mean'){
      for(col in 1:ncol(data)){
        stats = c()
        for(i in 0:(nrow(data)-size)){
          temp = mean(data[(1+i):(size+i),col])
          stats = append(stats, temp)
        }
        maMat[, col] = stats  
      }
    }
    ret = maMat
  }else{
    ret = data
  }
  return(ret)
}

# exmaple
# Moving_window(data, size=1, type='mean')
