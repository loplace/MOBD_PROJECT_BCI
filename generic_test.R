
portion_out <- function(sensor){

  start = (sensor-1) * 8 + 1
  end = sensor * 8
  
  out <- datasetXCY[, c(start:end,1633,1634)]
  scaled <- scale(out[, 1:8])
  scaled <- cbind(scaled, row_col = out$cData)
  scaled
  out <- cbind(scaled, target = out$target)
  remove(scaled)
  
  outPositive <- out[out$cData == 1 | out$cData == 7,]
  outPositive <- outPositive[outPositive$target == 1,]
  outPositive <- outPositive[1:20,]
  
  outPositive <- out[1:12,]
  
  matplot(t(outPositive[,1:8]), type = "l", col = ifelse(outPositive[, 10] == 1, 'red','green'))
  
  
  matplot(t(out[,1:8]), type = "l", col = ifelse(out[, 10] == 1, 'red','green'))
  
  
  ds_mean <- rowMeans(out[,1:8], na.rm = FALSE, dims = 1)
  out <- cbind(out, vmean = ds_mean)
}


