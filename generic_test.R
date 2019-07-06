
portion_out <- function(sensor){

  start = (sensor-1) * 8 + 1
  end = sensor * 8
  
  out <- datasetXCY[, c(start:end,1633,1634)]
  scaled <- scale(out[, 1:8])
  scaled <- cbind(scaled, row_col = out$cData)
  scaled
  out <- cbind(scaled, target = out$target)
  remove(scaled)
  
  matplot(t(out[,1:8]), type = "l", col = ifelse(out[, 10] == 1, 'red','green'))
  
  ds_mean <- rowMeans(out[,1:8], na.rm = FALSE, dims = 1)
  out <- cbind(out, vmean = ds_mean)
}


