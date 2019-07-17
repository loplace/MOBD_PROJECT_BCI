library(pracma)

applyMovinAverage <- function(dataset,backwardFactor,type){
  
 #a <- by(dataset, 1:nrow(dataset), function(row,backwardFactor,type) movingAverageOnSensor)
  
  apply(dataset,1,movingAverageOnSensor,backwardFactor,type)
  return(dataset)
  
  
}

movingAverageOnSensor <- function(rowOfDataset,backwardFactor,type){
  
  rowMovingAverage1 <- as.data.frame(movavg(as.numeric(rowOfDataset[1:204]) ,backwardFactor,type))
  rowMovingAverage2 <- as.data.frame(movavg(as.numeric(rowOfDataset[205:408]),backwardFactor,type))
  rowMovingAverage3 <- as.data.frame(movavg(as.numeric(rowOfDataset[409:612]),backwardFactor,type))
  rowMovingAverage4 <- as.data.frame(movavg(as.numeric(rowOfDataset[613:816]),backwardFactor,type))
  rowMovingAverage5 <- as.data.frame(movavg(as.numeric(rowOfDataset[817:1020]),backwardFactor,type))
  rowMovingAverage6 <- as.data.frame(movavg(as.numeric(rowOfDataset[1021:1224]),backwardFactor,type))
  rowMovingAverage7 <- as.data.frame(movavg(as.numeric(rowOfDataset[1225:1428]),backwardFactor,type))
  rowMovingAverage8 <- as.data.frame(movavg(as.numeric(rowOfDataset[1429:1632]),backwardFactor,type))
  
  new_set_row <- cbind(rowMovingAverage1,rowMovingAverage2,rowMovingAverage3,rowMovingAverage4,rowMovingAverage5,
                       rowMovingAverage6,rowMovingAverage7,rowMovingAverage8,rowOfDataset[1633])
  
  return(new_set_row)
}