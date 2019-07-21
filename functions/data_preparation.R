

applyMovingAverage <- function(dataset, trainOrTest){
  
  if(trainOrTest == 1){
    trainMA <- applyMovinAverage(dataset,12,"s")
    colnames(trainMA)[137] <- "target"
    return(trainMA)
  }
  else{
    testMA <- applyMovinAverage(dataset,12,"s")
    colnames(testMA)[137] <- "cData"
    testMA <- cbind(testMA,dataset$target)
    colnames(testMA)[138] <- "target"
    return(testMA)
  }
}

applyMovinAverage <- function(dataset,backwardFactor,type){
  
  newdataset <- data.frame(matrix(unlist(apply(dataset,1,movingAverageOnSensor,backwardFactor,type))
                                  ,nrow=nrow(dataset),byrow=T  ))
  
  return(newdataset)
  
}

movingAverageOnSensor <- function(rowOfDataset,backwardFactor,type){
  
  rowMovingAverage1 <- (movavg(as.numeric(rowOfDataset[1:204]) ,backwardFactor,type))
  rowMovingAverage2 <- (movavg(as.numeric(rowOfDataset[205:408]),backwardFactor,type))
  rowMovingAverage3 <- (movavg(as.numeric(rowOfDataset[409:612]),backwardFactor,type))
  rowMovingAverage4 <- (movavg(as.numeric(rowOfDataset[613:816]),backwardFactor,type))
  rowMovingAverage5 <- (movavg(as.numeric(rowOfDataset[817:1020]),backwardFactor,type))
  rowMovingAverage6 <- (movavg(as.numeric(rowOfDataset[1021:1224]),backwardFactor,type))
  rowMovingAverage7 <- (movavg(as.numeric(rowOfDataset[1225:1428]),backwardFactor,type))
  rowMovingAverage8 <- (movavg(as.numeric(rowOfDataset[1429:1632]),backwardFactor,type))
  
  rowMovingAverage1 <- transpose(as.data.frame(decimate(rowMovingAverage1,backwardFactor)))
  rowMovingAverage2 <- transpose(as.data.frame(decimate(rowMovingAverage2,backwardFactor)))
  rowMovingAverage3 <- transpose(as.data.frame(decimate(rowMovingAverage3,backwardFactor)))
  rowMovingAverage4 <- transpose(as.data.frame(decimate(rowMovingAverage4,backwardFactor)))
  rowMovingAverage5 <- transpose(as.data.frame(decimate(rowMovingAverage5,backwardFactor)))
  rowMovingAverage6 <- transpose(as.data.frame(decimate(rowMovingAverage6,backwardFactor)))
  rowMovingAverage7 <- transpose(as.data.frame(decimate(rowMovingAverage7,backwardFactor)))
  rowMovingAverage8 <- transpose(as.data.frame(decimate(rowMovingAverage8,backwardFactor)))
  
  new_set_row <- cbind(rowMovingAverage1,rowMovingAverage2,rowMovingAverage3,rowMovingAverage4,rowMovingAverage5,
                       rowMovingAverage6,rowMovingAverage7,rowMovingAverage8,rowOfDataset[1633])
  
  return(new_set_row)
}