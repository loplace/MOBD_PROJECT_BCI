#Questi metodi sono atti all'applicazione della media mobile e di un fattore di decimazione ai dataset.

#La media mobile permette di estrapolare il trend di una serie storica, quindi l'idea è che dia risalto
#alle misurazioni relative al target "1", le quali dovrebbero esibire un trend probabilmente di crescita,
#a differenza delle misurazione relative al target "-1", che probabilmente mostrano un trend simili tra loro.

#Il fattore di decimazione serve a fare sottocampionamento (Downsampling) per ridurre il numero di variabili in gioco
#cercando mi mantenere al contempo il carico informativo delle rilevazioni originali per ogni sensore. 
#Infatti, nonostante le SVM siano meno soggette alla "curse of dimensionality", una svm lineare non è generalmente
#in grado di separare correttamente punti con un tale numero di attributi, e al contempo usare un kernel che alza troppo 
#la dimensionalità dello spazio delle features potrebbe portare più facilmente ad overfitting, quindi ridurre il numero
#di variabili per ogni sensore aumenta notevolemente le prestazioni. 


applyMovingAverage <- function(dataset, trainOrTest){
  
  
  if(trainOrTest == 1){
    trainMA <- applyMovinAverage(dataset,12,"s")
    colnames(trainMA)[137] <- "target" #rinominiamo l'ultima colonna perchè il nome viene sovrascritto durante la apply
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
  
  #utilizziamo la apply per poter effettuare le operazioni (MovAvg, Decimate) per ogni riga del data frame, poi ritrasformiamo
  #il risultato in un dataframe sfruttando data.frame(matrix(unlist(...)))
  newdataset <- data.frame(matrix(unlist(apply(dataset,1,movingAverageOnSensor,backwardFactor,type))
                                  ,nrow=nrow(dataset),byrow=T  ))
  
  return(newdataset)
  
}

movingAverageOnSensor <- function(rowOfDataset,backwardFactor,type){
  
  #applichiamo la media mobile in maniera separata per ogni sensore (ogni 204 colonne)
  rowMovingAverage1 <- (movavg(as.numeric(rowOfDataset[1:204]) ,backwardFactor,type))
  rowMovingAverage2 <- (movavg(as.numeric(rowOfDataset[205:408]),backwardFactor,type))
  rowMovingAverage3 <- (movavg(as.numeric(rowOfDataset[409:612]),backwardFactor,type))
  rowMovingAverage4 <- (movavg(as.numeric(rowOfDataset[613:816]),backwardFactor,type))
  rowMovingAverage5 <- (movavg(as.numeric(rowOfDataset[817:1020]),backwardFactor,type))
  rowMovingAverage6 <- (movavg(as.numeric(rowOfDataset[1021:1224]),backwardFactor,type))
  rowMovingAverage7 <- (movavg(as.numeric(rowOfDataset[1225:1428]),backwardFactor,type))
  rowMovingAverage8 <- (movavg(as.numeric(rowOfDataset[1429:1632]),backwardFactor,type))
  
  #applichiamo il fattore di decimazione, trasformiamo in un dataframe ogni porzione di riga e riaggreghiamo
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