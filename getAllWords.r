
getSingleSensor <- function(sensor){
  start <- (sensor-1) * 204 + 1
  end <- sensor * 204
  pz <- datasetXCY[,start:end]
}

#elettrodi <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "P7","P8")


getFewSensors <- function(){
    FZ <- getSingleSensor(1)
    CZ <- getSingleSensor(2)
    PZ <- getSingleSensor(3)
    OZ <- getSingleSensor(4)
    P3 <- getSingleSensor(5)
    P4 <- getSingleSensor(6)
    P7 <- getSingleSensor(7)
    P8 <- getSingleSensor(8)
  
    
    #fewSensors <- cbind(CZ, PZ,P3, P4, P7, P8, datasetXCY[, 1633:1634])
    fewSensors <- cbind(CZ, PZ, P3, P4, OZ, FZ, datasetXCY[, 1633:1634])
    return(fewSensors)
}

getAllWords <- function(number_of_characters){
  load_file_with_names()

  dataset_sensorsP <- getFewSensors() 
  train <- dataset_sensorsP[1:3000, - which(colnames(dataset_sensorsP) == "cData")]
  #test  <- dataset_sensorsP[3001:3600, - which(colnames(dataset_sensorsP) == "cData")]
  test  <- dataset_sensorsP[3001:3600,]
  
  #default dataset
  #train <- datasetXCY[1:3000, - which(colnames(datasetXCY) == "cData")]
  #test  <- datasetXCY[3001:3600, - which(colnames(datasetXCY) == "cData")]
  
  costs <- table(dataset_sensorsP$target)/3600  # the weight vector must be named with the classes names

  class <- train_svm(train, "polynomial", 3, costs)
  
  for(i in 1:5){
    letter <- get_character(10, class, i, test)
    print(letter)
  }
}

#idee scartate
idee_scartate <- function(){
  #feature selection
  trainNorm <- scale(train[,  - which(colnames(train) == "target")])
  trainNorm <- as.data.frame(trainNorm)
  target <- train[, which(colnames(train) == "target")]
  trainNorm <- cbind(trainNorm, target)
  
  train <- plotImportance(train, 0.9)
  test <- test[, -highlyCorrelated]
}
