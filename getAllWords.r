
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
  
  fewSensors <- cbind(FZ,CZ,PZ,OZ,P3,P4,P7,P8, datasetXCY[, 1633:1634])
  #fewSensors <- cbind(PZ,P3,P4,P7,P8, datasetXCY[, 1633:1634])
  
  return(fewSensors)
}

getAllWords <- function(number_of_characters){
  load_file_with_names()

  dataset_sensorsP <- getFewSensors() 
  
  #Test is 2BACI
  train <- dataset_sensorsP[- (1:600), - which(colnames(dataset_sensorsP) == "cData")]
  test  <- dataset_sensorsP[1:600,]
  
  #Test is 5ROSE
  train <- dataset_sensorsP[-(601:1200), - which(colnames(dataset_sensorsP) == "cData")]
  test  <- dataset_sensorsP[601:1200,]
  
  #Test is ZUPPA
  train <- dataset_sensorsP[-(1201:1800), - which(colnames(dataset_sensorsP) == "cData")]
  test  <- dataset_sensorsP[1201:1800,]
  
  #Test is GATTO
  train <- dataset_sensorsP[-(1801:2400), - which(colnames(dataset_sensorsP) == "cData")]
  test  <- dataset_sensorsP[1801:2400,]
  
  #Test is MENTE
  train <- dataset_sensorsP[-(2401:3000), - which(colnames(dataset_sensorsP) == "cData")]
  test  <- dataset_sensorsP[2401:3000,]
  
  #Test is VIOLE
  train <- dataset_sensorsP[-(3001:3600), - which(colnames(dataset_sensorsP) == "cData")]
  test  <- dataset_sensorsP[3001:3600,]
  

  #apply moving average
  trainMA <- applyMovinAverage(train,12,"s")
  colnames(trainMA)[137] <- "target"
  
  testMA <- applyMovinAverage(test,12,"s")
  colnames(testMA)[137] <- "cData"
  testMA <- cbind(testMA,test$target)
  colnames(testMA)[138] <- "target"
  
  
  #reduced_train <- reduce_non_target_lines_in_train(train,4)
  
  costs <- table(dataset_sensorsP$target)/3600  # the weight vector must be named with the classes names

  #usage: dataset_train,kernelToApply,gammaToApply,degreeToApply,weightsVector)
  classPoly <- train_polynomial_svm(train, "polynomial", 3,10,NULL)
  
  #usage: dataset_train,kernelToApply,gammaToApply,costToApply,weightsVector)
  classGauss <- train_gaussian_svm(trainMA,"radial",10^(-4),8,NULL)
  
  classLinear <- train_linear_svm(train,"linear",10,NULL)
  
#  for(i in 1:5){
#    letter <- get_character(10, class, i, test)
#    print(letter)
#  }
  
  for(i in 1:5){
    letter <- get_character_by_argmax(10, classGauss, i, testMA)
   # print(letter)
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
