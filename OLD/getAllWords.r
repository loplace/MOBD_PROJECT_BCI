
getSingleSensor <- function(sensor){
  start <- (sensor-1) * 204 + 1
  end <- sensor * 204
  pz <- datasetXCY[,start:end]
}

#elettrodi <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "P7","P8")

getTestWord <- function(dataset, word){
  
  # ALLENO IL MODELLO SU TUTTO IL DATASET
  if(word == 0){
    train <- dataset
  }
  # ESEGUO UNO SPLIT IN TRAIN E TEST
  else {
    start = (word-1)*600 + 1 
    end   = start + 599
    train <- dataset[- (start:end), - which(colnames(dataset) == "cData")]
    test  <- dataset[start:end,]
  }
  
  return(list('train' = train, 'test' = test))
}

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
  
  #Test is 2BACI --1
  #Test is 5ROSE --2
  #Test is ZUPPA --3
  #Test is GATTO --4
  #Test is MENTE --5
  #Test is VIOLA --6
  ds_split <- getTestWord(6)
  train <- ds_split$train
  test <- ds_split$test


  #apply moving average
  trainMA <- applyMovinAverage(train,12,"s")
  colnames(trainMA)[137] <- "target"
  
  testMA <- applyMovinAverage(test,12,"s")
  colnames(testMA)[137] <- "cData"
  testMA <- cbind(testMA,test$target)
  colnames(testMA)[138] <- "target"

  
  #usage: dataset_train,kernelToApply,gammaToApply,costToApply,weightsVector)
  classGauss <- train_gaussian_svm(trainMA,"radial",10^(-4),8,NULL)

  
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
  
  reduced_train <- reduce_non_target_lines_in_train(train,4.5)
  
  # using the previous dataset...
  costs <- table(train$target)  # the weight vector must be named with the classes names
  costs[1] <- 1    # a class -1 mismatch has a terrible cost
  costs[2] <- 1    # a class +1 mismatch not so much...
  costs <- table(dataset_sensorsP$target)/3600  # the weight vector must be named with the classes names
  
  
  #usage: dataset_train,kernelToApply,gammaToApply,degreeToApply,weightsVector)
  classPoly <- train_polynomial_svm(trainMA, "polynomial", 3,10,NULL)
  
  
  classLinear <- train_linear_svm(train,"linear",10,NULL)
  
}
