
getSingleSensor <- function(sensor){
  start <- (sensor-1) * 204 + 1
  end <- sensor * 204
  pz <- datasetXCY[,start:end]
}

#elettrodi <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "P7","P8")


getFewSensors <- function(){
  PZ <- getSingleSensor(3)
  P3 <- getSingleSensor(5)
  P4 <- getSingleSensor(6)
  P7 <- getSingleSensor(7)
  P8 <- getSingleSensor(8)
  
  fewSensors <- cbind(PZ, P3, P4, P7, P8, datasetXCY[, 1633:1634])
  return(fewSensors)

}

getAllWords <- function(number_of_characters){
  load_file_with_names()
  dataset_sensorsP <- getFewSensors() 
  train <- dataset_sensorsP[1:3000, - which(colnames(dataset_sensorsP) == "cData")]
  test  <- dataset_sensorsP[3001:3600, - which(colnames(dataset_sensorsP) == "cData")]
  
  merged_train <- merge_columns(dataset = train)
  train_full <- cbind(train,merged_train)
  
  reduced_train <- reduce_non_target_lines_in_train(train,4.5)
  
  # using the previous dataset...
  costs <- table(train$target)  # the weight vector must be named with the classes names
  costs[1] <- 1    # a class -1 mismatch has a terrible cost
  costs[2] <- 1    # a class +1 mismatch not so much...

  class <- train_svm(train, "polynomial", 3, costs)
  
  for(i in 1:5){
    letter <- get_character(10, class, i, test)
    print(letter)
  }
}
