
getRowOrdered <- function(train_dataset, finalRow = 1633){
  
  dToReturn <- train_dataset
  
  for(i in 1:500){
    pOfM <- dToReturn[(6*i-5):(6*i),]
    pOfM <- pOfM[order(pOfM[, finalRow]), ]
    dToReturn[(6*i-5):(6*i),] <- pOfM
  }
  
  return(dToReturn)
  
}

operationBetweenMatrices <- function(A, B, additionalParameter = 10){
  result <- A + (1/additionalParameter)*B
  return(result)
}

getIterationMean <- function(orderedTrain, gap = 12, nOfChars = 25, iterations = 10, tCol = 1634){
  orderedTrain <- as.matrix(orderedTrain)
  numberOfRows <- gap*nOfChars
  mToRet <- matrix(0L, nrow = numberOfRows, ncol = tCol)
  
  for (i in 1:nOfChars){
    for (j in 1:iterations){
      A <- mToRet[(i*gap -(gap-1)):(i*gap), ]
      B <- orderedTrain[(iterations*gap*(i-1)+gap*(j-1)+1):(iterations*gap*(i-1)+gap*(j-1)+gap), ]
      test <- operationBetweenMatrices(A, B, iterations)
      mToRet[(i*gap -(gap-1)):(i*gap), ] <- test
    }
  }
  return (mToRet)
}

getTrainWithMeans <- function(){
  load_file_with_names()
  
  dataset_sensorsP <- getFewSensors()
  train <- dataset_sensorsP[1:3000, ]
  test  <- dataset_sensorsP[3001:3600, - which(colnames(dataset_sensorsP) == "cData")]
  
  trainOrdered <- getRowOrdered(train, 1022)
  
  column_names <- colnames(train)
  
  trainFinal <- getIterationMean(trainOrdered, tCol = 1022)
  trainFinal <- as.data.frame(trainFinal)
  colnames(trainFinal) <- column_names
  trainFinal$target<-as.factor(trainFinal$target)
  
  #TEST 1
  train <- rbind(train, trainFinal)
  
  costs <- table(train$target)  # the weight vector must be named with the classes names
  costs[1] <- 1       # a class -1 mismatch has a terrible cost
  costs[2] <- 1    # a class +1 mismatch not so much...
  class <- train_svm(train[, -1021], "polynomial", 3, costs)
  
  for(i in 1:5){
    letter <- get_character(10, class, i, test)
    print(letter)
  }
}

