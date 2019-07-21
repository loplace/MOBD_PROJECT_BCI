source("libraries/libraries.R")
source("functions/load_and_manipulate.R")
source("functions/train.R")
source("functions/work_on_test.R")
source("functions/data_preparation.R")

model_has_to_be_trained <- function(testWord = 0){
  dataset <- load_dataset()
  if(testWord != 0){
    ds_split <- getTestWord(dataset, testWord)
    train <- ds_split$train
    test <<- ds_split$test
  }
  else{
    train <- dataset
  }
  
  trainMA <- applyMovingAverage(train, 1)
  classGauss <- train_gaussian_svm(trainMA,"radial",10^(-4),8,NULL)
  saveModel(classGauss)
}

model_has_been_trained <- function(){
  returnWord(test)
}

for(i in 1:6){
  model_has_to_be_trained(i)
  model_has_been_trained()
}
