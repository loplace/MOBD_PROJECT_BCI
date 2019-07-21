source("libraries/libraries.R")
source("functions/load_and_manipulate.R")
source("functions/train.R")
source("functions/work_on_test.R")
source("functions/data_preparation.R")

# far girare questa funzione, decommentando la chiamata alla fine di questo file .R, 
# solo se per qualche motivo nella cartella 'model' non è presente il modello.
#
# Questa funzione allena la SVM. 
# L'argomento in input indica quale parola escludere dal train. 
# Questa verrà automaticamente usata come test.
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

# Nel caso in cui il modello sia presente nella directory 'model' è possibile 
# runnare direttamente questa funzione. 
# IL DATASET DI TEST DEVE ESSERE PRESENTE NELL'ENVIRONMENT CON NOME 'test'
# in output viene stampata in console la parola prevista
model_has_been_trained <- function(){
  returnWord(test)
}