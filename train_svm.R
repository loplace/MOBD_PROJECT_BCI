library(e1071)

train_svm <- function(dataset_train,kernelToApply,degreeToApply,weightsVector){
  
  # divido train e test in modo che l'ultima parola rappresenti il test set
  #train <- datasetXCY[1:3000, -1633]
  #test  <- datasetXCY[3001:3600, -1633]
  
  # scriviamo una semplice svm per testare i risultati di base

  classifier = svm(formula = target ~ .,
                   data = dataset_train,
                   type = 'C-classification',
                   kernel = kernelToApply,
                   degree = degreeToApply,
                   class.weights = weightsVector
                   )
  return(classifier)
}