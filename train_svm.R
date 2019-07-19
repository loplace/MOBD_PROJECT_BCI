library(e1071)

train_polynomial_svm <- function(dataset_train,kernelToApply,degreeToApply,costToApply,weightsVector){
  
  # divido train e test in modo che l'ultima parola rappresenti il test set
  #train <- datasetXCY[1:3000, -1633]
  #test  <- datasetXCY[3001:3600, -1633]
  
  # scriviamo una semplice svm per testare i risultati di base

  classifier = svm(formula = target ~ .,
                   data = dataset_train,
                   type = 'C-classification',
                   kernel = kernelToApply,
                   degree = degreeToApply, #Needed for polynomial
                   cost = costToApply,
                   class.weights = weightsVector
                   )
  return(classifier)
}


train_gaussian_svm <- function(dataset_train,kernelToApply,gammaToApply,costToApply,weightsVector){
  
  # divido train e test in modo che l'ultima parola rappresenti il test set
  #train <- datasetXCY[1:3000, -1633]
  #test  <- datasetXCY[3001:3600, -1633]
  
  # scriviamo una semplice svm per testare i risultati di base
  
  classifier = svm(formula = target ~ .,
                   data = dataset_train,
                   type = 'C-classification',
                   kernel = kernelToApply,
                   gamma = gammaToApply,   
                   cost = costToApply,
                   class.weights = weightsVector
  )
  return(classifier)
}

train_linear_svm <- function(dataset_train,kernelToApply,costToApply,weightsVector){
  
  # divido train e test in modo che l'ultima parola rappresenti il test set
  #train <- datasetXCY[1:3000, -1633]
  #test  <- datasetXCY[3001:3600, -1633]
  
  # scriviamo una semplice svm per testare i risultati di base
  
  classifier = svm(formula = target ~ .,
                   data = dataset_train,
                   type = 'C-classification',
                   kernel = kernelToApply,
                   cost = costToApply,
                   class.weights = weightsVector
  )
  return(classifier)
}