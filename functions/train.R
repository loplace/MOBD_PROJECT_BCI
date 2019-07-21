train_gaussian_svm <- function(dataset_train,kernelToApply,gammaToApply,costToApply,weightsVector){
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

saveModel <- function(svmModel){
  save(svmModel, file = "./model/svmModel.Rdata")
}

loadModel <- function(){
  model <- load(file = "./model/svmModel.Rdata")
  return(get(model))
}