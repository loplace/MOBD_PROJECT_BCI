
# functione che effettua il train della SVM e ritorna il classificatore
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

# funzione che salva il classificatore nella directory 'model'
saveModel <- function(svmModel){
  save(svmModel, file = "./model/svmModel.Rdata")
}

# funzione che carica il classificatore dalla directory 'model'
loadModel <- function(){
  model <- load(file = "./model/svmModel.Rdata")
  return(get(model))
}