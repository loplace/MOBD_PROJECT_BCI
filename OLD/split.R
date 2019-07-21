install.packages('caTools') 
library(caTools) 

install.packages('e1071') 
library(e1071) 

simone_svm <- function(out){
  set.seed(12345) 
  
  split = sample.split(out[,10], SplitRatio = 0.7) 
  
  training_set = subset(out, split == TRUE) 
  test_set = subset(out, split == FALSE) 
  
  head(training_set)
  head(test_set)
  
  classifier = svm(formula = target ~ ., 
                   data = training_set, 
                   type = 'C-classification', 
                   kernel = 'linear') 
  
  y_pred = predict(classifier, newdata = test_set, decision.values = TRUE, probability = TRUE)
  y_pred
  
  # Making the Confusion Matrix 
  cm = table(test_set[, 10], y_pred) 
  cm
}
