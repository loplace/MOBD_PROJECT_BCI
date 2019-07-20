
library(mlbench)
library(caret)
library(randomForest)
library(gam)

plotImportance <- function(dataset, co){
  set.seed(7)
  # calculate correlation matrix
  correlationMatrix <- cor(dataset)
  # summarize the correlation matrix
  #print(correlationMatrix)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated <<- findCorrelation(correlationMatrix, cutoff=co)
  # print indexes of highly correlated attributes
  #print(highlyCorrelated)
  return (dataset[, -highlyCorrelated])
}

feature_selection_RF <- function(dataset){
  # ensure the results are repeatable
  set.seed(7)
  # define the control using a random forest selection function
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  # run the RFE algorithm
  results <- rfe(dataset[,1:1020], dataset[,1021], sizes=c(1:1020), rfeControl=control)
  results <- sbf(dataset[,1:1020], dataset[,1021], sbfControl = sbfControl())
  # summarize the results
  print(results)
  # list the chosen features
  predictors(results)
  # plot the results
  plot(results, type=c("g", "o"))
}