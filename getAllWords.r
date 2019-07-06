
getAllWords <- function(number_of_characters){
  train <- datasetXCY[1:3000, -1633]
  class <- train_svm(train, "polynomial", 3)
  for(i in 1:number_of_characters){
    letter <- get_character(10, class, i)
    print(letter)
  }
}