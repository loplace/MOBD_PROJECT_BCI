

get_row_col_indexes_by_argmax <- function(sub_iteration,classifier,testToPredict, nrowOfBlock = 6){
  
  start_point <- (sub_iteration-1)*nrowOfBlock + 1
  end_point <- start_point + (nrowOfBlock-1)
  
  c_of_pred <- testToPredict[start_point:end_point, which(colnames(testToPredict) == "cData")]
  testToPredictWithoutC <- testToPredict[, - which(colnames(testToPredict) == "cData")]
  
  y_pred = predict(classifier, newdata = testToPredictWithoutC[start_point:end_point, - which(colnames(testToPredictWithoutC) == "target") ], 
                   decision.values = TRUE)
  
  # prendiamo le colonne associate alle predizioni
  #c_of_pred <- datasetXCY[start_point:end_point, 1633] 
  
  # y_pred è un oggetto di tipo factor in R. Per convertirlo in un array di numeri bisogna usare 
  # la funzione as.numeric ricordando che i risultati che escono corrispondono alla posizione del 
  # vettore [-1 1]
  target_prediction <- as.numeric(y_pred)
  target_prediction[target_prediction == 1] = -1
  target_prediction[target_prediction == 2] = 1
  
  # uniamo le predizioni alle colonne e alle decisioni
  target_prediction <- cbind(c_of_pred, target_prediction, attr(y_pred, "decision.values"))
  colnames(target_prediction) = c("row_col", "prediction", "decision_value")
  
  #convertiamo la tabella in un data_frame, ora abbiamo la predizione delle 6 righe/colonne di 1 iterazione
  target_prediction_df<- as.data.frame(target_prediction) 
  
  return(target_prediction_df)
  
}




get_character_by_argmax <- function(num_iteration,classifier, letter, testToPredict){
  
  test_with_c <- testToPredict
  testToPredict <- testToPredict[, - which(colnames(testToPredict) == "cData")]
  
  start_index <- (letter-1)*num_iteration*2 + 1
  last_index <- start_index + 2*num_iteration -1
  
  #es. (1,3,5,7,9,11,13,15,17,19)
  #1 = primo blocco di righe , 3 = secondo blocco di righe, etc. fino al 10mo
  row_sub_indexes <- seq(start_index,last_index,by = 2)
  col_sub_indexes <- seq(start_index+1,last_index,by = 2)
  
  all_rows <- NULL
  all_columns <- NULL
  
  #get rows prediction for each iteration
  for (j in row_sub_indexes){
    block <- get_row_col_indexes_by_argmax(j,classifier,test_with_c, 6)
    all_rows <- rbind(all_rows,block)
  }

  for (j in col_sub_indexes){
    block <- get_row_col_indexes_by_argmax(j,classifier,test_with_c, 6)
    all_columns <- rbind(all_columns,block)
  }

  row_index<- NULL
  
  col_index <- NULL
  
  row_index <- returnIndex(1,6,all_rows, 1)
  
  col_index <- returnIndex(7,12,all_columns ,2)
  
  m <- getMatrix()
  character <- m[row_index,col_index]

  return (character)
}

returnIndex <- function(start, end, all_rows_and_cols, row_or_col, currentMax = -5000){
  for (k in start:end){
    r_or_c <- all_rows_and_cols[all_rows_and_cols$row_col == k, ]
    decision_values <- r_or_c$decision_value
    target <- r_or_c$prediction
    final_values <- as.vector(decision_values) * -1
    for ( i in 1:length(final_values)){
      final_values[i] <- (final_values[i])^2 *sign(final_values[i])
    }
    row_col_sum <- sum(final_values)
    #print(row_col_sum)
    
    if (row_col_sum > currentMax){
      currentMax <- row_col_sum
      if(row_or_col == 2){
        index <- k - 6
      }
      else{
        index <- k
      }
    }
  }
  #print("********************************")
  return(index)
}





#**************************************************************************************






returnWord <- function(test){
  testMA <-applyMovingAverage(test,2)
  svmModel <- loadModel()
  
  word = ''
  for(i in 1:5){
    letter <- get_character_by_argmax(10, svmModel, i, testMA)
    word <- paste(word, letter)
  }
  print(word)
  
}