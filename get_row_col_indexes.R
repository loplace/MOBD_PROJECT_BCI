# Given an integer sub_iteration, this function return the most probable index of row/column wished by user


get_row_col_indexes <- function(sub_iteration,classifier,testToPredict, nrowOfBlock = 6, comparison = NULL){
  
  start_point <- (sub_iteration-1)*nrowOfBlock + 1
  end_point <- start_point + (nrowOfBlock-1)
  
  y_pred = predict(classifier, newdata = testToPredict[start_point:end_point, - which(colnames(testToPredict) == "target") ], 
                   decision.values = TRUE)
  
  # prendiamo le colonne associate alle predizioni
  if(is.null(comparison)){
    c_of_pred <- datasetXCY[start_point:end_point, 1633] 
  }
  else{
    c_of_pred <- comparison[start_point:end_point]
  }
  # y_pred è un oggetto di tipo factor in R. Per convertirlo in un array di numeri bisogna usare 
  # la funzione as.numeric ricordando che i risultati che escono corrispondono alla posizione del 
  # vettore [-1 1]
  target_prediction <- as.numeric(y_pred)
  target_prediction[target_prediction == 1] = -1
  target_prediction[target_prediction == 2] = 1
  
  # uniamo le predizioni alle colonne e alle decisioni
  target_prediction <- cbind(c_of_pred, target_prediction, attr(y_pred, "decision.values"))
  colnames(target_prediction) = c("row_col", "prediction", "decision_value")
  
  #convertiamo la tabella in un data_frame
  target_prediction_df<- as.data.frame(target_prediction) 
  
  #dividiamo in due il dataset in modo da capire quali elementi sono stati classificati + e quali -
  target_prediction_split <- split(target_prediction_df, target_prediction_df$prediction)
  tar_pred_plus <- target_prediction_split$'1'
  tar_pred_minus <- target_prediction_split$'-1'
  
  
  if (nrow(data.frame(tar_pred_plus = numeric())) != 0) {
    max_row <- tar_pred_plus[which.max(tar_pred_plus$decision_value), ]
    my_index <- max_row[1]
  } else {
    min_row <- tar_pred_minus[which.min(tar_pred_minus$decision_value), ]
    my_index <- min_row[1]
  }
  
  return(my_index)
  
}

