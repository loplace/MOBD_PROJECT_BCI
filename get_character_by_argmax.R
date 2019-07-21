

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Count elements equals the mode in the original array
get_mode_occurrences <- function(mode,v){
  a <- length(which(v == mode))/10
  return (a)
}

getMatrix <- function(){
  c1 <- c("A","G","M","S","Y","5")
  c2 <- c("B","H","N","T","Z","6")
  c3 <- c("C","I","O","U","1","7")
  c4 <- c("D","J","P","V","2","8")
  c5 <- c("E","K","Q","W","3","9")
  c6 <- c("F", "L", "R", "X", "4", "_")
  
  my_matrix <- cbind(c1, c2, c3, c4, c5, c6)
  
  return(my_matrix)
}

get_character_by_argmax <- function(num_iteration,classifier, lecter, testToPredict){
  
  test_with_c <- testToPredict
  testToPredict <- testToPredict[, - which(colnames(testToPredict) == "cData")]
  
  start_index <- (lecter-1)*num_iteration*2 + 1
  last_index <- start_index + 2*num_iteration -1
  
  #es. (1,3,5,7,9,11,13,15,17,19)
  #1 = primo blocco di righe , 3 = secondo blocco di righe, etc. fino al 10mo
  row_sub_indexes <- seq(start_index,last_index,by = 2)
  col_sub_indexes <- seq(start_index+1,last_index,by = 2)
  
  row_indexes <- numeric(10)
  col_indexes <- numeric(10)
  
  all_rows <- NULL
  all_columns <- NULL
  
  #get rows prediction for each iteration
  for (j in row_sub_indexes){
    block <- get_row_col_indexes_by_argmax(j,classifier,testToPredict, 6)
    all_rows <- rbind(all_rows,block)
  }
  #print(all_rows)
  
  for (j in col_sub_indexes){
    block <- get_row_col_indexes_by_argmax(j,classifier,testToPredict, 6)
    all_columns <- rbind(all_columns,block)
  }
  #print(all_columns)
  

  sum_of_row_decisionvalue <- -5000
  sum_of_col_decisionvalue <- -5000
  
  row_index<- NULL
  col_index <- NULL

  for(i in 1:6){
    row <- all_rows[all_rows$row_col == i, ]
    decision_values <- row$decision_value
    target <- row$prediction
    final_values <- as.vector(decision_values) * -1
    for ( k in 1:length(final_values)){
      final_values[k] <- (final_values[k])^2 *sign(final_values[k])
  }
    row_sum <- sum(final_values)
  
    if (row_sum > sum_of_row_decisionvalue){
      sum_of_row_decisionvalue <- row_sum
      row_index <- i}
  }


  for (j in 7:12){
    col <- all_columns[all_columns$row_col == j, ]
    decision_values <- col$decision_value
    target <- col$prediction
    final_values <- as.vector(decision_values) * -1
    for ( i in 1:length(final_values)){
      final_values[i] <- (final_values[i])^2 *sign(final_values[i])
    }
    col_sum <- sum(final_values)
    if (col_sum > sum_of_col_decisionvalue){
      sum_of_col_decisionvalue <- col_sum
      col_index <- j -6}
  }
  

  
  m <- getMatrix()
  character <- m[row_index,col_index]
  print(character)
  

  return (character)
}
