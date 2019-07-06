# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getMatrix <- function(){
  c1 <- c("A","G","M","S","Y","5")
  c2 <- c( "B","H","N","T","Z","6")
  c3 <- c("C","I","O","U","1","7")
  c4 <- c("D","J","P","V","2","8")
  c5 <- c("E","K","Q","W","3","9")
  c6 <- c("F", "L", "R", "X", "4", "_")
  
  my_matrix <- cbind(c1, c2, c3, c4, c5, c6)
  
  return(my_matrix)
}

get_character <- function(num_iteration,classifier, lecter){
  
  start_index <- (lecter-1)*num_iteration*2 + 1
  last_index <- start_index + 2*num_iteration -1
  
  #es. (1,3,5,7,9,11,13,15,17,19)
  row_sub_indexes <- seq(start_index,last_index,by = 2)
  col_sub_indexes <- seq(start_index+1,last_index,by = 2)
  
  row_indexes <- numeric(10)
  col_indexes <- numeric(10)
  
  
  i = 1 
  #get most probable row index for each iteration
  for (j in row_sub_indexes){
    row_indexes[i] <- get_row_col_indexes(j,classifier)
    i <- i +1
  }

  i = 1
  #get most probable column index for each iteration
  for (j in col_sub_indexes){
    col_indexes[i] <- get_row_col_indexes(j,classifier)
    i <- i+1
  }
  
  #compute mode for both arrays
  row_final_index <- as.numeric(getmode(row_indexes))
  col_final_index <- as.numeric(getmode(col_indexes))
  
  col_final_index <- col_final_index - 6
  
  m <- getMatrix()
  character <- m[row_final_index,col_final_index]
  
  return (character)
}
