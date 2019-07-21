
reduce_non_target_lines_in_train <- function(train,size) {
  
  target <- split(train, train$target)
  target_plus <- target$'1'
  target_minus <- target$'-1'
  
  target_minus <- sample_n(target_minus, nrow(target_plus)*size, replace = F)
  a <- (rbind(target_plus,target_minus))
  a <- a[sample(nrow(a)),]
  #print(nrow(a))
  return (a)
  
}