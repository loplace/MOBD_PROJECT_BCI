
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


load_dataset <- function (){
  
  # RINOMINO LE COLONNE
  elettrodi <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "P7","P8")
  column_names <- c()
  for (name in elettrodi){
    for (i in 1:204){
      column_name <- paste(name,i,sep = "_")
      column_names <- c(column_names,column_name)
    }
  }
  
  # LEGGO LE DIVERSE PARTI DEL DATASET E LE UNISCO
  datasetX_part1 <-read.table("dataset/xaa.txt",col.names = column_names)
  datasetX_part2 <-read.table("dataset/xab.txt",col.names = column_names)
  datasetX_part3 <-read.table("dataset/xac.txt",col.names = column_names)
  datasetX_part4 <-read.table("dataset/xad.txt",col.names = column_names)
  
  datasetX_1 <- rbind(datasetX_part1, datasetX_part2)
  datasetX_2 <- rbind(datasetX_part3, datasetX_part4)
  
  datasetX <- rbind(datasetX_1, datasetX_2)
  
  # UNISCO AL DATASET LO STIMOLO E IL TARGET
  datasetC <-read.table("dataset/C.txt",col.names = "stimolo")
  datasetY <-read.table("dataset/Y.txt", col.names = "target")
  datasetCY <- cbind(datasetC,datasetY)
  datasetXC <- cbind(datasetX,cData = datasetC$stimolo)
  datasetXCY <- cbind(datasetXC,target = datasetY$target)

  return(datasetXCY)
  
}


# Test is 2BACI --1
# Test is 5ROSE --2
# Test is ZUPPA --3
# Test is GATTO --4
# Test is MENTE --5
# Test is VIOLA --6
getTestWord <- function(dataset, word){
  
  # ALLENO IL MODELLO SU TUTTO IL DATASET
  if(word == 0){
    train <- dataset
  }
  # ESEGUO UNO SPLIT IN TRAIN E TEST
  else {
    start = (word-1)*600 + 1 
    end   = start + 599
    train <- dataset[- (start:end), - which(colnames(dataset) == "cData")]
    test  <- dataset[start:end,]
  }
  
  return(list('train' = train, 'test' = test))
}











