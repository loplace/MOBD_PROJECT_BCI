
# funzione che ritorna la matrice corrispondente alla griglia delle lettere
# presente nella traccia del problema
# la matrice finale risulta essere
# | A | B | C | D | E | F |
# | G | H | I | J | K | L |
# | M | N | O | P | Q | R |
# | S | T | U | V | W | X |
# | Y | Z | 1 | 2 | 3 | 4 |
# | 5 | 6 | 7 | 8 | 9 | _ |
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


# Per poter avorare con più file di piccola dimensione invece che con un singolo grande file, 
# il dataset è stato diviso in più parti, presenti nella directory 'dataset'.
# Questa funzione combina tutte le parti del dataset e ritorna un unico dataframe contenente
# i valori dei sensori, la colonna stimolo e il target
load_dataset <- function (){
  
  # vengono rinominate le colonne
  elettrodi <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "P7","P8")
  column_names <- c()
  for (name in elettrodi){
    for (i in 1:204){
      column_name <- paste(name,i,sep = "_")
      column_names <- c(column_names,column_name)
    }
  }
  
  # vengono unite le diverse parti del dataset
  datasetX_part1 <-read.table("dataset/xaa.txt",col.names = column_names)
  datasetX_part2 <-read.table("dataset/xab.txt",col.names = column_names)
  datasetX_part3 <-read.table("dataset/xac.txt",col.names = column_names)
  datasetX_part4 <-read.table("dataset/xad.txt",col.names = column_names)
  
  datasetX_1 <- rbind(datasetX_part1, datasetX_part2)
  datasetX_2 <- rbind(datasetX_part3, datasetX_part4)
  datasetX <- rbind(datasetX_1, datasetX_2)
  
  # il dataset viene unito al target e allo stimolo
  datasetC <-read.table("dataset/C.txt",col.names = "stimolo")
  datasetY <-read.table("dataset/Y.txt", col.names = "target")
  datasetCY <- cbind(datasetC,datasetY)
  datasetXC <- cbind(datasetX,cData = datasetC$stimolo)
  datasetXCY <- cbind(datasetXC,target = datasetY$target)

  return(datasetXCY)
  
}

#come load_dataset, ma per il test-set
load_test_dataset <- function (){
  
  elettrodi <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "P7","P8")
  column_names <- c()
  for (name in elettrodi){
    for (i in 1:204){
      column_name <- paste(name,i,sep = "_")
      column_names <- c(column_names,column_name)
    }
  }
  
  datasetX <-read.table("test/X.txt",col.names = column_names)
  datasetC <-read.table("test/C.txt",col.names = "stimolo")
  datasetY <-read.table("test/Y.txt", col.names = "target")
  datasetCY <- cbind(datasetC,datasetY)
  datasetXC <- cbind(datasetX,cData = datasetC$stimolo)
  datasetXCY <- cbind(datasetXC,target = datasetY$target)
  
  test <<- datasetXCY
}


# Test is 2BACI --1
# Test is 5ROSE --2
# Test is ZUPPA --3
# Test is GATTO --4
# Test is MENTE --5
# Test is VIOLA --6
# 
# Questa funzione divide opportunamente train e test set.
# I dati relativi alla parola inserita dall'utente (sotto forma di indice, 
# riportato sopra) diventano il nuovo test set. Tutti gli altri sono il train set
getTestWord <- function(dataset, word){
  
  # se si inserisce 0 si allena il modello su tutto il dataset
  if(word == 0){
    train <- dataset
  }
  # altrimenti viene eseguito uno split
  else {
    start = (word-1)*600 + 1 
    end   = start + 599
    train <- dataset[- (start:end), - which(colnames(dataset) == "cData")]
    test  <- dataset[start:end,]
  }
  
  return(list('train' = train, 'test' = test))
}











