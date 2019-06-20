# Script to Generate Column Names
library(dplyr)

load_file_with_names <- function (filePath){
  
  elettrodi <- c("Fz", "Cz", "Pz", "Oz", "P3", "P4", "P7" , "P8")
  column_names <- c()
  for (name in elettrodi){
    for (i in 1:204){
      column_name <- paste(name,i,sep = "_")
      column_names <- c(column_names,column_name)
      }
  }
  datasetX_part1 <-read.table("dataset/xaa.txt",col.names = column_names)
  datasetX_part2 <-read.table("dataset/xab.txt",col.names = column_names)
  datasetX_part3 <-read.table("dataset/xac.txt",col.names = column_names)
  datasetX_part4 <-read.table("dataset/xad.txt",col.names = column_names)
  
  datasetX_1 <- rbind(datasetX_part1, datasetX_part2)
  datasetX_2 <- rbind(datasetX_part3, datasetX_part4)
  
  datasetX <- rbind(datasetX_1, datasetX_2)

  #datasetX <-read.table("X.txt",col.names = column_names)
  
  datasetC <-read.table("dataset/C.txt",col.names = "stimolo")
  datasetXC <- cbind(datasetX,cData = datasetC$stimolo)
  write.csv(datasetXC,"dataset/datasetXC.csv")

}