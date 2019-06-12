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
  
  datasetX <-read.table("X.txt",col.names = column_names)
  datasetC <-read.table("C.txt",col.names = "stimolo")
  datasetXC <- cbind(datasetX,cData = datasetC$stimolo)
  write.csv(datasetXC,"datasetXC.csv")

}