library(matrixStats)

merge_columns <- function(dataset){
  
  target <- dataset$target
  #print(target)
  
  #print("--------------------------------------------------------------------------------------")
  
  trainV <- data.matrix(dataset,rownames.force = NA)
 # print(trainV[,1:4])
  
  
  meansSens1 <- rowMeans(trainV[,1:204])
  #print(meansSens1)
  meansSens2 <- rowMeans(trainV[,205:408])
  meansSens3 <- rowMeans(trainV[,409:612])
  meansSens4 <- rowMeans(trainV[,613:816])
  meansSens5 <- rowMeans(trainV[,817:1020])
  meansSens6 <- rowMeans(trainV[,1021:1224])
  meansSens7 <- rowMeans(trainV[,1225:1428])
  meansSens8 <- rowMeans(trainV[,1429:1632])
  
 # print("--------------------------------------------------------------------------------------")
  
  devStd1 <- rowSds(trainV[,1:204])
  devStd2 <- rowSds(trainV[,205:408])
  devStd3 <- rowSds(trainV[,409:612])
  devStd4 <- rowSds(trainV[,613:816])
  devStd5 <- rowSds(trainV[,817:1020])
  devStd6 <- rowSds(trainV[,1021:1224])
  devStd7 <- rowSds(trainV[,1225:1428])
  devStd8 <- rowSds(trainV[,1429:1632])
  

new_set_mean <- cbind(meansSens1,meansSens2,meansSens3,meansSens4,meansSens5,meansSens6,meansSens7,meansSens8)
new_set_devStd <- cbind(devStd1,devStd2,devStd3,devStd4,devStd5,devStd6,devStd7,devStd8)
new_set <- as.data.frame(cbind(new_set_mean,new_set_devStd)) 


return(new_set)

}

