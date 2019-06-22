portion <- datasetXCY[1:12, c(9:16,1633,1634)]
portion
scaled <- scale(portion[, 1:8])
scaled <- cbind(scaled, row_col = portion$cData)
scaled
portion <- cbind(scaled, target = portion$target)
remove(scaled)

matplot(t(portion[,1:8]), type = "l", col = ifelse(portion[, 10] == 1, 'red','green'))

ds_mean <- rowMeans(portion[,1:8], na.rm = FALSE, dims = 1)
portion <- cbind(portion, vmean = ds_mean)


