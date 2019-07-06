# test 1 0607

# show head of dataset
head(datasetXCY)

# ogni lettera viene identificata tramite 10 iterazioni. ogni iterazione comprende 12 flash
# per leggere una lettera bisogna quindi elaborare 120 righe. 
# si hanno 5 parole per cui ci si aspetta un numero di righe pari a 
# (numero di flash per parola)*(parole) = (5*120)*6 = 3600
nrow(datasetXCY)

# divido train e test in modo che l'ultima parola rappresenti il test set
train <- datasetXCY[1:3000, -1633]
test  <- datasetXCY[3001:3600, -1633]

# scriviamo una semplice svm per testare i risultati di base
classifier = svm(formula = target ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'polynomial')

# decidiamo l'iterazione da classificare
sub_iteration <- 1
start_point <- (sub_iteration-1)*6 + 1
end_point <- start_point + 5

# effetuiamo la predizione sul test set
y_pred = predict(classifier, newdata = test[start_point:end_point,-1633], 
                 decision.values = TRUE)

# prendiamo le colonne associate alle predizioni
c_of_pred <- datasetXCY[start_point:end_point, 1633] 

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
  my_col <- max_row[1]
} else {
  min_row <- tar_pred_minus[which.min(tar_pred_minus$decision_value), ]
  my_col <- min_row[1]
}





# eseguiamo il plot della Confusion Matrix 
cm = table(test[start_point:end_point, 1633], y_pred)
cm

#plottiamo graficamente i punti per trovare eventuali pattern
plot(attr(y_pred, "decision.values")[1:6,], col = ifelse(test[start_point:end_point, 1633] == 1, 'green','red'))




