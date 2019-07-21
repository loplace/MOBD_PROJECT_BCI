#test 1 0607

#show head of dataset
head(datasetXCY)

#ogni lettera viene identificata tramite 10 iterazioni. ogni iterazione comprende 12 flash
#per leggere una lettera bisogna quindi elaborare 120 righe. 
#si hanno 5 parole per cui ci si aspetta un numero di righe pari a 
#(numero di flash per parola)*(parole) = (5*120)*6 = 3600
nrow(datasetXC)

#divido train e test in modo che l'ultima parola rappresenti il test set
train <- datasetXC[1:3000, ]
test  <- datasetXC[3001:3600, ]