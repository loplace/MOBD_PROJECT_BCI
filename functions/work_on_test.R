
# Questa funzione permette di preparare il dati del test set, caricare il modello ed 
# effettuare la predizione. Al termine la parola richiesta viene scritta su console
returnWord <- function(test){
  testMA <-applyMovingAverage(test,2)
  svmModel <- loadModel()
  
  word = ''
  for(i in 1:5){
    letter <- get_character_by_argmax(svmModel, i, testMA)
    word <- paste(word, letter)
  }
  word <- paste(word, '')
  print(word)
  
}

# questa funzione ritorna una singola lettera predetta. 
get_character_by_argmax <- function(classifier, letter, testToPredict, num_iteration = 10){
  
  # si prendono degli indici che verranno utilizzati per ricavare un subset 
  # del testset relativo al blocco corrispondente alla parola analizzata.
  # Se ad esempio la parola scelta fosse la prima, start_index e last_index 
  # assumerebbero i valori 1, 20.
  start_index <- (letter-1)*num_iteration*2 + 1
  last_index <- start_index + 2*num_iteration -1
  
  # vengono suddivisi gli indici dei blocchi relativi a righe e colonne. 
  # ad esempio 
  # 1: primo blocco di righe, 3: secondo blocco di righe...
  # 2: primo blocco di colonne, 4: secondo blocco di colonne ... 
  row_sub_indexes <- seq(start_index,last_index,by = 2)
  col_sub_indexes <- seq(start_index+1,last_index,by = 2)
  
  all_rows <- NULL
  all_columns <- NULL
  
  # per ciascun singolo blocco di righe (rige 1:6, 13:18, ...) viene predetta la riga
  # della matrice delle lettere
  for (j in row_sub_indexes){
    block <- get_row_col_indexes_by_argmax(j,classifier,testToPredict, 6)
    all_rows <- rbind(all_rows,block)
  }
  
  # per ciascun singolo blocco di colonne (rige 7:12, 19:24, ...) viene predetta la colonna
  # della matrice delle lettere
  for (j in col_sub_indexes){
    block <- get_row_col_indexes_by_argmax(j,classifier,testToPredict, 6)
    all_columns <- rbind(all_columns,block)
  }
  
  row_index<- NULL
  col_index <- NULL
  
  # tramite meccanismi osservati nella funzione returnIndex, vengono scelti gli indici 
  # finali della lettera considerata
  row_index <- returnIndex(1,6,all_rows, 1)
  col_index <- returnIndex(7,12,all_columns ,2)
  
  # gli indici vengono usati per capire la lettera corrispondente
  m <- getMatrix()
  character <- m[row_index,col_index]
  
  return (character)
}

# funzione dove viene effettivamente eseguita la predizione.
# La sezione del dataset che viene sottoposta alla funzione è relativa ad un singolo blocco. 
# Un blocco è un insieme di righe ottenute dal flash di tutte le righe o colonne
# della matrice di lettere in una singola iterazione. 
# Ciascun blocco avrà quindi solo sei righe.
get_row_col_indexes_by_argmax <- function(sub_iteration,classifier,testToPredict, nrowOfBlock = 6){
  
  # viene preso il punto iniziale e finale del test set da considerare.
  # nel caso di sub_iteration con valore 1,2 o 3 si ha ad esempio
  # sub_iteration | 1   			         | 2                    | 3                  | ...
  # start_index   | 1   			         | 7                    | 13                 | ...
  # last index    | 6   			         | 12                   | 18                 | ...
  # significato   | 1* blocco di righe | 1° blocco di colonne | 2° blocco di righe | ...
  start_point <- (sub_iteration-1)*nrowOfBlock + 1
  end_point <- start_point + (nrowOfBlock-1)
  
  # viene rimossa la colonna cData dal test set, ovvero l'indice della riga o colonna
  # della matrice delle lettere che si è illuminata
  c_of_pred <- testToPredict[start_point:end_point, which(colnames(testToPredict) == "cData")]
  testToPredictWithoutC <- testToPredict[, - which(colnames(testToPredict) == "cData")]
  
  # viene eseguita la predizione
  y_pred = predict(classifier, newdata = testToPredictWithoutC[start_point:end_point, - which(colnames(testToPredictWithoutC) == "target") ], 
                   decision.values = TRUE)
  

  
  # y_pred è un oggetto di tipo factor in R. 
  # Per convertirlo in un array di numeri bisogna usare la funzione as.numeric 
  # ricordando che i risultati che escono corrispondono alla posizione del 
  # vettore [-1 1]
  target_prediction <- as.numeric(y_pred)
  target_prediction[target_prediction == 1] = -1
  target_prediction[target_prediction == 2] = 1
  
  # uniamo le predizioni alle colonne e alle decisioni
  target_prediction <- cbind(c_of_pred, target_prediction, attr(y_pred, "decision.values"))
  colnames(target_prediction) = c("row_col", "prediction", "decision_value")
  
  # la tabella viene convertita in un dataframe, 
  target_prediction_df<- as.data.frame(target_prediction) 
  
  return(target_prediction_df)
  
}

# Si inizializza come massimo corrente un valore pari a meno infinito (-5000).
# Per ciascuna lettera si hanno dieci predizioni con dieci differenti decision value.
# Per come è stato definito l'iperpiano di separazione, i valori con target predetto -1 
# hanno decision value positivo. I valori con target predetto +1 hanno decision value 
# negativo.
# nel seguente schema è mostrato un esempio dei valori predetti relativi alle dieci
# iterazioni per una singola riga (il seguente shema fa quindi riferimento alla stessa
# riga illuminata nelle dieci differenti iterazioni)
#
#
#                      -1   /  +1
#                          /
#               d.v.(1.2) /
#                    *   /
#  d.v.(3.5)            /  d.v.(-1.5)
#       *              /       *
#                     /
#         d.v.(1.7)  /                 d.v.(-5.1)
#             *     /                     *
#                  /
#
# l'idea di base è dare valore non solo ai (pochi) punti predetti come +1 ma anche 
# all'incertezza con cui un punto viene predetto -1.
# I decision value vengono cambiati di segno (in modo da massimizzare il valore e non 
# minimizzarlo). Il valore finale a seguito delle dieci iterazioni è dato dalla somma 
# dei quadrati dei singoli valori per il segno che avevo prima dell'elevamento a potenza.
# Viene utilizzato il quadrato per dare più peso a predizioni particolarmente certe.
# Alla fine viene restituito l'indice per cui la funzione assume il valore massimo
returnIndex <- function(start, end, all_rows_and_cols, row_or_col, currentMax = -5000){
  # vengono esaminate le righe 1:6 o le colonne 7:12 della matrice che si illumina
  for (k in start:end){
    r_or_c <- all_rows_and_cols[all_rows_and_cols$row_col == k, ]
    decision_values <- r_or_c$decision_value
    target <- r_or_c$prediction
    final_values <- as.vector(decision_values) * -1
    for ( i in 1:length(final_values)){
      final_values[i] <- (final_values[i])^2 *sign(final_values[i])
    }
    row_col_sum <- sum(final_values)
    #print(row_col_sum)
    
    # Se il valore della funzione è maggiore di quello corrente, l'indice attuale 
    # diventa il nuovo candidato
    if (row_col_sum > currentMax){
      currentMax <- row_col_sum
      # le colonne hanno indice da 7 a 12. per cercare sulla matrice 
      # inizializzata precedentemte bisogna togliere 6
      if(row_or_col == 2){
        index <- k - 6
      }
      else{
        index <- k
      }
    }
  }
  #print("********************************")
  return(index)
}