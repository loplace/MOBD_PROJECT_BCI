
# eseguiamo il plot della Confusion Matrix 
cm = table(test[start_point:end_point, 1633], y_pred)
cm

#plottiamo graficamente i punti per trovare eventuali pattern
plot(attr(y_pred, "decision.values")[1:6,], col = ifelse(test[start_point:end_point, 1633] == 1, 'green','red'))




