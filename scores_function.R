
# Nov, 05, 2019.


# Creamos una funcion para calcular las medidas de desempeno
# fuente: https://stackoverflow.com/questions/8499361/easy-way-of-counting-precision-recall-and-f1-score-in-r

# inputs:
# predicted: vector de estimaciones de la clase del modelo
# expected: vector de valores reales de la clase

# outputs:
# medidas accuracy, recall, precision y f1 para cada valor de la clase.

scores <- function(predicted, expected) {
  predicted <- factor(as.character(predicted), levels=unique(as.character(expected) ) )
  expected  <- as.factor(expected)
  cm = as.matrix(table(expected, predicted))
  
  accuracy <- sum(diag(cm)) / sum(cm)
  precision <- diag(cm) / colSums(cm) # precision para clase NO y YES
  recall <- diag(cm) / rowSums(cm)
  recall <- rev(recall) #  recall para clase NO y YES
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  
  #Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0
  
  #Binary F1 or Multi-class macro-averaged F1
  #ifelse(nlevels(expected) == 2, f1[positive.class], mean(f1))
  
  return(list(accuracy = accuracy, precision = precision, recall = recall, f1 = f1))
}
# ejemplo:
# perf <- scores(predicted = predicciones, expected = data_test$survived)