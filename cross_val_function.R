
# nov, 5, 2019.

# Funcion que nos entrega todas las medidas para cada una de las validaciones.

# output: data frame con las medidas de desempeno para cada k-fold.

cross_val <- function(df, k = 10) {
  library(rpart)
  listas <- gen_cross_validation_sets(df = df, k = k)
  performance <-  matrix(NA, ncol=7, nrow=k)
  for ( i in 1:length(listas) ) {
    d <- get_sets(lista = listas, i = i)
    data_train <- d$train_set
    data_test <- d$test_set
    modelo <- rpart(survived ~., data = data_train, method = 'class')
    predicciones <- predict(modelo, data_test, type = 'class')
    perf <- scores(predicted = predicciones, expected = data_test$survived)
    performance[i, ] <- c(perf$accuracy, perf$precision, perf$recall, perf$f1)
  }
  colnames(performance) <- c("accuracy", "precision_yes", "precision_no", "recall_yes", "recall_no", "f1_yes", "f1_no")
  perfromance <- as.data.frame(performance)
  return(performance)
}


