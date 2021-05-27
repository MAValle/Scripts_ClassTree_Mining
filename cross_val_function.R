
# may, 27, 2021.

# Funcion que nos entrega todas las medidas para cada una de las validaciones.
# NOTA: se asume que TODAS las variables seran predictoras. Caso contrario deben sacarse
# inputs:
# nameClass  = nombre de la variable clase
# df: dataframe con todas als variables predictoras y variable clase.
# k = numero de cross validations.
# prune_tree=FALSE  en caso que no queramso arboles podados.

# output: data frame con las medidas de desempeno para cada k-fold.

cross_val <- function(df, k = 10, nameClass, prune_tree=FALSE) {
  library(rpart)
  ClassColNum <- as.numeric( grep(nameClass, names(df)) )
  listas <- gen_cross_validation_sets(df = df, k = k)
  performance <-  matrix(NA, ncol=7, nrow=k)
  for ( i in 1:length(listas) ) {
    d <- get_sets(lista = listas, i = i)
    data_train <- d$train_set
    data_test <- d$test_set
    
    #model training
    f <- paste(names(data_train)[ClassColNum], "~", paste(names(data_train)[-ClassColNum], collapse=" + "))
    modelo <- rpart(f , data = data_train, method = 'class')
    #modelo <- rpart( paste(names(df[ClassColNum])) ~., data = data_train, method = 'class')
    
    if(prune_tree==FALSE) {
      predicciones <- predict(modelo, data_test, type = 'class')
      reales <- data_test[[names(data_test[ClassColNum])]]    
      perf <- scores(predicted = predicciones, expected = reales )
      performance[i, ] <- c(perf$accuracy, perf$precision, perf$recall, perf$f1)
      
    }else{
      opt <- which.min(modelo$cptable[,"xerror"])
      cp <- modelo$cptable[opt, "CP"]
      pruned_modelo <- prune(modelo,cp)
      reales <- data_test[[names(data_test[ClassColNum])]]    
      predicciones <- predict(modelo, data_test, type = 'class')
      reales <- data_test[[names(data_test[ClassColNum])]]    
      perf <- scores(predicted = predicciones, expected = reales )
      performance[i, ] <- c(perf$accuracy, perf$precision, perf$recall, perf$f1)
    }
  }
  colnames(performance) <- c("accuracy", "precision_yes", "precision_no", "recall_yes", "recall_no", "f1_yes", "f1_no")
  perfromance <- as.data.frame(performance)
  return(performance)
}

# EJEMPLO:
# cross_val(df=data, k=10, nameClass="Sales", prune_tree=TRUE)

