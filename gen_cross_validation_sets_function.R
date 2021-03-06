
# nov 05, 2019

# funcion para generar train set y test set utilizando cross validation
# pasos:
# 1. dividir el data set de trabajo en 10 partes en forma aleatoria
#    1.1 tendremos 10 vectores indicando los numeros de fila de cada 1/10 del dataset.
# 2. Generamos 10 mini datasets D1, D2, ... D10
# 3. repetimos 10 veces i = 1.. hasta 10.
#      3.1 elegimos test set D1, y juntamos sets D2 al D10 para train set.
#      3.2 Output: test set como Di, y train set como Di, ... Dj con i...j <> i 

# inputs: df = data set de trabajo limpio
# inputs: k = numero de cross validations (usualmente k=10)
# inputs: i = sets de validacion deseado ( 1 <= i <= k)
# output: test_set y train_set.



# 15-jun-21
# Version 2: 
# vamos a utilizar funcion gen_cross_validation_sets2 que evita el problema de 
# subampleo producido por la version original. Esta version se hace sobre la 
# base de aleatorizar el orden de todas las filas de df y luego tomar floor(nfilas/k)
# consecutivas para generar los folds.


gen_cross_validation_sets <- function(df, k = 10) {
  
  nfilas <- nrow(df)
  filas_per_set <-  floor(nfilas/k)
  
  result <- list(k)
  #temp <- df
  
  ids <- sample(c(1:nrow(df)), size = filas_per_set*k)
  new_df_shuffled <- df[ids,]
  
  start = 1
  for (v in 1:k) {
    end = v*filas_per_set
    result[[v]] <- new_df_shuffled[start:end, ]  # en result quedan almacenados los 1/k datasets
    start = end+1
  }
  salida <-  return(result)
}

# ejemplo
# listas <- gen_cross_validation_sets(df = df, k = 10)
