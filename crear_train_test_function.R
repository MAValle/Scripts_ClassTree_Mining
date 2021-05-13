
# nov 5, 2019

# inputs:
# data: data frame de trabajo depurada
# size: proporcion de la data para set de datos de entrenamiento.


crear_train_test <- function(data, size = 0.8) {
  n_row = nrow(data)
  total_row = round(size * n_row)
  train_sample  <- sample(1:n_row, total_row)
  train_set <- data[train_sample, ]
  test_set <- data[-train_sample, ]
  return(list(train_set = train_set, test_set = test_set))
}
