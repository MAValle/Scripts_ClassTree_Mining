
# nov 5, 2019

# inputs:
# data: data frame de trabajo depurada
# size: proporcion de la data para set de datos de entrenamiento.
# train = TRUE para obtener train set, FALSE para test set.

crear_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = round(size * n_row)
  train_sample  <- sample(1:n_row, total_row)
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}