
# nov, 5, 2019.

# Funcion para generar el set de entretamiento y el de test
# inputs:
#    lista = dataframes con 1/k de los data sets (es la salida de gen_cross_validation_sets)
#    i = numero de cross validation (1 va de 1 hasta k)


get_sets <- function(lista, i) {
  if(i > length(lista)) {
    stop('El argumento i no puede ser mayor que el numero de cross validations')
  }
  test_set <-  lista[[i]]
  templ <- lista[c(-i)]
  train_set <- do.call(rbind, templ)
  
  return(list(train_set = train_set, test_set = test_set))
}
