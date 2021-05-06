# Ejemplo classification tree

# Los arboles de clasificacion es una tecnica de Machine Learning
# basado en el algoritmo de particion recursiva, y es la base
# para otros algoritmo mas potentes como el Random Forest.

# Uilizamos bbdd de ejemplo del titanic que contiene 13 variables
# y 1309 observaciones. La idea es predecir que personas van a 
# sobrevivir despues de la colision del Titanic con el iceberg.

# Dictionary:
#   survival	Survival	0 = No, 1 = Yes
#   pclass	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
#   sex	Sex	
#   Age	Age in years	
#   sibsp	# of siblings / spouses aboard the Titanic	
#   parch	# of parents / children aboard the Titanic	
#   ticket	Ticket number	
#   fare	Passenger fare	
#   cabin	Cabin number	
#   embarked	Port of Embarkation	C = Cherbourg, Q = Queenstown, S = Southampton

# # # # # # 0. CARGA Y LIMPIEZA DE DATOS
remove(list = ls())
library(dplyr)

path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
data <- read.csv(path)
str(data)
head(data)
tail(data)
table(data$survived)

# # # # # # 0. CARGA FUNCIONES:
source("scores_function.R")
source("crear_train_test_function.R")
source("gen_cross_validation_sets_function.R")
source("get_sets_function.R")
source("cross_val_function.R")


#  ------> Solo en caso que las variables sex, embarked, age y fare no queden como "factor".
data$sex <- as.factor(data$sex)
data$embarked <- as.factor(data$embarked )
data$age <- as.factor(data$age)
data$fare <- as.factor(data$fare)
str(data)





# # # # # # LIMPIEZA BBDD
# Hay varios valores NA. Aqui vamos a eliminar observaciones que 
# no tienen toda la informaci??n disponible.

# Tambien vamos a eliminar variables que no utilizaremos 
# tales como  home.dest,cabin, name, X y ticket

# La variable clase survive y pcalss la dejaremos en "factor"

library(dplyr)
# Drop variables
df <- data %>%
  select( -c(home.dest, cabin, name, x, ticket ) ) %>%
  mutate_all(funs(replace(., .=='?', NA))) %>% 
  mutate(pclass = factor(  pclass , levels = c(1,2,3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor( survived, levels = c(0,1), labels = c('No', 'Yes')))  %>%
  na.omit()
glimpse(df)



table(df$pclass) # factor
table(df$survived) # factor
table(df$sex) # factor
table(df$age) # factor *******
table(df$sibsp) #entero
table(df$parch) #entero
table(df$fare)  #factor ****
hist(df$fare)
table(df$embarked)  # factor


# dropear factores sin uso.
df <- df %>%  # df <- droplevels(df)
  droplevels() 

# pero age y fare siguen siendo factor, las queremos en formato numerico.
df$age <- as.numeric(df$age)
df$fare <- as.double( levels( df$fare ))[ df$fare ]
str(df)






# # # # # # 1. TRAIN / TEST SET
# creamos set para entrenamiento y test
# instalamos rpart.plot (en caso no este instalada)
# la practica comun es dividir la data en 80/20 para entrenamiento y test
# respectivamente.
# podemos crear una funcion que se llame crear_train_test() que
# tome tres argumentos.

# video: https://youtu.be/pOJIgdJ85S8 o como hacer una funcion en:  https://sites.google.com/view/channelrsvideos

set.seed(678) # solo si queremos replicar resultados despues
data_ <- crear_train_test(data = df, size = 0.8)

data_train <- data_[[1]]
data_test <- data_[[2]]

# para saber si las proporciones de la variable clase se mantienen.
prop.table(table(df$survived))
prop.table(table(data_train$survived))
prop.table(table(data_test$survived))





  











# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # # #  2. ENTRENAR EL MODELO
# Por defecto la funcion rpart() utiliza indice  GINI de impureza para 
# particionar el arbol. Mientras mas alto el indice, implica que las instancias
# son mas distintas en el nodo.

install.packages("rpart.plot")

library(rpart)
library(rpart.plot)


modelo <- rpart(survived ~. , data = data_train, method = 'class')


modelo2 <- rpart(survived ~ str()
                 data = data_train, method = 'class')


  rpart.plot(modelo)
rpart.plot(modelo2)



# % de sobrevivientes en el set train
table(data_train$survived)/nrow(data_train)
# % de mujeres
sum(data_train$sex == 'female')/nrow(data_train) # 37%

sum(data_train$survived == 'Yes' & data_train$sex == 'female')/nrow(data_train) #28% (El 76% de este 28% sobrevive)
sum(data_train$survived == 'No' & data_train$sex == 'female')/nrow(data_train) #9%




print(modelo)
rpart.control()

# Veamos E INTERPRETEMOS el modelo:
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 















# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # #  3. TEST Y PREDICT CON CT
predicciones <- predict(modelo, data_test, type = 'class')
#table_mat <- table(data_test$survived, predicciones)
table_mat <- table(predicciones, data_test$survived)
table_mat


# # # # # #  4 y 5. PERFORMANCE MEASURES Y CONFUSION MATRIX
accuracy <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy : ', accuracy))

#apliquemos:
perf <- scores(predicted = predicciones, expected = data_test$survived)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 





# # # # # #  6. CROSS VALIDATIONS
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

gen_cross_validation_sets <- function(df, k = 10) {

  nfilas <- nrow(df)
  filas_per_set <-  floor(nfilas/k)
  
  result <- list(k)
  temp <- df
  
  for (v in 1:k) {
    ids <- sample(c(1:nrow(temp)), size = filas_per_set)
    temp <- temp[-ids, ]
    result[[v]] <- df[ids, ]  # en result quedan almacenados los 1/k datasets
  }
  salida <-  return(result)
}
# ejemplo
listas <- gen_cross_validation_sets(df = df, k = 10)

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
# ejemplo
d <- get_sets(lista = listas, i=1)
d$train_set
d$test_set


# Funcion que nos entrega todas las medidas para cada una de las validaciones.
cross_val <- function(df, k = 10) {
  library(rpart)
  listas <- gen_cross_validation_sets(df = data, k = k)
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




# # # # # #  6. CROSS VALIDATIONS (PARTE II)
# Hacer 10-FCV sobre data frame df.
# ejemplo:
desempeno <- cross_val(df = df, k = 10)
colMeans(desempeno)


