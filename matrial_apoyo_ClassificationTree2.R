# Ejemplo classification tree para los estudiantes.

# 250521

# seguimos el ejemplo con bbdd de https://daviddalpiaz.github.io/r4sl/trees.html

# Utilice  dataset Carseats del package ISLR. Debe modificar la variable de respuesta
# Sales del original que es una variable numerica, a una variable binaria con atributo
# high (1) para alto nivel de ventas (mayor que 8), y atributo low (0) para bajo nivel 
# de ventas (menor o igual a 8).
# A simulated data set containing sales of child car seats at 400 different stores.

library(ISLR)
library(dplyr)
library(rpart)
library(rpart.plot)
data(Carseats)
#?Carseats
str(Carseats)
data <- Carseats
rm(Carseats)

# diccionario
# Sales : Unit sales (in thousands) at each location
# CompPrice: Price charged by competitor at each location
# Income: Community income level (in thousands of dollars)
# Advertising: Local advertising budget for company at each location (in thousands of dollars)
# Population: Population size in region (in thousands)
# Price: Price company charges for car seats at each site
# ShelveLoc: A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site
# Age: Average age of the local population
# Education: Education level at each location
# Urban: A factor with levels No and Yes to indicate whether the store is in an urban or rural location
# US : A factor with levels No and Yes to indicate whether the store is in the US or not


# # # # # # 0. CARGA FUNCIONES:
source("scores_function.R")
source("crear_train_test_function.R")
source("gen_cross_validation_sets_function.R")
source("get_sets_function.R")
source("cross_val_function.R")


# # # # # #  PRIMERA PARTE
# modificacion de la variable
data$Sales = as.factor(ifelse(data$Sales <= 8, "Low", "High"))
str(data)


# Primero entrenar un  arbol de clasification no podado (unprunned) utilizando
# todos los predictores.
# ?tree and ?tree.control
seat_tree = rpart(Sales ~ ., data = data, method = 'class')

# seat_tree = tree(Sales ~ ., data = Carseats, 
#                  control = tree.control(nobs = nrow(Carseats), minsize = 10))
#summary(seat_tree)



# lleva a cabo una grafica del arbol de clasificacion
#plot(seat_tree)
#text(seat_tree, pretty = 0)
#title(main = "Unpruned Classification Tree")

# con rpart.plot
rpart.plot(seat_tree)
# Determine cuanto nodos terminales tiene el arbol y cual es la tasa de 
# misclassification.
table(data$Sales)/400
table(data$Sales, data$ShelveLoc)/400

# ver el detalle de las reglas
seat_tree

# predicciones
predicciones <- predict(seat_tree, data, type = 'class')
# matriz de confusion
#confusion matrix
table_mat <- table(pred=predicciones, true=data$Sales)
table_mat

# Accuracy
accuracy <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy : ', accuracy))

#apliquemos:
perf <- scores(predicted = predicciones, expected = data$Sales)
perf






# # # # # #  SEGUNDA PARTE
# Veamos algunos parametros de control.
# Entrene un arbol de clasificacion utulizando un set de datos de training
# y deje uno para test.

set.seed(123) # solo si queremos replicar resultados despues
data_ <- crear_train_test(data = data, size = 0.8)

data_train <- data_[[1]]
data_test <- data_[[2]]

# para saber si las proporciones de la variable clase se mantienen.
prop.table(table(data$Sales))
prop.table(table(data_train$Sales))
prop.table(table(data_test$Sales))

# entrenar modelo
modelo <- rpart(Sales ~., data = data_train, method = 'class')
rpart.plot(modelo)


# predicciones
predicciones <- predict(modelo, data_test, type = 'class')
# matriz de confusion
#confusion matrix
table_mat <- table(pred=predicciones, true=data_test$Sales)
table_mat

# Accuracy
accuracy <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy : ', accuracy))

#apliquemos:
perf <- scores(predicted = predicciones, expected = data_test$Sales)
perf





# cost-complexity:
# La idea es podar el arbol utilizando el parametro de costo cp. Basicamente, es intentar 
# lograr un modelo de arbol lo mas simple posible para evitar overfitting y dejar el modelo
# lo mas generalizable posible. Si vemos que la poda no afecta mucho los resultados, entonces
# nos quedamos con el arbol podado.

# Seleccionamos un parametros apropiado de poda (cost-complexity parameter) \alpha 
# tomando el valor que resulte en la menor prediccion de error.
# Todos estos calculos ya lo hace R al entrenar el modelo con el modelo original (rpart)

printcp(modelo)

# vemps que el menor valor de error de cross-validation (xerror en la tabla) ocurre 
# para \alpha = 0.022059 (esto es CP en Rpart).
opt <- which.min(modelo$cptable[,"xerror"])
cp <- modelo$cptable[opt, "CP"]

# ahora hacemos la poda basado en este valor.
modelo_pruned <- prune(modelo, cp)
#plot tree
rpart.plot(modelo_pruned)


# volvamos a hacer prediccion con este modelo  (recordemos que en modelo anterior acc = 0.775)
predicciones <- predict(modelo_pruned, data_test, type = 'class')
# matriz de confusion
#confusion matrix
table_mat <- table(pred=predicciones, true=data_test$Sales)
table_mat

accuracy <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy : ', accuracy))  # nos da lo mismo!!! y con un modelo mas simple.

# Ahora necesitamos saber si esto se mantiene para distintos sets de entrenamiento.





# # # # # #  TERCERA PARTE
# lleve a cabo un 10FC para entrenar arboles de clasificacion y reporte resultados
# de desempeno. No olvidar determinar considerar la poda.

cross_val(df=data, k=10, nameClass="Sales", prune_tree=TRUE)

cross_val(df=data, k=10, nameClass="Sales", prune_tree=FALSE)
