### Project  - Scripts_ClassTree_Mining

Se describen una serie de funciones útiles en R para analizar datos y crear árboles de clasificación (classification Trees - CTs) con respuesta binaria utilizando el paquete Rpart en R.



#### Lista de funciones relevantes

**crear_train_test_function.R**: función que crea un set de entrenamiento y test a partir de un set de datos completos. 

**cross_val_function.R**: función que permite llevar a cabo una validación cruzada de k folds.

**get_cross_validation_sets_function.R**: función que permite generar los k-folds a partir de un set de datos completos.

**get_sets_function.R**: función que genera el set de entrenamiento y test.

**scores_function.R**: función que permite computar medidas de desempeno del clasificador. Incluye Accuracy, Recall, Precision and F2.



###Scripts

**material_apoyo_ClassificationTree.R**: Ejemplo de aplicación de Classification Tree (CT), utilizando las funciones descritas.