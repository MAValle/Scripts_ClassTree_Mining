### Project  - Scripts_ClassTree_Mining

Se describen una serie de funciones útiles en R para analizar datos y crear árboles de clasificación (classification Trees - CTs) con respuesta binaria utilizando el paquete Rpart en R.



#### Lista de funciones relevantes

**crear_train_test_function.R**: función que crea un set de entrenamiento y test a partir de un set de datos completos. 

**cross_val_function.R**: función que permite llevar a cabo una validación cruzada de k folds.

**get_cross_validation_sets_function.R**: función que permite generar los k-folds a partir de un set de datos completos.

**get_sets_function.R**: función que genera el set de entrenamiento y test.

**scores_function.R**: función que permite computar medidas de desempeno del clasificador. Incluye Accuracy, Recall, Precision and F2.

**SBC_function**: función que permite llevar a cabo undersampling a clase mayoritaria utilizando método de: Yen, S. J., & Lee, Y. S. (2009). Cluster-based under-sampling approaches for imbalanced data distributions. Expert Systems with Applications, 36(3), 5718-5727.



###Scripts

**material_apoyo_ClassificationTree.R**: Ejemplo de aplicación de Classification Tree (CT), utilizando las funciones descritas.

** material_apoyo_ClassificationTree2.R: Ejemplo 2 de aplicación de Classification Tree (CT), para el problema de poda de árboles.

**script_apoyo_undersampling.R**: Ejemplo de aplicación de Undersampling basado en clustering.
