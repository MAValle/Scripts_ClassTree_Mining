# UNDERSAMPLING ejemplo

# en este ejemplo, aplicamos undersampling a clase mayoritaria utilizando
# # Yen, S. J., & Lee, Y. S. (2009). Cluster-based under-sampling approaches for imbalanced 
# data distributions. Expert Systems with Applications, 36(3), 5718-5727.




remove(list = ls())
source("SBC_function.R")


# usando bbdd distinta.
library(imbalance)
data('haberman')
data <- haberman
# The dataset contains cases from a study that was conducted between 1958 and 1970 at the University
# of Chicago’s Billings Hospital on the survival of patients who had undergone surgery for breast
# cancer.
#Age Age of patient at time of operation. Discrete attribute.
#Year Patient’s year of operation. Discrete attribute.
#Positive Number of positive axillary nodes detected. Discrete attribute.
#Class Two possible survival status: positive(survival rate of less than 5 years), negative (survival rate or more than 5 years).
table(data$Class)

# PCA
d_pca <- prcomp(data[,c("Age", "Year", "Positive")])
summary(d_pca)
# con las primeras dos componentes ya explicanos el 94.15% de la variabilidad de los datos.

library(ggfortify)
# coloreo por clase
autoplot(d_pca, data = data, colour = 'Class')
# # # # # # Componente principal



#  undersampling por cluster
new_data <- SBC(data = data, k=4, name="Class", m=1)
table(new_data$Class)
table(data$Class)




# PCA a la nueva data
d_pca <- prcomp(new_data[,c("Age", "Year", "Positive")])
summary(d_pca)
# con las primeras dos componentes ya explicanos el 94.15% de la variabilidad de los datos.

library(ggfortify)
# coloreo por clase
autoplot(d_pca, data = new_data, colour = 'Class')
# # # # # # Componente principal
