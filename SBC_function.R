# jan, 17, 2021.

# Funcion para aplicar undersampling a clase mayoritaria utilizando
# # Yen, S. J., & Lee, Y. S. (2009). Cluster-based under-sampling approaches for imbalanced 
# data distributions. Expert Systems with Applications, 36(3), 5718-5727.
# inputs:
#    lista = dataframes con 1/k de los data sets (es la salida de gen_cross_validation_sets)
#    i = numero de cross validation (1 va de 1 hasta k)

#  undersampling por clsuter
# https://github.com/RomeroBarata/bimba/tree/master/R
# https://github.com/RomeroBarata/bimba/blob/master/R/SBC.R

# 1. determinar el nombre de la variable clase
# 2. determinar K = el numero de clusters, N = numero de instancias, m el ratio de MA/MI
# 3. hacer el k-means
# 4. para cada cluster i, determinar el Size_MA y el Size_MI
#       (si no Size_MI del cluster i es 0, entonces lo dejamos en 1)
# 5. para cada cluster i determinar the number of MA samples to be sampled
# 6. samplear MA de cada uno de los clusters (sera app m * Size_MI)
# 7. combinar las instancias encontradas en 6 con todas las MI de la base de datos.

# Inputs:
# K = numero de clusters
# m = ratio entre clase mayoritario y minoritaria (si es 1, habra igual proporcion)
# data = dataset
# name = numbre de la variable clase en el dataset.
SBC <- function(data, k , name, m=1) {
  id <- which(colnames(data) == name)
  
  # identificar clase mayoritaria
  clase_MA <- names(which.max( table(data[,id])  )  )
  clase_MI <- names(which.min( table(data[,id])  )  )
  
  
  #N = nrow(data)
  #m = table(data[,id])[clase_MA]/table(data[,id])[clase_MI]
  
  # k-means
  cluster_ids <- kmeans(data[, -id], centers = k)$cluster
  
  df <- data
  df$cluster_ids <- cluster_ids
  rs <- as.data.frame(table(df$Class, df$cluster_ids)) # clase, cluster, frecuencia
  
  # conteo de Size_MA y el Size_MI para cada cluster
  wh <- which(rs$Freq == 0)
  if (length(wh) > 0) {
    rs[, "Freq"] <- 1
  }
  
  # determinar numero de MA a samplear en cada cluster
  # calculo de denominador
  den = 0
  num <- numeric(length=k)
  for ( i in 1:k) {
    hh <- subset(rs, rs$Var2 == i)
    id_MA <- which(hh$Var1 == clase_MA)
    id_MI <- which(hh$Var1 == clase_MI)
    den <- den + hh[id_MA, 3]/hh[id_MI, 3]
    num[i] <- hh[id_MA, 3]/hh[id_MI, 3]
  }
  SSizes <- floor( m*table(data[,id])[clase_MI] * num/den ) 
  
  selected_samples <- numeric()
  # sampleo de MAs
  for ( i in 1:k) {
    idx <- which( (df$cluster_ids == i) & (df$Class == clase_MA)  )
    idx_sampled <- sample(idx, SSizes[i], replace=TRUE)
    selected_samples <- c(selected_samples,  idx_sampled )
  }
  
  
  # formacion de la nueva bbdd
  df_MI <- subset(df, df$Class == clase_MI)
  df_MA <- df[selected_samples, ]
  new_df <- rbind(df_MI, df_MA)
  # vamos a quital la solucion del cluster
  new_df <- new_df[, -ncol(new_df)]
  
  
  return(new_df)
}
# ejemplo
#new_data <- SBC(data = data, k=2, name="Class", m=1)
#table(new_data$Class)
#table(data$Class)
