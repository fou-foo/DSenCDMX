kmeans.online.b.init <- function(data, k, alpha){
  # clousure para distribuir la eleccion del elemento k
  data <- data
  alpha <- alpha
  function(k){
    # Entradas 
    # data (data.frame): Dataframe donde las observaciones son los elementos a clusterizar y las columnas son las variables
    # k (int): Numero de cluster requerido
    # alpha (numeric): learning rate
    # Salida
    # kmeans.online con los elementos:
    # tabla.master (data.frame): Dos columnas, la primera con el id de la observacion y la segunda con el label del cluster
    # statas.intra (vector): Vector con la media d ela varianza intraelementos
    
    data <- docs.vector
    tabla.master <- data.frame(Obs = row.names(data), Cluster= rep(-Inf, dim(data)[1]))
    # inicializacion alatoria entre el minimo y maximo de cada variable
    stats.min <- sapply(data, min)
    stats.max <- sapply(data, max)
    set.seed(0)
    centroides <- mapply(function(x, y) {runif(k, x, y)},  stats.min, stats.max) 
    # termina inicializacion de centroides
    
    # comienza kmeans proceso online
    
    for( i in 1:dim(data)[1])
    {
      #i <- 11
      #print(i)
      # comienza asignacion de cluster mas cercano
      observacion.en.juego <- as.matrix(data[i, ])
      m.temp <- as.matrix(rbind(observacion.en.juego, centroides))
      distancias <- dist(m.temp)
      m.distancias <- as.matrix(distancias)
      k.i <- which.min(m.distancias[1, 2:(k+1)])
      tabla.master$Cluster[i] <- k.i
      # termina asignacion de cluster más cercano 
      # update de cluster
      centroides[k.i, ] <- centroides[k.i, ] + alpha*observacion.en.juego
    }
    stats <- rep(-Inf, k)
    for ( i in 1:k)
    {
      index <- which( tabla.master$Cluster == i)
      data.subset <- docs.vector[ index, ]
      stats.i <- dist(data.subset)
      stats[i] <- sum(stats.i) # asumimos independencia entre las variables
    }
    kmeans.online <- list( tabla.master =tabla.master, statas.intra = stats)
    return(kmeans.online)
  }
}
kmeans.online.b <- kmeans.online.b.init(data = data, alpha = alpha)