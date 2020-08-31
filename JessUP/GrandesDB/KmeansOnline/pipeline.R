setwd("C:/Users/usuario/Desktop/GitHub/DSenCDMX/JessUP/GrandesDB/K-meansOnline")
dir()
rm(list=ls())
data <- read.csv(file = 'docword.nips.txt', header = FALSE, sep=' ', skip = 3)
names(data) <- c('Id.Doc', 'Id.Word', 'freq')
require(reshape2)
docs.vector <- dcast(data, Id.Doc ~Id.Word, value.var = 'freq', fill=0)
docs.vector$Id.Doc <- NULL
#docs.vector <- head(docs.vector, 100)
#docs.vector[, 1:dim(docs.vector)[2]] <- scale(docs.vector)
alpha <- 0.1

cluster <- 1:7
for( i in 1:7)
{
  #print(i)
  res <- kmeans.online.b(k=5*i)
  cluster[i] <- sum(res$statas.intra)
  #print(cluster)
}
plot((cluster),type = 'l', xlab = 'Multiplos de 5', ylab = 'Var intra cluster')
plot(abs(diff(cluster)),type = 'l',  xlab = 'Multiplos de 5', ylab = 'diff(Var) intra cluster')
#k <- 4
k <- 25
##############################################
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
##############################################
res <- kmeans.online.b(k=k)
words <- read.csv('vocab.nips.txt', header = FALSE)
names(words) <- 'Palabra'
words$Id.Word <- as.numeric(row.names(words))
data <- merge(data, words, all.x=TRUE)
cluster.palabras.top10 <- data.frame(Cluster=1:25, Palabras='')
require(dplyr)
for (i in 1:25)
{
  index <- which(res$tabla.master$Cluster==i)
  data.subset <- subset(data, Id.Doc %in% index)
  data.subset %>% select(Id.Doc, freq, Palabra) %>% group_by(Palabra) %>% summarise(freq=sum(freq)) %>% 
    arrange(-freq) %>% head(10) -> temp
  string <- paste0(temp$Palabra, collapse = '', sep=', ')
  cluster.palabras.top10$Palabras[i] <- string
}
write.csv(res$tabla.master,file='tabla.master.csv', row.names = FALSE)
cluster.palabras.top10
a <- res$tabla.master
require(xtable)
xtable(cluster.palabras.top10)
##############################################