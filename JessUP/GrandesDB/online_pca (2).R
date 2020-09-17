#--ejemplo del algoritmo PCA en el data set MNIST
#-
#- Se realiza una comparacion entre batch-PCA (PCA tradicional) y
#- online-PCA utilizando el algoritmo Incremental PCA

rm(list = ls(all = TRUE))
setwd("C:/Users/raul/Desktop/up/ml_grandes_datos/clase_06")


#--utilizo el MNIST data set que se encuentra en la libreria de Keras
mnist <- keras::dataset_mnist()
X <- mnist$train$x
y <- mnist$train$y

#--Por fines didacticos, solo selecciono las muestras correspondientes a un numero
X <- X[y==3, , ]
dim(X)

#--plotear una imagen
image(1:28, 1:28, t(X[4, , ]), col=gray((0:255)/255))

#--convertimos cada matrix(imagen) a un vector
X <- keras::array_reshape(X, c(nrow(X), 784))

#-------------- Batch PCA (PCA tradicional) --------------------------------
batch_pca <- prcomp(X)
str(batch_pca)

  #--porcentaje de varianza explicada
sum((batch_pca$sdev[1:150])^2) / sum((batch_pca$sdev)^2)
#---------------------------------------------------------------------------

#-------------- online PCA -------------------------------------------------
  #--parametros a definir
k  <- 50    #--número principal components que deseamos calcular
n0 <- 100    #--número de muestras usadas para inicializar el algoritmo

  #--calculamos batch-PCA con una muestra pequeña para inicializar el 
  # algoritmo online
pca <- prcomp(X[1:n0, ])
  #--definimos las variables que se actualizaran en el algoritmo online
xbar <- pca$center #--promedio
pca  <- list(values  = pca$sdev[1:k]^2,     #--eigenvalues (varianzas)
             vectors = pca$rotation[,1:k])  #--eigenvectors (nueva base)

n = dim(X)[1]
for (i in (n0+1):n)
{
  #--actualizamos el promedio, recibe tres argumentos
    #  xbar = el promedio anterior
    #  X[i, ] = la muestra con la que vamos a actualizar
    #  i-1 = es el tamaño de la muestra antes de observar X[i, ]
  xbar <- onlinePCA::updateMean(xbar, X[i, ], i-1)
  #--actualizamos nuestro calculo de PCA
  pca  <- onlinePCA::incRpca(lambda = pca$values, #-eigenvalues
                             U = pca$vectors,     #-eigenvectors
                             x = X[i,],           #-muestra actual
                             n = i-1,             #-tamaño de la muestra antes de actualizar
                             q = k,               #-número de principal componentes a calcular
                             center = xbar)       #-promedio
}
#--------------------------------------------------------------------------

#---------- COMPARACION: batch-PCA y online PCA ---------------------------
  #--porcentaje de varianza explicada
sum((batch_pca$sdev[1:50])^2) / sum((batch_pca$sdev)^2)
sum(pca$values[1:50]) / sum((batch_pca$sdev)^2)


  #--comparacion del primer princial component
pc1_batch <- batch_pca$rotation[,1] * (batch_pca$sdev[1])^2
pc1_online <- pca$vectors[,1] * pca$values[1]

par(mfrow = c(1,2))
image(1:28, 1:28, matrix(pc1_batch, nrow = 28), col=gray((0:255)/255))
image(1:28, 1:28, matrix(pc1_online, nrow = 28), col=gray((0:255)/255))

  #--comparación de los vectores transformados
data.frame(Y_batch = t(batch_pca$rotation[,1:50]) %*% t(X[1, , drop = FALSE]),
           Y_online = t(pca$vectors[,1:50]) %*% t(X[1, , drop = FALSE]) )


