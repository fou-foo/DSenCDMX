rm(list = ls(all = TRUE))
t1 <- Sys.time()
setwd('C:\\Users\\usuario\\Desktop\\DSenCDMX\\JessUP\\GrandesDB\\Tarea2GBD2\\')
dir()
path.file <- "checkouts-by-title.csv" #--path donde se encuentra mi dataset

#--tamaño de muestra
n_muestras <- 100000

#--definir el número de filas a leer en cada iteración
n_filas_read <- 3e6 # la rm nos permite cargar este numero de registros y agilizar el muestreo

#--abrir una conexión para leer el archivo
connection <- file(path.file, open = "r")

#--nombre de las columnas (primera fila)
col_names  <- read.csv(connection, nrows = 1, header = FALSE)

#--definimos nuestro buffer (muestra) y los rellenamos con las primeras n_muestras filas
buffer <- read.csv(connection, nrows = n_muestras, header = FALSE, stringsAsFactors = FALSE)

#--indice que nos permitira generar los números aleatorios correctamente
posicion_inicial <- n_muestras

#--definimos una función que nos permita generar numeros aletorios de forma incremental
random_unif <- function(x) sample.int(x,1)

contador <- 1
repeat{
  print(paste0("Posicion inicial: ", posicion_inicial))
  
   #--leemos una parte del archivo
  temp <- read.csv(connection, nrows = n_filas_read, header = FALSE)

  #--indices que controlan el maximo de cada numero aleatorio
  maximo <- c(1:nrow(temp)) + posicion_inicial

  #--generamos numeros aleatorios de forma vectorizada, segun esto solo permuta los indices
  j = vapply(maximo, random_unif, FUN.VALUE = integer(1))

  #--observamos cuales de los numeros aleatorios son menores que nuestra muestra
  idx <- j <= n_muestras

  #--sustituimos los que resultaron menores
  buffer[j[idx], ] <- temp[idx, ]
  
  print(paste0("iteracion: ", contador))
  contador <- contador + 1
  #--redefinimos la posición inicial para la siguiente iteracion
  posicion_inicial <- posicion_inicial + nrow(temp)
  
  #--si el numero de filas leidas es menor que el esperado,
  #-asumimos que se acabo el archivo y salimos del ciclo
  if(nrow(temp) < n_filas_read)
    break
}
t2 <- Sys.time()
t2 - t1 #(Time difference of 10.45983 mins )



#--guardar nuestra muestra para un futuro analisis
write.csv(buffer, paste0("muestra_", path.file), row.names = FALSE )

#--cerrar la conexión con el archivo
close(connection)
