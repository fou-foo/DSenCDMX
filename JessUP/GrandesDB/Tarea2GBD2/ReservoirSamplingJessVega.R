library(data.table) # la mejorcita forma de leer archivos en R
rm(list = ls(all = TRUE))

t1 <- Sys.time()
setwd('C:\\Users\\usuario\\Desktop\\DSenCDMX\\JessUP\\GrandesDB\\Tarea2GBD2\\') #--path donde se encuentra mi dataset
dir()
path.file <- "checkouts-by-title.csv"
#--tamaño de muestra
n_muestras = 100000

#--definir el número de filas a leer en cada iteración
n_filas_read = 1e6


#--definimos nuestro buffer (muestra) y los rellenamos con las primeras n_muestras filas
buffer <- fread(file= path.file, nrows = n_muestras)

#--indice que nos permitira generar los números aleatorios correctamente
posicion_inicial <- n_muestras

contador <- 1
repeat{
  print(paste0("Posicion inicial: ", posicion_inicial))
  
  #--leemos una parte del archivo
  temp <- fread(file=path.file, nrows = n_filas_read, skip = posicion_inicial + 1) # continuamos muestreando
  
  #--indices que controlan el maximo de cada numero aleatorio
  maximo <- c(1:nrow(temp)) # el orden de los indices se mantiene en la linea 29, donde se lee secuencialmente el archivo original  
  
  #--muestreamos SIN REEMPLAZO la seccion de archivo leida
  j <- sample(maximo)
  sort(unique(j))
  
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
t2 - t1
#--guardar nuestra muestra para un futuro analisis
saveRDS(buffer, "mi_muestra.rds")

#--cerrar la conexión con el archivo
close(connection)
