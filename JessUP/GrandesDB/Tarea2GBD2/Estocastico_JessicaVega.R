rm(list = ls(all = TRUE))
library(dplyr)
# function utilities
# 
normalizar <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
##################################
setwd('C:\\Users\\usuario\\Desktop\\GitHub\\DSenCDMX\\JessUP\\GrandesDB\\Tarea2GBD2\\')
dat <- read.csv("muestra_checkouts-by-title.csv")
summary(dat)
n <- nrow(dat)

y <- dat %>%
  dplyr::select(V6) %>% 
  as.matrix() 
dat$V6 <- NULL

X <- dat %>%
  select(V4, V5) %>% mutate(x0 = 1, year = normalizar(V4), month=normalizar(V5))
dat$V4 <- dat$V5 <- X$V4 <- X$V5 <- NULL
# convertimos en numericas las variables nominales, slamente para poder hacer la multiplicacion de matrices
for (i in 1:dim(dat)[2])
{#i<- 1
  temp <- dat[ , i]
  temp <- factor(temp)
  dat[ , i] <- normalizar(as.numeric(temp))
}
X <- cbind(X, dat)
X <- as.matrix(X)
######### limpiamos memoria 
rm(dat)
gc()
Sys.sleep(2)
epoch <- 1000
alpha <-0.01

beta <- matrix(rep(0, dim(X)[2]), nrow = dim(X)[2])
costo <- rep(0, epoch)
dim(X)

for(i in seq(1, epoch, by = 1)){
  y_hat <- X %*% beta
  error <- y_hat - y
  beta <- beta - alpha/n * t(X) %*% error
  costo[i] <- 1/(2*n) * sum(error**2)
}

plot(costo)
print("finished")

