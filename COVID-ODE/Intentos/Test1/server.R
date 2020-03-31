rm(list=ls())
######################### packages requeridos
library(deSolve)

#########################
X_theta_4 <- function( theta, tiempos, X_ini)
{
    SEIRmod <- function(x,tiempos, parms=theta)
    {
        valores.init2 <- t(parms$valores.init[, c("s", "e", "is", "ia","r") ])
        colnames(valores.init2) <- paste0('Sistema', 1:parms$n_b) # cuidar numero de estados
        M1 <- matrix(0, nrow = parms$n_b*5, ncol = parms$n_b*5)
        index <- 0:5
        for (i in index)
        {
            M1[ i*5 +1 , i*5 +1 ] <-
                parms$betas$beta[i+1] * ( -sum( valores.init2[ c("is", "ia"), paste0('Sistema', i+1)] ) )
            M1[ i*5 +2 , i*5 +1 ] <-
                parms$betas$beta[i+1 ]*( -sum( valores.init2[ c("is", "ia"), paste0('Sistema', i+2)] ) )
            M1[rep(i*5, each = 3) + 2:4 , i*5 + 2 ] <-
                c(-parms$sigma, parms$sigma * (1-parms$alpha), parms$sigma*parms$alpha)
            M1[ rep(i*5, each = 2) + c(3,5) , i*5 + 3 ] <-
                c(-parms$gamma_s, parms$gamma_s)
            M1[ rep(i*5, each = 2) + 4:5 , i*5 + 4 ] <-
                c(-parms$gamma_s, parms$gamma_s)
        }
        M2 <- matrix(0, nrow = 5*parms$n_b, ncol = 5*parms$n_b)
        index.j <- index.i <- 0:(parms$n_b-1)
        for( i in index.i)
        {
            for( j in index.j)
            {
                M2[ rep(i*5+1, 5) + 0:4, rep(j*5+1, 5) + 0:4 ] <-
                    diag(x = parms$deltas[j+1, i+1], ncol = 5, nrow = 5)
            }
        }
        index <- 0:(parms$n_b-1)
        for (i in index)
        {
            M2[ rep( i*5+1, each=5)+0:4  , rep( i*5+1, each=5)+0:4  ] <-
                diag( x = -sum( deltas[ i+1,  ][ -(i+1) ]), nrow = 5, ncol = 5)
        }
        M <- M1+M2
        x <- matrix(1, nrow = dim(M)[1], ncol = 1)
        v <- M%*%x
        v <- list(v)
        return(v)
    }
    ## Solver
    out <- lsoda(X_ini, tiempos, SEIRmod, theta)
    return(out)
}

#################### Parametros
####################
days <- 60
tiempos <- seq(0, days,length = days * 5 + 1)
sigma <- 1/5
alpha <- 1/2
gamma_s <- 1/10
gamma_a <- 1/10
w <- 1
valores.init <- read.csv('~/Desktop/COVID-ODE/Test1/ValoresInit.csv',
                         stringsAsFactors = FALSE)
names(valores.init)[1] <- 'Estado'
N <- valores.init$Poblacion # creo que no se ocupan
# supuesto de mismo orden en el nombre de los estados
deltas <-read.csv("~/Desktop/COVID-ODE/Test1/Deltas.csv")
betas <- read.csv("~/Desktop/COVID-ODE/Test1/Betas.csv")
names(betas) <- 'beta'
valores.init2 <- t(valores.init[, c("s", "e", "is", "ia","r") ])
colnames(valores.init2) <- paste0('Sistema', 1:3) # cuidar numero de estados
X_ini <- as.vector(valores.init2)
names(X_ini) <- paste0(row.names(valores.init2), '_',
                       rep( colnames(valores.init2), each = 5) )
############# Terminan
############# Parametros
theta <- list()
theta[['sigma']] <- sigma
theta[['alpha']] <- alpha
theta[['gamma_s']] <- gamma_s
theta[['gamma_a']] <- gamma_a
theta[['betas']] <- betas
theta[['deltas']] <- deltas
theta[['valores.init']] <- valores.init
theta[['w']] <- w
theta[['n_b']] <- 3
#### solucion del sistema de ecuaciones
t1 <- Sys.time()
sim <- X_theta_4(theta, tiempos, X_ini )
t2 <- Sys.time()
t2 - t1

