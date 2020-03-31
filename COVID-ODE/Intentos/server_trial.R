rm(list=ls())
######################### packages requeridos
library(deSolve)
#################### Parametros
####################
days <- 60
tiempos <- seq(0, days,length = days * 5 + 1)
sigma <- 1/5
alpha <- 1/2
gamma_s <- 1/10
gamma_a <- 1/10
w <- 1
valores.init <- read.csv('~/Desktop/COVID-ODE/Init/ValoresInit.csv',
                         stringsAsFactors = FALSE)
names(valores.init)[1] <- 'Estado'
N <- valores.init$Poblacion # creo que no se ocupan
# supuesto de mismo orden en el nombre de los estados
deltas <-read.csv("~/Desktop/COVID-ODE/Init/Deltas.csv")
betas <- read.csv("~/Desktop/COVID-ODE/Init/Betas.csv")
names(betas) <- 'beta'
valores.init2 <- t(valores.init[, c("s", "e", "is", "ia","r") ])
colnames(valores.init2) <- as.character(1:32) # cuidar numero de estados
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
theta[['n_b']] <- 32
##############MATRIX
valores.init2 <- t(theta$valores.init[, c("s", "e", "is", "ia","r") ])
colnames(valores.init2) <- as.character(1:theta$n_b) # cuidar numero de estados
for (i in 1:length(X_ini))
{
    eval(parse(text = paste0( names(X_ini)[i], ' <- ', X_ini[i])))
}

M1 <- matrix(0, nrow = theta$n_b*5, ncol = theta$n_b*5)
index <- 0:30
for (i in index)
{
    M1[ i*5 +1 , i*5 +1 ] <-
        theta$betas$beta[i+1] * ( -sum( valores.init2[ c("is", "ia"), as.character(i+1)] ) )
    M1[ i*5 +2 , i*5 +1 ] <-
        theta$betas$beta[i+1 ]*( -sum( valores.init2[ c("is", "ia"), as.character(i+2)] ) )
    M1[rep(i*5, each = 3) + 2:4 , i*5 + 2 ] <-
        c(-theta$sigma, theta$sigma * (1-theta$alpha), theta$sigma*theta$alpha)
    M1[ rep(i*5, each = 2) + c(3,5) , i*5 + 3 ] <-
        c(-theta$gamma_s, theta$gamma_s)
    M1[ rep(i*5, each = 2) + 4:5 , i*5 + 4 ] <-
        c(-theta$gamma_s, theta$gamma_s)
}
M2 <- matrix(0, nrow = 5*theta$n_b, ncol = 5*theta$n_b)
index.j <- index.i <- 0:(theta$n_b-1)
for( i in index.i)
{
    for( j in index.j)
    {
        M2[ rep(i*5+1, 5) + 0:4, rep(j*5+1, 5) + 0:4 ] <-
            diag(x = theta$deltas[j+1, i+1], ncol = 5, nrow = 5)
    }
}
index <- 0:(theta$n_b-1)
for (i in index)
{
    M2[ rep( i*5+1, each=5)+0:4  , rep( i*5+1, each=5)+0:4  ] <-
        diag( x = -sum( deltas[ i+1,  ][ -(i+1) ]), nrow = 5, ncol = 5)
}
M <- M1+M2
colnames(M) <- row.names(M) <- names(X_ini)
############################
#########################
X_theta_4 <- function(  z)
{
    y <- matrix(1, nrow = dim(M)[1], ncol = 1)
    row.names(y) <- paste0('d_',row.names(M))

    for (i in 1:dim(M)[1])
    {
        string <- ''
        for(j in 1:dim(M)[1])
        {
            if( M[i,j] != 0)
            {
                string <- paste0(
                    string, as.character(M[i,j]), '*', row.names(M)[j], '+'  )
            }
        }
        string <- paste0( row.names(y)[i],' <- ',        string, '0'  )
        print(i)
        print(string)
        #z <- scan()
        eval(parse(text = string))
        #z <- scan()
        ls()
        index <- grep("d_", ls())
        vars <- ls()[index]
        res <- list()
        for (i in 1:length(index))
        {
            res[[ vars[[i]] ]] <- eval(parse(text = vars[[i]] ))
        }
        return(res)
    }
}
#### solucion del sistema de ecuaciones
t1 <- Sys.time()
sim <- lsoda(y = X_ini, times=tiempos, func=X_theta_4, parms = 0)
t2 <- Sys.time()
t2 - t1
write.csv(sim, file='siim_foo.csv', row.names = FALSE)



eval(parse(text='foo <- 1'))
