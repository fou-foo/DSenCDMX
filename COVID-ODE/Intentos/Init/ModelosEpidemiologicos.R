
rm(list=ls())

require(deSolve)

#### Modelo 1:SEIR Deterministico con infectados sintomáticos y asintomáticos
X_theta<-function(theta,t=tiempos, X_ini){
  SEIRmod <- function(t, x, theta) {
    with(as.list(c(theta, x)),
         {
           ds <- -bet*s*(is+ia)
           de <- bet*s*(is+ia)-sig*e
           dis <- sig*(1-alf)*e-gams*is
           dia <- sig*(alf)*e-gama*ia
           dr <- gams*is+gama*ia
           dy <-sig*(1-alf)*e
           res <- c(ds, de, dis, dia, dr,dy)
           list(res) }
    ) }
  ## Solver
  out <- lsoda(X_ini, t, SEIRmod, theta)
  out[which(out[,3]<0),3]<-0
  return(out)
}


# Ejemplo  ####################################################

# Tiempos considerados para las simulaciones
days<-60
tiempos = seq(0,days,length=days*5+1)
# Tamano total de la poblacion
N=500000
# Vector de valores iniciales para el sistema de ecuaciones diferenciales

X_ini=c(s =(N-10)/N, e = 0, is = 5/N, ia = 5/N, r = 0, y = 5/N)

#http://gabgoh.github.io/COVID/index.html
#Rocklöv, Sjödin and Wilder-Smith 	Princess Diamond 	14.8 	5.0 	10.0
#theta<-c(bet = 14.18*10,sig = 1/5,alf = 1/2 ,gams = 1/10,gama =1/10 )

theta<-c(bet = 1.3*10 ,sig = 1/5,alf = 1/2 ,gams = 1/10,gama =1/10 )

sim<-X_theta(theta=theta,tiempos,X_ini)
head(sim)
colnames(sim)
summary(rowSums(sim[,-c(1,7)]))

#dev.off()
matplot(sim[,1],sim[,-1],t="l",xlab="time", ylab="%")
legend("bottomright",legend= colnames(sim)[-1],lty=1:6, col=1:6)

matplot(sim[,1],sim[,-1]*N,t="l",xlab="time",  ylab="number of individuals")
legend("bottomright",legend= colnames(sim)[-1],lty=1:6, col=1:6)


matplot(sim[-1,1],diff(sim[,7]*N),t="l",xlab="time",  ylab="number of individuals")






####################################################################################

#### Modelo 2:SEIR Deterministico con infectados sintomáticos y asintomáticos y tres grupos de edad
X_theta_2<-function(theta,t=tiempos, X_ini){
  SEIRmod <- function(t, x, theta) {
    with(as.list(c(theta, x)),
         {
           ds1 <- -bet*s1*(is1+ia1+is2+ia2+is3+ia3)
           ds2 <- -bet*s2*(is1+ia1+is2+ia2+is3+ia3)
           ds3 <- -bet*s3*(is1+ia1+is2+ia2+is3+ia3)
           de1 <- bet*s1*(is1+ia1+is2+ia2+is3+ia3) - sig1*e1
           de2 <- bet*s2*(is1+ia1+is2+ia2+is3+ia3) - sig2*e2
           de3 <- bet*s3*(is1+ia1+is2+ia2+is3+ia3) - sig3*e3
           dis1 <- sig1*(1-alf)*e1-gam1s*is1
           dis2 <- sig2*(1-alf)*e2-gam2s*is2
           dis3 <- sig3*(1-alf)*e3-gam3s*is3
           dia1 <- sig1*(alf)*e1-gam1a*ia1
           dia2 <- sig2*(alf)*e2-gam2a*ia2
           dia3 <- sig3*(alf)*e3-gam3a*ia3
           dr <- gam1s*is1+gam2s*is2+gam3s*is3 + gam1a*ia1+gam2a*ia2+gam3a*ia3
           dy1 <- sig1*(1-alf)*e1
           dy2 <- sig2*(1-alf)*e2
           dy3 <- sig3*(1-alf)*e3
           res <- c(ds1, ds2, ds3, de1, de2, de3, dis1, dis2, dis3, dia1, dia2, dia3, dr, dy1, dy2, dy3)
           list(res)}
    ) }
  ## Solver
  out <- lsoda(X_ini, t, SEIRmod, theta)
  out[which(out[,3]<0),3]<-0
  return(out)
}
# Ejemplo  ####################################################

# Tiempos considerados para las simulaciones
days<-60
tiempos = seq(0,days,length=days*5+1)

# Vector de valores iniciales para el sistema de ecuaciones diferenciales
nis1<-3
nis2<-10
nis3<-5
N1<-100000
N2<-200000
N3<-100000
# Tamano total de la poblacion
N<-N1+N2+N3

X_ini2=c(s1 =(N1-nis1)/N
        , s2 =(N2-nis2)/N
        , s3 =(N3-nis3)/N
        , e1 = 0
        , e2 = 0
        , e3 = 0
        , is1 = nis1/N
        , is2 = nis2/N
        , is3 = nis3/N
        , ia1 = 2*nis1/N
        , ia2 = 2*nis2/N
        , ia3 = 2*nis3/N
        , r = 0
        , y1 = nis1/N
        , y2 = nis2/N
        , y3 = nis3/N
        )

#http://gabgoh.github.io/COVID/index.html
#Rocklöv, Sjödin and Wilder-Smith 	Princess Diamond 	14.8 	5.0 	10.0
#theta<-c(bet = 14.18*10,sig = 1/5,alf = 1/2 ,gams = 1/10,gama =1/10 )

theta<-c(bet = 1.2*10
         , sig1 = 1/5
         , sig2 = 1/5
         , sig3 = 1/5
         , alf = 1/2
         , gam1s = 1/10
         , gam2s = 1/10
         , gam3s = 1/10
         , gam1a =1/10
         , gam2a =1/10
         , gam3a =1/10
         )

sim<-X_theta_2(theta=theta,tiempos,X_ini2)
head(sim)
colnames(sim)
summary(rowSums(sim[,-c(1,15:17)]))

plot(rowSums(sim[,-c(1,15:17)]),t="l")
plot(sim[,14],t="l")
matplot(sim[,2:14],t="l")

#dev.off()
matplot(sim[,1],sim[,-1],t="l",xlab="time", ylab="%")
legend("bottomright",legend= colnames(sim)[-1],lty=1:6, col=1:6)

matplot(sim[,1],sim[,-1]*N,t="l",xlab="time",  ylab="number of individuals")
legend("bottomright",legend= colnames(sim)[-1],lty=1:6, col=1:6)

matplot(sim[-1,1],cbind(diff(sim[,15]*N),diff(sim[,16]*N),diff(sim[,17]*N)),t="l",xlab="time",  ylab="number of individuals")






####################################################################################

#### Modelo 3:SEIR con subpoblaciones según el modelo 1 y migración entre los estados de la Republica

source("Modelo3_LR.R")


# Ejemplo  ####################################################

# Tiempos considerados para las simulaciones
days<-60
tiempos = seq(0,days,length=days*5+1)

# Vector de valores iniciales para el sistema de ecuaciones diferenciales

###  Valores iniciales
valores_ini<-read.csv("valores_ini.csv", head=TRUE)
n_s<-paste("s",1:32,sep="")
n_e<-paste("e",1:32,sep="")
n_is<-paste("is",1:32,sep="")
n_ia<-paste("ia",1:32,sep="")
n_r<-paste("r",1:32,sep="")
n_y<-paste("y",1:32,sep="")

tt<-as.matrix(valores_ini[,-(1:5)])
tt<-as.vector(tt)
X_ini<-c(tt)

nombres<-c(n_s,n_e,n_is,n_ia,n_r,n_y)
names(X_ini)<-nombres

N<-valores_ini[,4]

##  Parametros

#http://gabgoh.github.io/COVID/index.html
#Rocklöv, Sjödin and Wilder-Smith 	Princess Diamond 	14.8 	5.0 	10.0
#theta<-c(bet = 14.18*10,sig = 1/5,alf = 1/2 ,gams = 1/10,gama =1/10 )

parametros<-read.csv("parametros.csv", head=TRUE)

n_bet<-paste("bet",1:32,sep="")
n_delta<-paste("delta",rep(1:32,32),"_",rep(1:32,each=32),sep="")

tt<-as.matrix(parametros[,-(1:3)])
tt<-as.vector(tt)
#str(tt)
theta<-c(sig = 1/5,alf = 1/2 ,gams = 1/10,gama =1/10, tt)

nombres<-c("sig","alf","gams","gama", n_bet,n_delta)
#length(theta)
names(theta)<-nombres

#Simulacion
t1 <- Sys.time()
sim<-X_theta_3(theta=theta,tiempos,X_ini)
head(sim)
t2 <- Sys.time()
t2 - t1
#dev.off()
matplot(sim[,1],sim[,-1],t="l",xlab="time", ylab="%")
legend("bottomright",legend= colnames(sim)[-1],lty=1:6, col=1:6)


####################################################################################








