
rm(list=ls())

require(deSolve)


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
t2
#dev.off()
matplot(sim[,1],sim[,-1],t="l",xlab="time", ylab="%")
legend("bottomright",legend= colnames(sim)[-1],lty=1:6, col=1:6)

write.csv(sim, file='sim_Lety.csv', row.names=FALSE)
####################################################################################








