#Renta Fija. Bonos
library(tidyverse)

#Calculo del valor actual de un bono

#Una anualidad paga: (c/m)P 
#"c" es la tasa de cupón por año
#"m" es el número de períodos que ocurren los flujos de efectivo por año 
#"P" es el valor nominal por periodo para mT periodos
#"t" es el número de años hasta la madurez
#"i" son tipos de descuento nominales
#Fórmula para la suma del valor presente de los pagos de cupones del activo.
#valor presente con rendimientos positivos


VActual<-function(c,m,N,i,t){
  
  P <- (c/m) * N * (1/(i/m) - 1/((i/m) * 
                                    
  (1 + (i/m))^(m * t))) + N/(1 + (i/m))^(m * t)
  return(P)
}


#Bono semestral (m=2), 10 años, N = 100, cupon = 0,05
c<-0.05
N <- 100
i <- c(0.04, 0.05, 0.06)
m <- 2
t <- 10
VActual(c,m,N,i,t)

#Bono anual 5 años, cupon 6,5%, tipo de descuento 4,249%
VB5<-VActual(0.065,1,100,0.04249,5)
VB5

#Evolucion del valor con el interes
c<-0.05
N <- 100
i <- seq(0.01,2,0.01)
m <- 2
t <- 10
VBI<- VActual(c,m,N,i,t)
plot(i,VBI,"l",col="red",main="Evolucion del precio",ylab="Precio",xlab="Tipo de descuento",lwd=2)
Valor<-as.data.frame(cbind(i,VBI))

#Tabla de datos para el Bono
#Bonos con interes fijo y cupon fijo

BonoF<-function(c,N,i,t){
  
  cupon<-c(1:t) #Hay que crear vectores plantilla para que los for() trabajen
  periodo<-c(1:t)
  descuento<-c(1:t)
  
  for(x in 1:t){  
    cupon[x]<-c*N
    } #cupones desde el momento 1 hasta t
  
  cupon[t]=N*(1+c) #El ultimo cupon incluye el nominal
  
  for(x in 1:t){
    
    periodo[x]<-x
    
  } #periodos de tiempo
  
  for(x in 1:t){
    descuento[x]<-((c*N)/((1+i)^x))
  } #cupones descontados

  descuento[t]<-((N*(1+c))/((1+i)^t)) #El ultimo cupon incluye el nominal
  
  bono<-cbind(periodo,cupon,descuento) #Crear matriz
  precio<-sum(descuento) #Valor actual del bono
  colnames(bono)<-c("Tiempo","Cupón","Cupón Actual")
  
  lista<-list(bono,precio)
  names(lista)<-c("Tabla de datos", "Precio")
  return(lista)
  
}

#Bono con cupón 5%, Nominal = 100 a 10 años
c <- 0.065
N <- 100
i <- 0.04249
t <- 5

BonoF(c,N,i,t)
BonoFijo<-BonoF(c,N,i,t)
PrecioBonoFijo<-BonoFijo[[2]]
PrecioBonoFijo
TablaBonoFijo<-as.data.frame(BonoFijo[[1]])
TablaBonoFijo

#Ahora un bono a 100 años

t<-5
N<-100
c<-0.065
i<-c(0.033,0.037,0.039,0.041,0.043)
i<-i+1

BonoF(0.05,100,0.01,100)
Datos100<-as.data.frame(BonoF(0.05,100,0.01,100)[[1]])




#Duracion de un Bono

BonoD<-function(c,N,i,t){
  
  fd<-c(1:t)
  FNC<-c(1:t)
  VAFNC<-c(1:t)
  tt<-c(1:t)

  for(x in 1:t){
    fd[x]<-1/((1+i)^x) 
  }
  
  for(x in 1:t){
    FNC[x]<- (c*N) 
  }
  
  FNC[t]<-(1+c)*N
  
  
  VAFNC<- FNC*fd 
  VAFNCt<- FNC*fd*tt
  D<-sum(VAFNCt)/sum(VAFNC)
  DM<-D/(1+i)
  Datos<-cbind(tt,FNC,fd,VAFNC,VAFNCt)
  colnames(Datos)<-c("Periodo","FNC","F.Descuento","VA(FNC)","VA(FNC)·t")
  lista<-list(Datos,D,DM)
  names(lista)<-c("Tabla de Datos","Duracion (años)","Duracion modificada (%)")
  return(lista)
  
}



t<-5
N<-100
c<-0.06
i<-c(0.06)

D<-BonoD(c,N,i,t)
D
D<-as.data.frame(D[[1]]) #Tabla de datos

#Bono con interes variable

BonoV<-function(c,N,i,t){
  
  cupon<-c(1:t) #Hay que crear vectores plantilla para que los for() trabajen
  periodo<-c(1:t)
  descuento<-c(1:t)
  i<-1+i
  
  for(x in 1:t){  
    cupon[x]<-c*N
  } #cupones desde el momento 1 hasta t
  
  cupon[t]=N*(1+c) #El ultimo cupon incluye el nominal
  
  for(x in 1:t){
    
    periodo[x]<-x
    
  } #periodos de tiempo
  
  xi<-cumprod(i)
  
  for(x in 1:t){
    descuento[x]<-((c*N)/xi[x])
  } #cupones descontados
  
  descuento[t]<-((N*(1+c))/xi[t]) #El ultimo cupon incluye el nominal
  fd<-1/xi
  
  bono<-cbind(periodo,cupon,i,fd,descuento) #Crear matriz
  precio<-sum(descuento) #Valor actual del bono
  colnames(bono)<-c("Tiempo","Cupón","Interes","F. Descuento","Cupón Actual")
  
  lista<-list(bono,precio)
  names(lista)<-c("Tabla de datos","Precio del bono")
  
  return(lista)
  
}

i<-c(0.033,0.037,0.039,0.041,0.043)
N<-100
c<-0.065
t<-5

M<-BonoV(c,N,i,t)
M

M[[2]]#Precio del bono

#Bonos cupon cero y strips de deuda

BC0<-function(c,N,i,t){
  cupon<-c(1:t) #Hay que crear vectores plantilla para que los for() trabajen
  periodo<-c(1:t)
  descuento<-c(1:t)
  
  for(x in 1:t){  
    cupon[x]<-0
  } #cupones desde el momento 1 hasta t
  
  cupon[t]=N*(1+c) #El ultimo cupon incluye el nominal
  
  for(x in 1:t){
    
    periodo[x]<-x
    
  } #periodos de tiempo
  
  for(x in 1:t){
    descuento[x]<-0
  } #cupones descontados
  
  descuento[t]<-((N*(1+c))/((1+i)^t)) #El ultimo cupon incluye el nominal
  
  bono<-cbind(periodo,cupon,descuento) #Crear matriz
  precio<-sum(descuento) #Valor actual del bono
  colnames(bono)<-c("Tiempo","Cupón","Cupón Actual")
  
  lista<-list(bono,precio)
  names(lista)<-c("Tabla de datos", "Precio")
  return(lista)
  
}

#Bono con cupón 5%, Nominal = 100 a 10 años
c <- 0.065
N <- 100
i <- 0.04249
t <- 5

BC0(c,N,i,t)
PBC0 <- BC0(c,N,i,t)[[2]]

#Strips de deuda

SB<-function(c,N,i,t){
  cupon<-c(1:t+1) #Hay que crear vectores plantilla para que los for() trabajen
  periodo<-c(1:t,t)
  descuento<-c(1:t+1)
  
  for(x in 1:t+1){  
    cupon[x]<-c*N
  } #cupones desde el momento 1 hasta t
  
  cupon[1]<-c*N
  cupon[t]<-c*N #Ultimo cupon 
  cupon[t+1]<-N #Bono con el nominal
  
  for(x in 1:t+1){
    
    periodo[x]<-x
    
  } #periodos de tiempo
  
  periodo[t+1]<-t
  
  for(x in 1:t+1){
    descuento[x]<-((c*N)/((1+i)^x))
  } #cupones descontados
  
  descuento[1]<-(c*N)/((1+i))
  descuento[t+1]<-N/((1+i)^t)
  
  bono<-cbind(periodo,cupon,descuento) #Crear matriz
  colnames(bono)<-c("Tiempo","P.Nominal","P.Actual")
  
  cat("\n")
  print("Tabla de precios cupon 0")
  cat("\n")
  return(bono)
  
}


#Si queremos dividir un bono a 15 años con estas características:

c <- 0.065
N <- 100
i <- 0.04249
t <- 15

SB(c,N,i,t)


#Comparamos con bono a 15 años

BonoF(c,N,i,t)
A<-BonoF(c,N,i,t)[[2]]

SB15<-as.data.frame(SB(c,N,i,t))
B<-sum(SB15$P.Actual)

A
B
