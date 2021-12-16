
#  Rutinas aproximacion de raices de polinomios con: 
# 	* Metodo de Bolzano. (RutinaB)	
# 	* Metodo de Newton-Rapson (RutinaNR)       

# Parametros y elementos:                           
# 	C = (c0,c1,c2,...,cn) coef. polinomio de grado n.
# 	I = c(a,b) intervalo. 
# 	x0  punto de partida (para RutinaNR) dentro del intervalo [a,b]
# 	d   iteraciones.

#Si x0 esta cerca de la raiz la computacion es mas rapida

RutinaB <- function(C,I,d){
  library(polynom)
  FP <- polynomial(C)
  P  <- as.function(FP)
  a  <- I[1]
  b  <- I[2]
  if(P(a)*P(b)>0){print("No se cumplen las condiciones de Bolzano")}
  
  x <- rep(0,d)	# sucesion de soluciones parciales,
  
  m <- a
  M <- b
  x[1] <- (M+m)/2
  
  for(i in 2:d){
    if(P(m)*P(x[i-1])<0){ 
      x[i] <- (x[i-1]+m)/2 
      M <- x[i-1]
    }
    else{
      x[i] <- (M+x[i-1])/2 
      m <- x[i-1]
    }
  }
  #for() aplica iteraciones a la parte del intervalo que cumple condiciones
  
  plot(P,a,b,main="Polinomio",ylab="y",col="Red",lwd=2)
  abline(h=0)
  
  cat("Ecuacion polinomica: P(x)=0","\n")
  cat("para el polinomio: P(x) = ")
  print(FP)
  cat("Sucesion de ajuste de raiz en el intervalo [", a, "," ,b, "] usando Bolzano:", "\n")
  print(x)
  cat("Raiz:", x[d],"\n")	
}		




RutinaRN <- function(C,I,x0,d){	
  library(polynom)
  FP <- polynomial(C)
  P  <- as.function(FP)
  Cp <- deriv(FP)
  Pp <- as.function(polynomial(Cp))
  
  x    <- rep(0,d+1)
  x[1] <- x0
  a <- I[1]
  b <- I[2]
  
  for(i in 1:d){
    x[i+1] <- x[i] - (P(x[i]) / Pp(x[i]))
  }
  
  plot(P,a,b,main="Polinomio",ylab="y",col="Red",lwd=2)
  abline(h=0)
  
  cat("Ecuacion polinomica: P(x)=0","\n")
  cat("para el polinomio: P(x) = ")
  print(FP)
  cat("Sucesion de ajuste de raiz en el intervalo [", a, "," ,b, "] usando Newton-Rapson:", "\n")
  print(x)
  cat("Raiz:", x[d+1], "\n")
  x[d+1]	
}

#### Ejemplos.

C <- c(-2,0,1)
I <- c(0,3)
d <- 10

# Bolzano:
x <- RutinaB(C,I,d)	

# Newton-Rapson:
x0 <- 1 
x  <- RutinaRN(C,I,x0,d)

#Ejemplo
C<-c(-1/4,-1/2,1/2)
I<-c(-5/4,0)
RutinaB(C,I,d)
RutinaRN(C,I,-1,10)

#Tarea 4

C<-c((1/8),-(5/4),(1/8))
I<-c(0,2)
d<-100

RutinaB(C,I,d)
RutinaRN(C,I,0.5,d)

#Solucionar con la funcion uniroot
P <- function(x){(1/8)*(x^2) -(5/4)*x + 1/8}
I<-c(0,2)
uniroot(P,I)
