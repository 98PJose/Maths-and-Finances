
###Rutinas para modelo de Markowitz###

#Librerias 

library(quadprog)
library(fBasics)

# P   = Precios, con nombre activos
# x   = ponderaciones de una cartera de ejemplo.
# dd  = numero de decimales.
# Ojo : Rendimientos y riesgos (en R var es varianza corregida) en tantos por ciento.
# Usa la cuasivarianza


RutinaBII <- function(P,x,dd){
  
  n <- dim(P)[1] #filas de la matriz, numero de periodos
  m <- dim(P)[2] #columnas de la matriz, numero de activos
  
  # MA es la matriz de rendimientos (n-1) de los (m) activos. 
  # rA rendimientos esperados y RA matriz de varianzas.
  
  MA <- matrix(c(0),n-1,m)
  for(j in 1:m){
    for(i in 2:n){
      MA[i-1,j] <- 100*(P[i,j]-P[i-1,j])/P[i-1,j] #Rtos, variacion precio
    }
  }
  rA<- matrix(c(colMeans(MA)),1,m) #rtos esperados, media
  RA<- var(MA)
  
  Sal <- matrix(c(0),n+2,2*m+1)	
  Sal[1:n,1:m] 	       <- P	# Precios
  Sal[2:n,(m+1):(2*m)]   <- MA	# Rendimientos
  Sal[n+1,(m+1):(2*m)]   <- rA	# Rendimientos esperados (Medias).
  Sal[n+2,(m+1):(2*m)]   <- diag(RA)	# Riesgos activos
  
  # Rendimiento esperado y riesgo de la cartera del ejemplo. 
  for(i in 2:n){
    Sal[i,(2*m)+1] <- sum(x * MA[i-1,]) 
  }
  
  rC <- sum(x %*% t(rA))
  RC <- x %*% var(MA) %*% t(x)
  Sal[n+1,(2*m)+1] <- rC
  Sal[n+2,(2*m)+1] <- RC
  
  
  
  
  ### Madre: Salida
  cnP <- colnames(P)
  
  Sal <- round(Sal,2)
  
  SalP <- Sal[1:n,1:m]
  colnames(SalP)  <- cnP
  rownames(SalP)  <- seq(0,n-1)
  
  SalR <- Sal[,(m+1):(2*m+1)]
  colnames(SalR)  <- c(cnP,"Cart.Ejemplo")
  rownames(SalR)  <- c(seq(0,n-1),"Rend.medios","Riesgos")
  
  print("Tabla de precios:")
  print(SalP)
  cat("\n")
  
  Datos           <- matrix(c(x),1,m)
  colnames(Datos) <- cnP
  rownames(Datos) <- c("Pond Cartera ejemplo")
  print("Activos/Ponderaciones ejemplo:")
  print(Datos)
  cat("\n")
  
  print("Tabla de rendimientos:")
  print(SalR)
  cat("\n")
  
  
  
  V           <- var(MA)
  colnames(V) <- cnP
  rownames(V) <- cnP
  
  
  Sol <- matrix(c(0),m+1,m)
  Sol[1,]       <- rA		# rendimientos
  Sol[2:(m+1),] <- V		# Matriz de covarianzas
  
  
  
  ####  Markowitz.
  
  
  n <- dim(V)[1] # numero de activos
  d <- 10^(-dd)
  a <- seq(0,1,d)
  l <- length(a)	# 100d+1
  
  # Activos
  
  A <- matrix(c(0),n,2)
  colnames(A) <- c("Riesgo","Rendimiento")
  A[,2] <- rA
  A[,1] <- diag(V)
  
  # Ponderaciones: Combinar n valores entre 0 y 1 con d decimales.
  #todas las combinaciones posibles de carteras
  
  CP <- function(n,d){
    vm  <- rep(0,n)
    vM  <- rep(1,n)
    Dec <- d
    
    for (i in 1:n){
      if (i==1){ AD <- list(seq(vm[i], vM[i], Dec)) }
      else{ 
        AL <- list(seq(vm[i], vM[i], Dec))
        AD <- c(AL,AD)
      }
    }
    D   <- expand.grid(AD)
    Dp  <- data.frame(D,suma=rowSums(D))
    as.matrix(D[Dp$suma==1,])
  }
  
  P  <- CP(n,d)
  ll <- dim(P)[1]
  
  S <- matrix(c(0),ll,2)
  colnames(S) <- c("Riesgo","Rendimiento")
  
  
  for(i in 1:ll){
    S[i,2]       <- sum(P[i,]*rA)
    aa           <- matrix(c(P[i,]),1,n)
    S[i,1]       <- aa %*% V %*% t(aa)
  }
  
  SS <- matrix(c(0),ll,n+2)
  SS[,1:n]         <- P
  SS[,(n+1):(n+2)] <- S
  
  #print(SS)
  
  
  # Curva dos a dos activos.
  
  if(n>2){
    
    kk <- sum(ifelse(SS[,1]==0,1,0))
    m  <- choose(n,2)
    nn <- seq(1,n,1)
    C  <- array(c(0),c(m,kk,2)) #array matriz de mas de 2 dimensiones
    
    conta <- 0
    for(i in 1:(n-1)){
      for(j in (i+1):n){
        conta <- conta + 1
        I <- nn[which(nn != i & nn != j)]
        C[conta,,] <- SS[which(SS[,I] == 0),(n+1):(n+2)]
      }}
    
  }
  
  MV <- SS[which(S[,1]==min(S[,1])),]
  MVp <- matrix(c(MV[1:n]),1,n)
  PMV <- matrix(c(MV[(n+1):(n+2)]),1,2)
  colnames(MVp)<-cnP
  
  A<-matrix(c(0),m+1,m)
  A[1,]        <- rA
  A[2:(m+1),]  <- V
  colnames(A)  <- cnP
  rownames(A)  <- c("Rend.",cnP)
  
  print("Rend. y Matriz de covar de Activos")
  print(A)
  
  cat("\n")
  print("Riesgo / Rendimiento cartera de MARCOKOWITZ (MVP)")
  print(PMV)
  
  plot(S, main = "Figura 1. Grafico Carteras")
  par(new=TRUE)
  if(n>2){for(i in 1:m){points(C[i,,], col="blue")}}
  par(new=TRUE)
  points(PMV, col="red", pch = 24)
  
  Sol
  cat("\n")
  print("Ponderaciones cartera de MARCOKOWITZ (MVP)")
  return(MVp)
}

# Ejemplo Practica 2020.
# Precios:
#	t	A	B	C
#	1	14	1,3	8,4
#	2	15,8	1,8	8,8
#	3	14,5	2,4	8,15
#	4	16,3	1,9	9,4
#	5	17,5	2,1	8,6

# Empecemos con dos activos, A y B

Pp <- matrix(c(14,15.8,14.5,16.3,17.5,1.3,1.8,2.4,1.9,2.1),5,2)
colnames(Pp) <- c("A", "B")
xp <- matrix(c(0.4,0.6),1,2)
A <- RutinaBII(Pp,xp,1)


# Ahora con los tres (A, B y C)

P <- matrix(c(14,15.8,14.5,16.3,17.5,1.3,1.8,2.4,1.9,2.1,8.4,8.8,8.15,9.4,8.6),5,3)
colnames(P) <- c("A", "B", "C")

x <- matrix(c(0.2,0.5,0.3),1,3)

A <- RutinaBII(P,x,1) #1 decimal
(A <- RutinaBII(P,x,2)) #2 decimales
A <- RutinaBII(P,x,0) #2 decimales #Nos permite ver los rendimientos y riesgos de los activos base 
#cada pico de la grafica es uno de los activos

#Rutina para obtener los rentimientos
Retorno <- function(P){
  n <- dim(P)[1] #filas de la matriz, numero de periodos
  m <- dim(P)[2] #columnas de la matriz, numero de activos
  MA <- matrix(c(0),n-1,m)
  for(j in 1:m){
    for(i in 2:n){
      MA[i-1,j] <- (P[i,j]-P[i-1,j])/P[i-1,j] #Rtos, variacion precio
    }
  }
  
  print("Rendimientos")
  
  return(MA)
} 

#Rutina para obtener la cartera de Markowitz
#Rutina para obtener la cartera de Markowitz
Cartera <- function(rtos, r){
  # Arguments:
  # rtos - Matriz de rendimientos de los activos
  # r - redimiento promedio deseado
  # 1 Create Portfolio Settings:
  nAssets = ncol(rtos)
  Dmat = cov(rtos)
  dvec = rep(0, times=nAssets)
  Amat = t(rbind(
    Return=colMeans(rtos),
    Budget=rep(1, nAssets),
    LongOnly=diag(nAssets)))
  bvec = c(
    Return=r,
    budget=1,
    LongOnly=rep(0, times=nAssets))
  meq = 2
  # 2 Optimize Weights:
  portfolio = solve.QP(Dmat, dvec, Amat, bvec, meq)
  weights = round(portfolio$solution, digits = 4)
  names(weights) = colnames(rtos)
  # Return Value:
  lista<-list(
    Pesos = 100*weights,
    Riesgo = portfolio$value,
    Rendimiento = r)
  return(lista)
} 

#Ejemplo 1

rtos <- 100 * LPP2005REC[, 1:6]

r <- mean(colMeans(rtos))

Cartera(rtos, r)

#Ejemplo 2

(P <- matrix(c(14,15.8,14.5,16.3,17.5,1.3,1.8,2.4,
               1.9,2.1,8.4,8.8,8.15,9.4,8.6),5,3))


(rP <- Retorno(P))

(a<-Cartera(rP,0.059339))

#Rutina Modelo Markowitz #Solo pesos

PesosCMV <- function(rtos, rr){
  #rtos = matriz de rendimientos
  #rr = rendimiento esperado
  nAssets = ncol(rtos)
  portfolio = solve.QP(
    Dmat = cov(rtos),
    dvec = rep(0, times=nAssets),
    Amat = t(rbind(Return=colMeans(rtos),
                   Budget=rep(1, nAssets), LongOnly=diag(nAssets))),
    bvec = c(Return=rr, budget=1,
             LongOnly=rep(0, times=nAssets)),
    meq=2)
  weights = portfolio$solution
  return(weights)
}

PesosCMV(rP,0.059339)

#Frontera eficiente

Frontera <- function(rtos, nPoints){
  # Number of Assets:
  nAssets = ncol(rtos)
  # Target Returns:
  mu = colMeans(rtos)
  targetReturns <- seq(min(mu), max(mu), length=nPoints)
  # Optimized Weights:
  weights = rep(0, nAssets)
  weights[which.min(mu)] = 1
  for (i in 2:(nPoints-1)) {
    newWeights = PesosCMV(rtos, targetReturns[i])
    weights = rbind(weights, newWeights)
  }
  newWeights = rep(0, nAssets)
  newWeights[which.max(mu)] = 1
  weights = rbind(weights, newWeights)
  weights = round(weights, 4)
  colnames(weights) = colnames(rtos)
  rownames(weights) = 1:nPoints
  # Return Value:
  weights
  
  #Plot
  rto_objetivos = seq(min(mu), max(mu), length = nrow( weights))
  riesgo_objetivo = NULL
  for (i in 1:nrow( weights)) {
    riesgo_objetivo_nuevo = sqrt( weights[i, ] %*% cov(rtos) %*%
                                    weights[i, ])
    riesgo_objetivo = c(riesgo_objetivo, riesgo_objetivo_nuevo)
  }
  
  plot(riesgo_objetivo, rto_objetivos, pch = 19,
       xlab="Riesgo(sd)",ylab="Rendimiento(mean)",main="Curva de Minima Varianza")
  
  return(weights)
}

(pesos <- Frontera(rP,50))



