# CUADRO AMORTIZACION METODO FRANCES y TAE.

library(openxlsx)

# Elementos de la rutina RutinaBIII:
#  C cuantia del prestamo; G los gastos; n el numero de periodos; 
#  i el tipo de interes nominal anual; y m la fraccion en los pagos (si mensual m=12).

# En el metodo de amortizacion FRANCES los terminos amortizativos "Co" seran fijos, donde:
# como C = Co*(1-(1+i)^-n)/i) entonces Co = C/((1-(1+i)^-n)/i). 
# Cuadro de amortizacion: Los elementos del cuadro de amortizacion son: El capital vivo CV, 
# los interes I, la cuota de amortizacion A y el capital amortizado CA.

# Calculo de la TAE mediante aproximacion de raices con el Metodo de Newton-Rapson 
# usando rutina RutinaRN.



RutinaBIII <- function(C,G,n,i,m){
  
  # Funcion RNR.
  # 	C=Coef = (a0,a1,a2,...,an) coeficientes del polinomio de grado n. 
  # 	x0  punto de partida, 
  # 	d   iteraciones.	OJO elemento importante.
  # 	n   grado del polinomio.
  
  RutinaNR <- function(C,x0,d){	
    n  <- length(C)-1	# grado del polinomio.	
    
    Cp <- rep(1,n+1)		# Coef. del polinomio derivado.
    for(i in 1:n){ Cp[i]<-C[i+1]*i }
    
    P <- function(C,x){		# Valor del polinomio para x.
      a <- 0			# acumulador
      for(i in 1:(n+1)){a <- a + C[i]*x^{i-1} }
      a
    }
    x    <- rep(0,d+1)
    x[1] <- x0		# valor inicial.
    for(i in 1:d){		# Newton-Rapson
      x[i+1] <- x[i] - (P(C,x[i]) / P(Cp,x[i]) )
    }
    x[d+1]	
  }
  
  
  # Cuadro amortizacion.
  
  ani <- function(n,i){((1-(1+i)^{-n}))/i}    # Funcion ani.
  Sal<- matrix(c(0),n+1,6)             	# ponemos n+1 filas (t=0) y 5 columnas.
  colnames(Sal) <- c("Mes","Cuota","C. interes","C. amortizacion","Capital vivo","Capital amortizado")
  i <- i/m				# tipo anual nominal, frecuencia m.
  Co <- C/ani(n,i)			
  Sal[1,]<- c(0,0,0,0,C,0)		# primera fila de la tabla.
  for(j in 2:(n+1)){		        # Empezamos por la segunda fila. 
    Sal[j,1] <- j-1			# mes
    Sal[j,2] <- Co			# cuota fija          C0
    Sal[j,3] <- Sal[j-1,5]*i	# cuota intereses     I(t)  = CV(t-1)*i
    Sal[j,4] <- Co-Sal[j,3]		# cuota amortizacion  A(t)  = C0-I(t)
    Sal[j,5] <- Sal[j-1,5]-Sal[j,4]	# capital vivo        CV(t) = CV(t-1)-A(t)
    Sal[j,6] <- sum(Sal[,4])	# capital amortizado  CA(t) = sum(A(0):A(t))
  }
  Sal <- round(Sal,2)	# redondeo a dos decimales.
  print(Sal)
  
  
  # Calculo de la TAE con cuantias iguales conocidas (Co).
  A     <- (C-G)/Co
  Coef  <- c(A,-(1+A),rep(0,n-1),1) 	
  x     <- RutinaNR(Coef,.9,100)
  print("TAE en %")
  print(100*((1/x)^m-1))
  
  Sal
}



### Ejemplo practica ordenador PA-3.

C   <- 15000
G   <- 800
n   <- 12*2   # 2 años
m   <- 12
i   <- 0.065  # interes nominal mensual

A <- RutinaBIII(C,G,n,i,m)
A <- as.data.frame(A)
write.xlsx(A,"PrestamoFrances.xlsx")
