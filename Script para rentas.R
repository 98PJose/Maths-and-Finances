#  Bloque-I: Rutina para el cálculo del valor de una renta, 	                		          
#  Método Francés, pospagable, flujo/temporal/perpetua, constante / geométrica                    

#
# Construimos                            RutinaVR <- function(C0, P, n, r, i, d)     donde:
#
# Cuant.Inicial: C0
# Periodo:         si P="F", Flujos:   n=matriz de flujos(C0=0). 1ºcol.cuantias, 2ºcol.interes en %
#                         si P="T", Temporal: n=años. 
#                         si P="P", Perpetua: n=años de salida (Ejemplos: n=10 ó 20).
# Razón:            r  en tanto por ciento (si renta constante, r=0)
# Tipo interés:  i (tipo de interés anual en cada periodo, en porcentaje).
# Decimales:    d (entero positivo) número de decimales en salida.




RutinaVR <- function(C0,P,n,r,i,d){
  
  i <- i/100	# tanto por uno.
  r <- 1+(r/100)	# tanto por uno
  
  if(P=="F"){
    N <- n
    n <- dim(N)[1]		# periodos
    N[,2]   <- N[,2]/100         # tantos por uno.
    
    Sal                    <- matrix(c(0),n+1,5)           
    colnames(Sal) <- c("Año","FlujoCuantia","InteresPeriodo","V.actuales","V.finales")
    
    Sal[,1] <- seq(0,n)    
    Sal[,2] <- matrix(c(0,N[,1]),1,n+1)
    Sal[,3] <- matrix(c(0,N[,2]),1,n+1)
    
    I <- matrix(c(1),(n+1),2)	                   # tipos de intereses acumulados	
    for(j in 1:n){
      I[j,1]   <- (1+N[j,2])^{-1}	
      I[j,2]   <- (1+N[j,2])
    }
    
    for(j in 2:(n+1)){                                               
      Sal[j,4] <- N[j-1,1] * prod(I[1:(j-1),1])	# valores actualizados
      Sal[j,5] <- N[j-1,1] * prod(I[j:(n+1),2]) 	# valores finales		
    }
    
    Sal <- round(Sal,d)                                     # redondeo a d decimales
    
    print(Sal)
    cat("Valor actual de la renta:", sum(Sal[,4]), "\n")    # imprimir
    cat("Valor final de la renta:", sum(Sal[,5]), "\n")
  }
  if(P=="T"){
    Sal                    <- matrix(c(0),n+1,4)
    colnames(Sal) <- c("Año","FlujoCaja","V.actuales","V.finales")
    
    Sal[,1]  <- seq(0,n)
    Sal[2,2] <- C0
    
    for(j in 3:(n+1)){ 
      Sal[j,2] <- Sal[j-1,2]*r 	# cuantias en progresión geométrica.
    }	
    
    for(j in 2:(n+1)){ 
      Sal[j,3] <- Sal[j,2]*(1+i)^(-(j-1)) 	# valores actualizados
      Sal[j,4] <- Sal[j,2]*(1+i)^(n-j+1) 	# valores finales
    }
    
    Sal <- round(Sal,d)
    
    print(Sal)
    cat("Valor actual de la renta:", sum(Sal[,3]), "\n")
    cat("Valor final de la renta:", sum(Sal[,4]), "\n")
  }
  
  
  if(P=="P"){
    Sal                     <- matrix(c(0),n+1,3)
    colnames(Sal) <- c("Año","FlujoCaja","V.actuales")
    
    Sal[,1]  <- seq(0,n)
    Sal[2,2] <- C0
    
    if( abs(r) >= abs(1+i) ){		     # Siendo q=r,  |q|<|1+i|  ¿?¿¿?
      print("No convergente")
    }
    
    for(j in 3:(n+1)){ Sal[j,2] <- Sal[j-1,2]*r }		 # cuantias en progresión geométrica.
    for(j in 2:(n+1)){ Sal[j,3] <- Sal[j,2]*(1+i)^(-(j-1)) } # valores actuales
    
    C   <- C0/(1+i-r)					 # suma infinita
    Sal <- round(Sal,d)
    
    print(Sal)
    cat("Valor actual de la renta en", n, "periodos:", sum(Sal[,3]), "\n")
    cat("Valor actual de la renta perpetua:",C,"\n")
  }
  
  Sal
  
}



# Ejemplo: Práctica Ordenador Bloque I 2020/21.

# Ej1.
#A <- RutinaVR(400, "T", 7, 0, 3.5, 2)

# Ej2.
#A <- RutinaVR(1000, "T", 5, 0, 5, 2)

# Ej3.
A <- RutinaVR(1000, "T", 12, 3, 5, 2)

# Ej4.
#F <- matrix(c(12000,20000,16500,1200,rep(5,4)),4,2)
#A <- RutinaVR(0, "F", F, 0, 5, 2)

# Ej4bis.
#F <- matrix(c(12000,20000,16500,1200, seq(1:4)),4,2)
#A <- RutinaVR(0, "F", F, 0, 5, 20)

# Ej5.
A <- RutinaVR(1000, "P", 10, 3, 5, 2)
