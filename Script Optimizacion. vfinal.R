###############################################################
# Rutina para Optimizar lineal con variables reales positivas #
###############################################################
# Instalar paquete lpSolve.
# Problema (Min, <=, R+), lineal y no negativo.
# Datos de entrada:
# C = vector (1xn) de coeficientes de la funcion objetivo.
# A = matriz (mxn) de coeficientes de restricciones. n variables m restricciones
# B = vector (1xm) de terminos independientes.
# m1/m2 = numero de restricciones de <= y = (m=m1+m2). m1 = m - m2: m es el total de restricciones

RutinaOpLP <- function(C,A,B,m1){
  library("lpSolve")
  n <- dim(A)[2] # no variables.
  m <- dim(A)[1] # no restricciones.
  m2 <- m-m1
  rs <- c(rep("<=",m1), rep("==",m2)) # restricciones
  Sol <- lp("min",C,A,rs,B)
  cat("solucion:",Sol$sol,"\n")
  cat("F.Objetivo:",Sol$objval,"\n")
  Sol
}
# Ejemplo:
# Min x - y + 2z
# s.q.: 
# x - 2y + z <= 3
# 2x - y + z <= 1
# x + y + 2z = 4
# x,y,z en R+
# Entonces:
C <- c(1,-1,2)
A <- matrix(c(1,2,1,-2,-1,1,1,1,2),3,3)
B <- c(3,1,4)
# Solucion: 0, 4, 0
# F.Objetivo: -4
S <- RutinaOpLP(C,A,B,2)

#Ejemplo
#Min 12x + 20y
# 2x + 5y >= 20
# 3x + y >= 17 
# x,y en Z+
#Un Max se debe convertir a Min cambiando signos
#Las restricciones deben ser <= por tanto se cambia el signo
C <- c(12,20)
A <- matrix(c(-2,-3,-5,-1),2,2)
B <- c(-20, -17)
S <- RutinaOpLP(C,A,B,1)

#######################################################
# Rutina para resolver problemas de:                  #
# Opt. cuadratica con restricciones lineales de <= y  #
# variables reales positivas (OpCLP).                 #
#######################################################
# Instalar paquete lpSolve.
# Problema (Min, >=, R+), cuadratico/lineal no negativo.
# Datos de entrada:
# X0 = valor (inicial) en el interior del conjunto factible. Valor que cumpla las restricciones
# C = matriz (nxn) de la forma cuadratica en la funcion objetivo.
# A = matriz (mxn) de coeficientes de restricciones.
# B = vector (1xm) de terminos independientes.

RutinaOpCLP <- function(X0,C,A,B){
  library("lpSolve")
  f <- function(X){t(X) %*% C %*% X} # funcion objetivo.
  Gf <- function(X){2 * X %*% C} # gradiente de f.
  Sol <- constrOptim(X0, f, Gf, A, B)
  cat("Solucion:",Sol$par,"\n")
  cat("F.Objetivo:",Sol$value,"\n")
  Sol
}
# Ejemplo:
# Min x^2 + y^2 + z^2 - xy + 2xz - yz
# s.q.: x + 2y + z >= 2
# 2x - y + z >= 1
# x,y,z en R+
# Entonces:
X0 <- c(1,1,5)
C <- matrix(c(1,-1/2,1,-1/2,1,-1/2,1,-1/2,1),3,3) #Forma cuadratica
A <- matrix(c(1,2,2,-1,1,1),2,3)
B <- c(2,1)
# Solucion: 2.099974 0.7142861 -1.528546
# F.Objetivo: 0.4285714
S <- RutinaOpCLP(X0,C,A,B)

#Ejemplo 
#Min f(x,y) = x^2 + y^2 + 3xy
#s.a
#2x +5y >= 20
#3x +y >= 17
#En caso de ser necesario se deberia convertir la FO a Min y las restricciones a >=
A <- matrix(c(2,3,5,1),2,2)
C <- matrix(c(1,3/2,3/2,1),2,2) #Forma cuadratica
B<-c(20,17)
X0 <- c(5,3)
S <- RutinaOpCLP(X0,C,A,B)


#TA3

#Apartado 1
C <- c(1,0,1)
A <- matrix(c(-8,-1,-1,0.06,-1,0,0,-1,1),3,3)  #Es la matriz de coeficientes de las restricciones
B <- c(-60,-24,0)  #Es el vector de términos independientes
S <- RutinaOpLP(C,A,B,2) #Es la solución

#Apartado 3

#v1
X0 <- c(10,1,11) #Punto que cumple las restricciones.
C <- matrix(c(-1,0,0,0,-1,0,0,0,-1),3,3)
#C = matriz (nxn) de al forma cuadrática en la función objetivo.
A <- matrix(c(8,-1,-1,-0.06,0,-1,0,1,-1),3,3)  #Es la matriz de coeficientes de las restricciones
B <- c(60,0,-24)  #Es el vector de términos independientes
S <- RutinaOpCLP(X0,C,A,B)

#v2
X0 <- c(10,1,11) #Punto que cumple las restricciones.
C <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
#C = matriz (nxn) de al forma cuadrática en la función objetivo.
A <- matrix(c(8,-1,-1,-0.06,0,-1,0,1,-1),3,3)  #Es la matriz de coeficientes de las restricciones
B <- c(60,0,-24)  #Es el vector de términos independientes
S <- RutinaOpCLP(X0,C,A,B)