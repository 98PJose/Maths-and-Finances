###Análisis Técnico Bursátil###

#OHLC "Open", "High", "Low", "Close", "Volume"

#Librerias

library(ggplot2)
library(readr)
library(rvest)
library(pbapply)
library(TTR)
library(dygraphs)
library(tidyquant)
library(timetk)
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series 
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(plotly)     # Interactive plots
library(corrplot)   # Visuazlize correlation plots
library(fpp2)
library(zoo)
library(VIM)

#Bases de datos y obtencion

accion1 <- read_csv("C:/Users/98pjo/OneDrive/Escritorio/Análisis Técnico/accion.txt")
getSymbols("GS",src="yahoo") #sin especificar fecha #Obtenemos cotizaciones de GS
getSymbols("ACS.MC",src="yahoo",from="2017-12-01", 
           to="2018-01-01")%>%get() #src="fuente" #con rango de fechas
getSymbols("XPT/USD",src="oanda") #Conversion dolar a platino

#Creacion de objeto con datos
accion2<-getSymbols("ACS.MC",src="yahoo",from="2017-12-01", 
                    to="2018-01-01")%>%get() #src="fuente" #con rango de fechas

getSymbols("SBUX",from="2018-01-01")

#Separar por tiempo o fecha (Ejemplo acciones de GS)

GS['2018'] #Goldman's 2018 OHLC 

GS['2018-01'] #now just January of 2008

GS['2017-06::2018-01-12'] #De Junio del 17 a 12 de Enero del 18

GS['::'] #Todo GS

GS['2018::'] # 2018 en adelante

last(GS) #ultima observacion

#Escala de tiempo

periodicity(ACS.MC) #Informacion sobre la periodicidad
unclass(periodicity(ACS.MC)) #Mas informacion
to.weekly(ACS.MC) #Convertimos a periodo semanal
to.monthly(GS) #a mensual
#Numero de dias, semanas y años en la muestra:
ndays(GS); nweeks(GS); nyears(GS)

#Tratamiento temporal

# Encontrar el precio de cierre maximo de cada semana 
apply.weekly(GS,FUN=function(x) { max(Cl(x)) } ) 

# Lo mismo en formato general 
period.apply(GS,endpoints(GS,on='weeks'), 
             FUN=function(x) { max(Cl(x)) } ) 

#Maximo cierre mensual
period.apply(GS,endpoints(GS,on='months'), 
             FUN=function(x) { max(Cl(x)) } )
#Con min(Cl(x)) tendriamos el minimo

#Otra alternativa
as.numeric(period.max(Cl(GS),endpoints(GS,on='weeks')))

#Obtencion de retornos (Rendimientos/Rentabilidad)
#Tasas de variación de las cotizaciones

dailyReturn(SBUX) # returns by day
rtos<-dailyReturn(SBUX) # returns by day

weeklyReturn(SBUX) # returns by week

monthlyReturn(SBUX) # returns by month, indexed by yearmon

allReturns(SBUX) # todas las posibilidades

#Comprobacion grafica de la normalidad de los retornos

SBUX_log_returns <- SBUX %>%
  Ad() %>%
  dailyReturn(type = "log")

names(SBUX_log_returns) <- "SBUX.Log.Returns"

# Plot the log-returns    
SBUX_log_returns %>%    
  ggplot(aes(x = SBUX.Log.Returns)) + 
  geom_histogram(bins = 100) + 
  geom_density() +
  geom_rug(alpha = 0.5)

#Observar la distribucion de los retornos
probs <- c(.005, .025, .25, .5, .75, .975, .995)
dist_log_returns <- SBUX_log_returns %>% 
  quantile(probs = probs, na.rm = TRUE)
dist_log_returns

#Media y desviacion tipica logarítmica (Volatilidad)
mean_log_returns <- mean(SBUX_log_returns, na.rm = TRUE)
sd_log_returns <- sd(SBUX_log_returns, na.rm = TRUE)
mean_log_returns 
sd_log_returns 

#Analisis descriptivo
summary(accion1)
mean(accion1$Close)
sd(accion1$Close) #Volatilidad precio
sd(rtos) #Volatilidad rendimientos
mean(rtos) #Media

#Graficos

#Grafico de cotizaciones con plot(x,y)

plot(accion1$Date,accion1$Close,"l",xlab="Fecha",ylab="Cotizacion",main="Grafico cotizacion")
#xlab,ylab son los titulos de los ejes, main el titulo y "l" indica lineal
#Color azul
plot(accion1$Date,accion1$Close,"l",xlab="Fecha",ylab="Cotizacion",
     main="Grafico cotizacion", col="blue")
#xlab,ylab son los titulos de los ejes, main el titulo y "l" indica lineal

#Grafico cotizaciones con ggplot()

ggplot(data = accion1) +
  geom_line(mapping = aes(x = Date, y = Close))
#Color azul
ggplot(data = accion1) +
  geom_line(mapping = aes(x = Date, y = Close),color="blue")

#Grafico con chartSeries()

#Comprobamos si los datos tienen la forma OHLC "Open", "High", "Low", "Close", "Volume"
is.OHLC(accion1)

chartSeries(GS) #Grafico del título Goldman(GS)
chartSeries(GS,theme='white') #theme='white' fondo blanco
chartSeries(GS,theme='white.mono') #theme='white.mono' blanco y negro
chartSeries(GS,subset='2015-07-01::2015-08-09') #Seleccion de fechas

reChart(major.ticks='months',subset='first 16 weeks') #Seleccion fechas
reChart(major.ticks='years',subset='first 16 months') #Seleccion fechas

chartSeries(GS,theme='white',subset = '2018-01::2018-06') #theme='white' fondo blanco
#subset permite escoger fechas

#Añadimos indicadores al grafico
#Tabla de indicadores en tabla.png

chartSeries(GS,TA="addVo();addBBands();addCCI()")

chartSeries(GS,TA="addVo();addBBands();addCCI()")
reChart(major.ticks='months',subset='first 30 weeks')

chartSeries(GS,TA="addVo();addBBands();addCCI()",
            subset='2015-07-01::2015-08-09')

chartSeries(GS, 
            type = c("auto", "matchsticks"), 
            show.grid = TRUE,
            subset='first 30 weeks',
            theme='white',
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addSMA(n=20,col = 'red'),addSMA(n=5,col = 'blue'),addVo()))

barChart(to.daily(ACS.MC),up.col='white',dn.col='blue') #Grafico de barras
#to.daily() hace el objeto diario
barChart(to.weekly(ACS.MC),up.col='white',dn.col='blue') #semanal

chartSeries(XPTUSD,name="Platinum (.oz) in $USD") 

chartSeries(to.weekly(XPTUSD),up.col='green',dn.col='red')

#Media móvil simple

#Medias y su representacion a distintos plazos
plot(accion1$Date,accion1$Close,"l",ylab="Cotizacion",col="red",main="Cotizacion",lwd=2)

mediasaccion1.5<-rollmean(accion1$Close, k=5, fill=NA) #Media movil a 5
plot(accion1$Date[1:length(mediasaccion1.5)],mediasaccion1.5,"l",
     ylab="Cotizacion",xlab="fecha",col="red",main="Media Movil Simple a 5",lwd=2)

mediasaccion1.10<-rollmean(accion1$Close, k=10, fill=NA) #Media movil a 10
plot(accion1$Date[1:length(mediasaccion1.10)],mediasaccion1.10,"l",
     ylab="Cotizacion",xlab="fecha",col="red",main="Media Movil Simple a 10",lwd=2)

mediasaccion1.25<-rollmean(accion1$Close, k=25, fill=NA) #Media movil a 25
plot(accion1$Date[1:length(mediasaccion1.25)],mediasaccion1.25,"l",
     ylab="Cotizacion",xlab="fecha",col="red",main="Media Movil Simple a 25",lwd=2)

mediasaccion1.50<-rollmean(accion1$Close, k=50, fill=NA) #Media movil a 50
plot(accion1$Date[1:length(mediasaccion1.50)],mediasaccion1.50,"l",
     ylab="Cotizacion",xlab="fecha",col="red",main="Media Movil Simple a 50",lwd=2)

mediasaccion1.100<-rollmean(accion1$Close, k=100, fill=NA) #Media movil a 100
plot(accion1$Date[1:length(mediasaccion1.100)],mediasaccion1.100,"l",
     ylab="Cotizacion",xlab="fecha",col="red",main="Media Movil Simple a 100",lwd=2)

#Media movil junto a la cotizacion
plot(accion1$Date,accion1$Close,"l",ylab="Cotizacion",col="blue",main="Cotizacion",lwd=1)
lines(accion1$Date[1:length(mediasaccion1.100)],mediasaccion1.100,lwd=2,col="red")

#Media Movil a corto y largo plazo
mediasaccion1.20<-rollmean(accion1$Close[1:100], k=20, fill=NA) #Media movil a 20
mediasaccion1.5<-rollmean(accion1$Close[1:100], k=5, fill=NA) #Media movil a 5

plot(accion1$Date[1:100],accion1$Close[1:100],"l",
     ylab="Cotizacion",xlab="Fecha",main="Cotización",lwd=2)
lines(accion1$Date[1:length(mediasaccion1.20)],mediasaccion1.20,lwd=1,col="red")
lines(accion1$Date[1:length(mediasaccion1.5)],mediasaccion1.5,lwd=1,col="blue")
legend(legend=c("Cotizacion","Media k = 5", "Media k = 20"),
       fill=c("black", "blue", "red"), x = "bottomright")

#Medias moviles con chartSeries() #addSMA 

getSymbols("IBE.MC",src="yahoo",from="2010-12-01", 
           to="2019-12-01")%>%get() #src="fuente" #con rango de fechas

chartSeries(IBE.MC,TA="addSMA()",
            subset= '2018-01::2018-06')
addSMA(n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "brown")

chartSeries(IBE.MC, 
            type = c("auto", "matchsticks"), 
            show.grid = TRUE,
            subset= '2018-01::2018-06',
            theme='white',
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addSMA(n=20,col = 'red'),addSMA(n=5,col = 'blue')))

#Tabla con varias medias moviles simples

SMAs <- accion1[1:300,] %>%
  select(Date, srate = Close) %>%
  mutate(srate_ma01 = rollmean(srate, k = 5, fill = NA),
         srate_ma02 = rollmean(srate, k = 25, fill = NA),
         srate_ma03 = rollmean(srate, k = 50, fill = NA),
         srate_ma05 = rollmean(srate, k = 100, fill = NA),
         srate_ma10 = rollmean(srate, k = 150, fill = NA))

SMAs

#Grafico con ggplot()

SMAs %>%
  gather(metric, value, srate:srate_ma10) %>%
  ggplot(aes(Date, value, color = metric)) +
  geom_line()

#Media movil exponencial

#Mediante una rutina
myEMA <- function (price,n){
  ema <- c()
  ema[1:(n-1)] <- NA
  ema[n]<- mean(price[1:n])
  beta <- 2/(n+1)
  for (i in (n+1):length(price)){
    ema[i]<-beta * price[i] + 
      (1-beta) * ema[i-1]
  }
  ema <- reclass(ema,price)
  return(ema)
}

#n=k

EMA.5<-myEMA(accion1$Close[1:100],5)
EMA.15<-myEMA(accion1$Close[1:100],15)

plot(accion1$Date[1:100],accion1$Close[1:100],"l",
     ylab="Cotizacion",xlab="Fecha",main="Cotización",lwd=2)
lines(accion1$Date[1:length(EMA.5)],EMA.5,lwd=1,col="red")
lines(accion1$Date[1:length(EMA.15)],EMA.15,lwd=1,col="blue")
legend(legend=c("Cotizacion","Media k = 5", "Media k = 15"),
       fill=c("black", "red", "blue"), x = "bottomright")

#Mediante EMA()

emak15<-EMA(accion1$Close, n=15)

plot(accion1$Date[1:100],accion1$Close[1:100],"l",
     ylab="Cotizacion",xlab="Fecha",main="Cotización",lwd=2)
lines(accion1$Date[1:length(emak15)],emak15,lwd=1,col="red")

#Media Exponencial con chartSeries()

chartSeries(IBE.MC,
            subset='2018-01::2018-06',
            theme=chartTheme('white'))
addEMA(n=30,on=1,col = "blue")

#Soportes y resistencias

#Rutina para detectar en el gráfico

detectSupportResistance <- function(timeSeries, tolerance=0.01, nChunks=10, nPoints=3, plotChart=TRUE)
{
  #detect maximums and minimums
  N = length(timeSeries)
  stp = floor(N / nChunks)
  minz = array(0.0, dim=nChunks)
  whichMinz = array(0, dim=nChunks)
  maxz = array(0.0, dim=nChunks)
  whichMaxz = array(0, dim=nChunks)
  for(j in 1:(nChunks-1)) 
  {
    lft = (j-1)*stp + 1  #left and right elements of each chunk
    rght = j*stp
    whichMinz[j] = which.min(timeSeries[lft:rght]) + lft
    minz[j] = min(timeSeries[lft:rght])
    whichMaxz[j] = which.max(timeSeries[lft:rght]) + lft
    maxz[j] = max(timeSeries[lft:rght])
  }   
  #last chunk
  lft = j*stp + 1  #left and right elements of each chunk
  rght = N
  whichMinz[nChunks] = which.min(timeSeries[lft:rght]) + lft
  minz[nChunks] = min(timeSeries[lft:rght])
  whichMaxz[nChunks] = which.max(timeSeries[lft:rght]) + lft
  maxz[nChunks] = max(timeSeries[lft:rght])
  
  result = list()
  result[["minima"]] = NULL
  result[["minimaAt"]] = NULL
  result[["maxima"]] = NULL
  result[["maximaAt"]] = NULL
  span = tolerance*(max(maxz) - min(minz))
  
  rang = order(minz)[1:nPoints]
  if((minz[rang[nPoints]] - minz[rang[1]]) <= span)
  {
    result[["minima"]] = minz[rang[1:nPoints]]
    result[["minimaAt"]] = whichMinz[rang[1:nPoints]]
  } 
  
  rang = order(maxz, decreasing = TRUE)[1:nPoints]
  if((maxz[rang[1]] - maxz[rang[nPoints]]) <= span)
  {
    result[["maxima"]] = maxz[rang[1:nPoints]]
    result[["maximaAt"]] = whichMaxz[rang[1:nPoints]]
  } 
  
  if(plotChart)
  {
    ts.plot(timeSeries)
    points(whichMinz, minz, col="blue")
    points(whichMaxz, maxz, col="red")
    if(!is.null(result[["minima"]])  &&  !is.null(result[["minimaAt"]]))
      abline(lm(result[["minima"]] ~  result[["minimaAt"]]))
    if(!is.null(result[["maxima"]])  &&  !is.null(result[["maximaAt"]]))
      abline(lm(result[["maxima"]] ~  result[["maximaAt"]]))
  } 
  
  return(result)    
}

#tolerance. Las R y Sp no son exactos, es un error.
#nChunks. Chunks en que se divide la serie y se calculan maximos y minimos


detectSupportResistance(accion1$Close,tolerance=0.01, nChunks=10,
                        nPoints=3, plotChart=TRUE)

#Rutina para obtener resistencia y soporte general

PPRSI<-function(P){
  
  Maximo<-P[which.max(P)]
  Minimo<-P[which.min(P)]
  Close<-P[length(P)]
  PP<-(Maximo+Minimo+Close)/3
  RI<-(2*PP)-Minimo
  RII<-PP+(Maximo-Minimo)
  SI<-(2*PP)-Maximo
  SII<-PP-(Maximo-Minimo)
  if (SII < 0){SII=0}
  lista<-list(RI,RII,SI,SII)
  names(lista)<-c("Primera Resistencia","Segunda Resistencia","Primer Soporte",
                  "Segundo Soporte")
  return(lista)
}
#Se usa en un intervalo periodico para el siguiente

pivotes<-PPRSI(accion1$Close[1:7])
pivotes

#Primera resistencia
RI<-pivotes[[1]]
RII<-pivotes[[2]]
SI<-pivotes[[3]]
SII<-pivotes[[4]]

#Usando get symbols

AENA<-getSymbols("AENA.MC",src="yahoo",from="2021-01-20", 
           to="2021-02-01")%>%get()
AENA.C<-AENA$AENA.MC.Close
AENA.C<-as.numeric(AENA.C)
PPRSI(AENA.C)

#Bucle para la obtención de resistencias y soportes
#Requiere objeto OHLC

PPRSII<-function(H,L,C){
  
  t<-length(H)
  PP<-c(1:t)
  RI<-c(1:t)
  RII<-c(1:t)
  SI<-c(1:t)
  SII<-c(1:t)
  
  for (i in 1:t) {
    
    PP[i]<-(H[i]+L[i]+C[i])/3
    
  }
  
  for (i in 1:t) {
    
    RI[i]<-(2*PP[i])-L[i]
    
  }
  
  for (i in 1:t) {
    
    RII[i]<-PP[i]+(H[i]-L[i])
    
  }
  
  for (i in 1:t) {
    
    SI[i]<-(2*PP[i])-H[i]
    
  }
  
  for (i in 1:t) {
    
    SII[i]<-PP[i]-(H[i]-L[i])
    
  }
  
  lista<-list(SI,SII,RI,RII)
  names(lista)<-c("S1","S2","R1","R2")
  
  return(lista)
}

SPYR<-PPRSII(H=accion1$High[1:45],L=accion1$Low[1:45],C=accion1$Close[1:45])
SPYR[[1]]
plot(accion1$Date[1:45],accion1$Close[1:45],xlab="Fecha",ylab="Precio",
     main="Soportes y Resistencias","l")
lines(accion1$Date[1:45],SPYR[[1]],col="Red")
lines(accion1$Date[1:45],SPYR[[3]],col="Blue")

#Relative Strength Index (RSI)

RSI(price=accion1$Close, n = 27) #RSI(precio,n)

plot(accion1$Date,RSI(price=accion1$Close, n = 27),"l",col="red",
     xlab="Fecha",ylab="RSI",main="Relative Strength Index")

plot(accion1$Date,accion1$Close,"l",col="blue",
     xlab="Fecha",ylab="Cotizacion",main="Cotizacion Accion 1")

#Con chartSeries():
chartSeries(GS, 
            type = c("auto", "matchsticks"), 
            show.grid = TRUE,
            subset='first 30 weeks',
            theme='white',
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addRSI(),addSMA(n=10,col = 'red')))

#Stochastic Momentum Index (SMI)

A1SMI<-as.data.frame(stoch(accion1$Close, nfastK=3, nFastD = 3,
                           nSlowD = 5, nsignal=9))


chartSeries(GS, 
            type = c("auto", "matchsticks"), 
            show.grid = TRUE,
            subset='first 30 weeks',
            theme='white',
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addSMI(n=13,slow=25,fast=2,signal=9,ma.type="EMA"),
                 addSMA(n=10,col = 'red')))

                                
#MACD

#Mediante EMA()
MACD<-EMA(accion1$Close[1:200], n=12)-EMA(accion1$Close[1:200], n=26)
SEN<-EMA(MACD, n=9)
DIF<-MACD-SEN

plot(accion1$Date[1:200],DIF,"h",col="blue",xlab="Fecha",ylab="MACD",main="MACD")
lines(accion1$Date[1:200],MACD,"l",col="blue",lwd=2)
lines(accion1$Date[1:200],SEN,col="red")
legend(legend=c("MACD", "Señal"),
       fill=c("blue", "red"), x = "bottomright")

#Mediante MACD()
MACD(accion1$Close[1:200], nFast = 12, nSlow = 26, nSig = 9)
accion1MACD<-as.data.frame(MACD(accion1$Close[1:200], nFast = 12, nSlow = 26, nSig = 9))
DIF2<-accion1MACD$macd-accion1MACD$signal

plot(accion1$Date[1:200],DIF2,"h",col="blue",xlab="Fecha",ylab="MACD",main="MACD")
lines(accion1$Date[1:200],accion1MACD$macd,"l",col="blue",lwd=2)
lines(accion1$Date[1:200],accion1MACD$signal,col="red")
legend(legend=c("MACD", "Señal"),
       fill=c("blue", "red"), x = "bottomright")

#Mediante chartSeries

chartSeries(GS, 
            type = c("auto", "matchsticks"), 
            show.grid = TRUE,
            subset='first 30 weeks',
            theme='white',
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addMACD(),
                 addSMA(n=12,col = 'red')))

#ADX

ADX(GS,n=14)
plot(ADX(GS,n=14))

#Con chartSeries
chartSeries(GS, 
            type = c("auto", "matchsticks"), 
            show.grid = TRUE,
            subset='first 30 weeks',
            theme='white',
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addADX(n=14),
                 addSMA(n=12,col = 'red')))

#ATR

ATR(GS,n=14)
ATRGS<-ATR(GS,n=14)
plot(ATRGS)

#Con chartSeries

chartSeries(GS, 
            type = c("auto", "matchsticks"), 
            show.grid = TRUE,
            subset='first 30 weeks',
            theme='white',
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addATR(n=14),
                 addSMA(n=12,col = 'red')))

#CCI

CCI(accion1$Close,n=14,c=0.015)

plot(accion1$Date[1:200],CCI(accion1$Close[1:200],n=14,c=0.015),"l",xlab="Fecha",
     ylab="CCI",main="CCI")

#Con chartSeries

chartSeries(GS, 
            type = c("auto", "matchsticks"), 
            show.grid = TRUE,
            subset='first 30 weeks',
            theme='white',
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addCCI(n=14, c=0.015),
                 addSMA(n=12,col = 'red')))

#Ultimate oscillator

UOGS<-ultimateOscillator(GS, n = c(7, 14, 28),
                   wts = c(4, 2, 1))

plot(UOGS,col="blue")

#ROC

ROC(GS,n=5)
plot(ROC(GS,n=5))

#Con chartSeries

chartSeries(GS, 
            type = c("auto", "matchsticks"), 
            show.grid = TRUE,
            subset='first 30 weeks',
            theme='white',
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addROC(n=5),
                 addSMA(n=12,col = 'red')))

#Bollinger Bands

chartSeries(GS, 
            type = c("auto", "matchsticks"), 
            show.grid = TRUE,
            subset='first 30 weeks',
            theme='white',
            major.ticks='auto', minor.ticks=TRUE,
            multi.col = FALSE,
            TA=c(addBBands(n=20)))

