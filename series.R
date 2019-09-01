library(forecast)
library(tseries)
library(ggfortify)
library(fUnitRoots)
library(lmtest)
library(FitAR)

setwd("C:/Users/DELL/Documents/UVG/VIII_Semestre/Data Science/DSLaboratorio3")

# --------------------
# Lectura de los datos
# --------------------
datos <- read.csv("datosImp.csv")

summary(datos)

class(datos)

#Transformar NA´s en 0´s
datos$Diesel[is.na(datos$Diesel)] <- 0
datos$DieselLS[is.na(datos$DieselLS)] <- 0
datos$DieselULS[is.na(datos$DieselULS)] <- 0

#Sumar columnas de tipos de Diesel en una sola 
datos$all_diesel <- rowSums( datos[,9:11] )

#Transformamos los datos en una serie temporal 
datos_diesel<-ts(datos$all_diesel, start = c(2001,1), frequency = 12)
print(datos_diesel)

#Trazamos la serie de tiempo datos_diesel
autoplot(datos_diesel, ts.colour = "blue", ts.linetype = "dashed", xlab = "Time", ylab = "Diesel",
         title = "Diesel behavior")

#Descomposicion
plot(decompose(datos_diesel))
autoplot(stl(datos_diesel, s.window = "periodic"), ts.colour = "blue")

#Autocorrelacion 
acf(datos_diesel)

#Verifiacion de varianza
autoplot(acf(datos_diesel, plot = FALSE))

#Pruebas
adf.test(diff(log(datos_diesel)), alternative="stationary", k=0)
pp.test(diff(log(datos_diesel), alternative="stationary"))

#Nuevo grafico
plot(decompose(diff(log(datos_diesel))))

#MODELO ARIMA
ndiffs(datos_diesel)
nsdiffs(datos_diesel)

fitARIMAdl <- arima(datos_diesel, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
coeftest(fitARIMAdl)
confint(fitARIMAdl)

acf(fitARIMAdl$residuals,lag.max=140)
boxresult=LjungBoxTest (fitARIMAdl$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMAdl$residuals)
qqline(fitARIMAdl$residuals)

auto.arima(datos_diesel, trace=TRUE)

#Prediccion 2020
predict(fitARIMAdl,n.ahead = 5)
futurValdl <- forecast(fitARIMAdl,h=10, level=c(99.5))
plot(futurValdl)

########################################################
############ GASOLINA REGULAR   ########################
########################################################

#Transformamos los datos en una serie temporal 
datos_regular<-ts(datos$GasRegular, start = c(2001,1), frequency = 12)
print(datos_regular)

#Trazamos la serie de tiempo datos_diesel
autoplot(datos_regular, ts.colour = "blue", ts.linetype = "dashed", xlab = "Time", ylab = "Diesel",
         title = "Regular Gas behavior")

#Descomposicion
plot(decompose(datos_regular))
autoplot(stl(datos_regular, s.window = "periodic"), ts.colour = "blue")

#Autocorrelacion 
acf(datos_regular)

#Verifiacion de varianza
autoplot(acf(datos_regular, plot = FALSE))

#Pruebas
adf.test(diff(log(datos_regular)), alternative="stationary", k=0)
pp.test(diff(log(datos_regular), alternative="stationary"))

#Nuevo grafico
plot(decompose(diff(log(datos_regular))))

#MODELO ARIMA
ndiffs(datos_regular)
nsdiffs(datos_regular)

fitARIMAreg <- arima(datos_regular, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
coeftest(fitARIMAreg)
confint(fitARIMAreg)

acf(fitARIMAreg$residuals,lag.max=140)
boxresult=LjungBoxTest (fitARIMAreg$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMAreg$residuals)
qqline(fitARIMAreg$residuals)

auto.arima(datos_regular, trace=TRUE)

#Prediccion 2020
predict(fitARIMAreg,n.ahead = 5)
futurValdl <- forecast(fitARIMAreg,h=10, level=c(99.5))
plot(futurValdl)
