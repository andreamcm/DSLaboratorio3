#install.packages("forecast")
library(forecast)
#install.packages("tseries")
library(tseries)
#install.packages("ggfortify")
library(ggfortify)
#install.packages("fUnitRoots")
library(fUnitRoots)
#install.packages("lmtest")
library(lmtest)
install.packages("FitAR")
#library(FitAR)
library(ggplot2)

setwd("~/2019/UVG/Segundo Semestre/DataScience/Laboratorios/Laboratorio3/DSLaboratorio3")
setwd("C:/Users/DELL/Documents/UVG/VIII_Semestre/Data Science/DSLaboratorio3")

# --------------------
# Lectura de los datos
# --------------------
datos <- read.csv("datosImp.csv")

summary(datos)

class(datos)

########################################################
############ DIESEL   ##################################
########################################################

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

decomp = stl(datos_diesel, s.window="periodic")
var <- seasadj(decomp)
plot(decomp)
count_d1 = diff(var, differences = 1)
plot(count_d1)

#Pruebas
adf.test(count_d1, alternative="stationary", k=0)
pp.test(count_d1, alternative="stationary")

#Nuevo grafico
plot(decompose(count_d1))

#MODELO ARIMA
ndiffs(count_d1)
nsdiffs(count_d1)

fitARIMAdl <- arima(count_d1, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
coeftest(fitARIMAdl)
confint(fitARIMAdl)

acf(fitARIMAdl$residuals,lag.max=140)
boxresult=LjungBoxTest (fitARIMAdl$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMAdl$residuals)
qqline(fitARIMAdl$residuals)

auto.arima(count_d1, trace=TRUE)

modeloarimadiesel<-auto.arima(count_d1, seasonal=FALSE)
modeloarimadiesel
tsdisplay(residuals(modeloarimadiesel), lag.max=10, main='(3,1,1) Diesel Model Residuals')

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



########################################################
################## GASOLINA SUPER   ####################
########################################################

# Frecuencia
str(datos)
datos_super<-ts(datos$GasSuperior, start = c(2001,1), frequency = 12)
print(datos_super)


# Se traza la serie de tiempo para la gasolina superior
autoplot(datos_super, ts.colour = "blue", ts.linetype = "dashed", xlab = "Time", ylab = "Diesel",
         title = "Superior Gas behavior")


# Se descomponen los datos
plot(decompose(datos_regular))
autoplot(stl(datos_regular, s.window = "periodic"), ts.colour = "blue")


# Autocorrelación
acf(datos_super)


# Varianza
autoplot(acf(datos_super, plot = FALSE))


# Pruebas
adf.test(diff(log(datos_super)), alternative="stationary", k=0)
pp.test(diff(log(datos_super), alternative="stationary"))


# Gráfico nuevo
plot(decompose(diff(log(datos_super))))


# Modelo arima
ndiffs(datos_super)
nsdiffs(datos_super)

fitARIMAsup <- arima(datos_super, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
coeftest(fitARIMAsup)
confint(fitARIMAsup)

acf(fitARIMAsup$residuals,lag.max=140)
boxresult=LjungBoxTest (fitARIMAsup$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMAsup$residuals)
qqline(fitARIMAsup$residuals)

auto.arima(datos_super, trace=TRUE)

# Prediccion 2020
predict(fitARIMAsup,n.ahead = 5)
futurValdlSup <- forecast(fitARIMAsup,h=10, level=c(99.5))
plot(futurValdlSup)






