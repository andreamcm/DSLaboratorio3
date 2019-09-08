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
#install.packages("FitAR")
library(FitAR)
library(ggplot2)

#En Box.test(x, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)  si p > 0.05 esta bien
#RSS se le pasa el forecast, cercano a 0 es mejor (CARLOS 9)

setwd("~/2019/UVG/Segundo Semestre/DataScience/Laboratorios/Laboratorio3/DSLaboratorio3")
setwd("C:/Users/DELL/Documents/UVG/VIII_Semestre/Data Science/DSLaboratorio3")

# --------------------
# Lectura de los datos
# --------------------
datos <- read.csv("datosImp.csv")
str(datos)
datos$Anio
datos2017 <- datos[ datos$Anio != 2018 & datos$Anio != 2019, ]
datos2018_19 <- datos[ datos$Anio == 2018, ]
datos2019 <- datos[ datos$Anio == 2019, ]
datos2018_19 <- rbind(datos2018_19, datos2019)

summary(datos)

class(datos)

########################################################
############ DIESEL   ##################################
########################################################

#Transformar NA´s en 0´s
datos2017$Diesel[is.na(datos2017$Diesel)] <- 0
datos2017$DieselLS[is.na(datos2017$DieselLS)] <- 0
datos2017$DieselULS[is.na(datos2017$DieselULS)] <- 0

#Sumar columnas de tipos de Diesel en una sola 
datos2017$all_diesel <- rowSums( datos2017[,9:11] )

#Transformamos los datos en una serie temporal 
datos_diesel<-ts(datos2017$all_diesel, start = c(2001,1), frequency = 12)
print(datos_diesel)

#Trazamos la serie de tiempo datos_diesel
autoplot(datos_diesel, ts.colour = "blue", ts.linetype = "dashed", xlab = "Time", ylab = "Diesel",
         main = "Diesel behavior")

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
plot(count_d1)

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
tsdisplay(residuals(modeloarimadiesel), lag.max=10, main='(1,1,1)(1,0,0) Diesel Model Residuals')

#Prediccion 
predict(fitARIMAdl,n.ahead = 5)
futurValdl <- forecast(fitARIMAdl,h=35, level=c(95))
plot(futurValdl)


########################################################
################## GASOLINA REGULAR   ####################
########################################################
#Transformamos los datos en una serie temporal 
datos_regular<-ts(datos2017$GasRegular, start = c(2001,1), frequency = 12)
print(datos_regular)

#Trazamos la serie de tiempo datos_diesel
autoplot(datos_regular, ts.colour = "blue", ts.linetype = "dashed", xlab = "Time", ylab = "Regular Gas",
         main = "Regular Gasoline behavior")

#Descomposicion
plot(decompose(datos_regular))
autoplot(stl(datos_regular, s.window = "periodic"), ts.colour = "blue")

#Autocorrelacion 
acf(datos_regular, main = "Autocorrelacion de Gasolina Regular")

#Verifiacion de varianza
autoplot(acf(datos_regular, plot = FALSE))

decomp = stl(datos_regular, s.window="periodic")
var2 <- seasadj(decomp)
plot(decomp)
count_d2 = diff(var2, differences = 1)
plot(count_d2)

#Pruebas
adf.test(count_d2, alternative="stationary", k=0)
pp.test(count_d2, alternative="stationary")

#Nuevo grafico
plot(decompose(count_d2))
plot(count_d2)

#MODELO ARIMA
ndiffs(count_d2)
nsdiffs(count_d2)

fitARIMAReg <- arima(count_d2, order=c(1,0,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
coeftest(fitARIMAReg)
confint(fitARIMAReg)

acf(fitARIMAReg$residuals,lag.max=140)
boxresult=LjungBoxTest (fitARIMAReg$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMAReg$residuals)
qqline(fitARIMAReg$residuals)

auto.arima(count_d2, trace=TRUE)

modeloarimaregular<-auto.arima(count_d2, seasonal=FALSE)
modeloarimaregular
tsdisplay(residuals(modeloarimaregular), lag.max=10, main='(1,0,1) Regular Gasoline Model Residuals')

#Prediccion 
predict(fitARIMAReg,n.ahead = 5)
futurValReg2 <- forecast(fitARIMAReg,h=35, level=c(95))
plot(futurValReg2)


########################################################
################## GASOLINA SUPER   ####################
########################################################

# Frecuencia
str(datos2017)
datos_super<-ts(datos2017$GasSuperior, start = c(2001,1), frequency = 12)
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




