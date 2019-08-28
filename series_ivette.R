library(forecast)
library(tseries)
library(ggfortify)

setwd("C:/Users/DELL/Documents/UVG/VIII_Semestre/Data Science/DSLaboratorio3")

# --------------------
# Lectura de los datos
# --------------------
datos <- read.csv("datosImp.csv")

summary(datos)

class(datos)


#Transformamos los datos en una serie temporal 
datos2ts<-ts(datos$Diesel, start = c(2001,1), frequency = 12)
print(datos2ts)

#Trazamos la serie de tiempo datos2ts
autoplot(datos2ts, ts.colour = "blue", ts.linetype = "dashed", xlab = "Time", ylab = "Diesel",
         title = "Diesel behavior")
#Verifiacion de varianza
autoplot(acf(datos2ts, na.action = na.pass, plot = FALSE))
autoplot(stl(datos2ts, s.window = "periodic"), ts.colour = "blue")
plot(decompose(datos2ts))

#MODELO ARIMA
ndiffs(datos2ts)
nsdiffs(datos2ts)

diff.datos2ts<-autoplot(diff(datos2ts), ts.linetype = "dashed", ts.colour = "darkmagenta")
diff.datos2ts
