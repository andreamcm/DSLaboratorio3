# Universidad del Valle de Guatemala
# Ivette Cardona - 16020
# Andrea María Cordón - 16076



# -----------------------------------------
# Set del directorio donde estan los datos
# -----------------------------------------
# Andrea
setwd("~/2019/UVG/Segundo Semestre/DataScience/Laboratorios/Laboratorio3/DSLaboratorio3")
# Ivette
setwd("C:/Users/DELL/Documents/UVG/VIII_Semestre/Data Science/DSLaboratorio2")



# --------------------
# Librerias a utilizar
# --------------------
# Utilizadas anteriormente
library(dplyr)
library(ggplot2)
library(corrplot)
library(gmodels)
library(Hmisc)
library(ggthemes)
library(class)
library(caret)
# Series de tiempo
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)
install.packages("fUnitRoots")
library(fUnitRoots)
install.packages("ggfortify")
library(ggfortify)


# --------------------
# Lectura de los datos
# --------------------
datos <- read.csv("datosImp.csv")


# ------------------------------------------
# Analisis exploratorio con nuevas variables
# ------------------------------------------
summary(datos)
str(datos)

# GLP
hist(datos$GLP, col="lightcyan", main="Histograma de GLP")
qqnorm(datos$GLP, main="GLP")
qqline(datos$GLP, col = "red")
ks.test(datos$GLP,rnorm(length(datos$GLP)))

# GasAviacion
hist(datos$GasAviacion, col="lightcyan", main="Histograma de GasAviacion")
qqnorm(datos$GasAviacion, main="GasAviacion")
qqline(datos$GasAviacion, col = "red")
ks.test(datos$GasAviacion,rnorm(length(datos$GasAviacion)))

# GasSuperior
hist(datos$GasSuperior, col="lightcyan", main="Histograma de GasSuperior")
qqnorm(datos$GasSuperior, main="GasSuperior")
qqline(datos$GasSuperior, col = "red")
ks.test(datos$GasSuperior,rnorm(length(datos$GasSuperior)))

# GasRegular
hist(datos$GasRegular, col="lightcyan", main="Histograma de GasRegular")
qqnorm(datos$GasRegular, main="GasRegular")
qqline(datos$GasRegular, col = "red")
ks.test(datos$GasRegular,rnorm(length(datos$GasRegular)))

# Kerosina
hist(datos$Kerosina, col="lightcyan", main="Histograma de Kerosina")
qqnorm(datos$Kerosina, main="Kerosina")
qqline(datos$Kerosina, col = "red")
ks.test(datos$Kerosina,rnorm(length(datos$Kerosina)))

# rTurboJet
hist(datos$rTurboJet, col="lightcyan", main="Histograma de rTurboJet")
qqnorm(datos$rTurboJet, main="rTurboJet")
qqline(datos$rTurboJet, col = "red")
ks.test(datos$rTurboJet,rnorm(length(datos$rTurboJet)))

# Diesel
hist(datos$Diesel, col="lightcyan", main="Histograma de Diesel")
qqnorm(datos$Diesel, main="Diesel")
qqline(datos$Diesel, col = "red")
ks.test(datos$Diesel,rnorm(length(datos$Diesel)))

# Bunker
hist(datos$Bunker, col="lightcyan", main="Histograma de Bunker")
qqnorm(datos$Bunker, main="Bunker")
qqline(datos$Bunker, col = "red")
ks.test(datos$Bunker,rnorm(length(datos$Bunker)))

# Asfalto
hist(datos$Asfalto, col="lightcyan", main="Histograma de Asfalto")
qqnorm(datos$Asfalto, main="Asfalto")
qqline(datos$Asfalto, col = "red")
ks.test(datos$Asfalto,rnorm(length(datos$Asfalto)))

# PetCoke
hist(datos$PetCoke, col="lightcyan", main="Histograma de PetCoke")
qqnorm(datos$PetCoke, main="PetCoke")
qqline(datos$PetCoke, col = "red")
ks.test(datos$PetCoke,rnorm(length(datos$PetCoke)))

# AceitesLub
hist(datos$AceitesLub, col="lightcyan", main="Histograma de AceitesLub")
qqnorm(datos$AceitesLub, main="AceitesLub")
qqline(datos$AceitesLub, col = "red")
ks.test(datos$AceitesLub,rnorm(length(datos$AceitesLub)))

# GrasasLub
hist(datos$GrasasLub, col="lightcyan", main="Histograma de GrasasLub")
qqnorm(datos$GrasasLub, main="GrasasLub")
qqline(datos$GrasasLub, col = "red")
ks.test(datos$GrasasLub,rnorm(length(datos$GrasasLub)))

# Solventes
hist(datos$Solventes, col="lightcyan", main="Histograma de Solventes")
qqnorm(datos$Solventes, main="Solventes")
qqline(datos$Solventes, col = "red")
ks.test(datos$Solventes,rnorm(length(datos$Solventes)))

# Naftas
hist(datos$Naftas, col="lightcyan", main="Histograma de Naftas")
qqnorm(datos$Naftas, main="Naftas")
qqline(datos$Naftas, col = "red")
ks.test(datos$Naftas,rnorm(length(datos$Naftas)))

# Ceras
hist(datos$Ceras, col="lightcyan", main="Histograma de Ceras")
qqnorm(datos$Ceras, main="Ceras")
qqline(datos$Ceras, col = "red")
ks.test(datos$Ceras,rnorm(length(datos$Ceras)))

# Butano
hist(datos$Butano, col="lightcyan", main="Histograma de Butano")
qqnorm(datos$Butano, main="Butano")
qqline(datos$Butano, col = "red")
ks.test(datos$Butano,rnorm(length(datos$Butano)))

# Orimulsión
hist(datos$Orimulsion, col="lightcyan", main="Histograma de Orimulsión")
qqnorm(datos$Orimulsion, main="Orimulsión")
qqline(datos$Orimulsion, col = "red")
ks.test(datos$Orimulsion,rnorm(length(datos$Orimulsion)))

# MezclasOleosas
hist(datos$MezclasOleosas, col="lightcyan", main="Histograma de MezclasOleosas")
qqnorm(datos$MezclasOleosas, main="MezclasOleosas")
qqline(datos$MezclasOleosas, col = "red")
ks.test(datos$MezclasOleosas,rnorm(length(datos$MezclasOleosas)))






















