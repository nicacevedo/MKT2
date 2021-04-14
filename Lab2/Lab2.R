# Script para webear

# Librerías
library(readr)     #Para leer CSV
library(glmnet)    #Para ajustar modelo lineal
library(corrplot)  #Para realizar correlogramas
library(dplyr)     #Para manipulación de datos
library(ggplot2)   #Para gráficos
library(caret)     #Para validar y entrenar los modelos
library(knitr) 

# Seteos
options(digits = 3)  #Dígitos después del punto para observar (décimas, centésimas,...)
set.seed(12345)      #Fijar semilla de aleatoriedad

# Base
Casas <- read.csv("./Lab2/Casas.csv")
kable(Casas[1:5,1:9]) #kable crea una tabla head(Casas) sirve para visualiar la data

for (col in colnames(Casas)){
  hist(Casas[[col]])
}

