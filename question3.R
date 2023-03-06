#Se utilizará el conjunto de datos Auto de la librería ISLR, el cual contiene información sobre el kilometraje de gas, 
#caballos de fuerza, y otra información para 392 vehículos 

#Evaluar la presencia de valores atípicos multivariados con la distancia de Mahalanobis al cuadrado. Justificar el 
#análisis usando técnicas de visualización como la gráfica Q-Q y la gráfica de frecuencias acumuladas usando la distribución 
#chi-cuadrado  

# Limpieza
rm(list = ls())

#Librerias
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(xtable)

#install.packages("ISLR")
library(ISLR)
# Se desea predecir mpg usando horsepower
head(Auto)
str(Auto)
summary(Auto)

class(Auto$name)
