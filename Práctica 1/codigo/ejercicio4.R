# Autora: Gema Correa Fernández

#############################################################################
################################ PRÁCTICA 1 #################################
#############################################################################

setwd("~/Dropbox/Máster TECI/Bloque 3/STMP/PRÁCTICAS/Práctica 1")

# Importamos las librerías que necesitamos
library(forecast) # Cargamos la libreria forecast
library(gdata) # Cargamos para leer xlsx
library(tseries)
library(lmtest)
library(MLmetrics) # Para usar la función mape
library(readxl) # Para leer xlsx
library(ggplot2) # Para hacer gráficos 
library(tsoutliers) # Para detección automática de Outliers


################################ EJERCICIO 4 #################################

# Ajusta un modelo a los datos de la serie del ejercicio 4 para el periodo 
# 2002-2007 y a continuación calcula, para ese modelo, el error porcentual 
# absoluto medio (MAPE) cometido al predecir la serie para los doce meses del 
# año 2008. Repite este ejercicio para el periodo 2002-2018 y calcula el MAPE 
# cometido al predecir la serie para los meses enero a octubre del año 2019

# Usar la función mape(predicción, valor_obtenido) - mape(y_pred, y_true)

# ------------------------------------------
# Parte 1: Periodo 2002-2007 y predecir 2008
# ------------------------------------------

# Leemos los datos
# ejercicio4.csv <- read.csv("datos/Ejercicio-4.csv", sep=";")
ejercicio4.xlsx <- read_excel("datos/Ejercicio-4.xlsx", col_names=FALSE)

# Como están desordenados de 2019 a 2002, les damos la vuelta
datos <- ejercicio4.xlsx[with(ejercicio4.xlsx, order(ejercicio4.xlsx$...1)), ]

# Convertimos los datos a serie temporal
data <- ts(datos$...2, frequency = 12, start=c(2002, 1), end = c(2007, 12)); data

# Como se trata de datos mensuales, lo dividimos por el numero de dias por mes
data <- data/monthdays(data)
plot.ts(data, main = "Periodo de 2002 a 2007")


# Realizamos un modelo de selección automática (PROBAR CON MODELO MANUAL)
mod_20022007 <- auto.arima(data, lambda = BoxCox.lambda(data)); # Poner que el lambda es "1"
mod_20022007 # BoxCox.lambda(data) = -0.9999242  - ARIMA(0,0,1)(0,1,1)[12]
# lambda1 <- BoxCox.lambda(data, method = "guerrero", lower = -1, upper = 1)
# mod_20022007 <- auto.arima(data, lambda = 1); # Poner que el lambda es "1" ## CAMBIARLO Y EXPLICAR
# Al ser -0.999959


## Realizamos la PREDICCIÓN PARA 2008
## ..................................

# Calculamos la predicción para los 12 meses siguientes (todo 2008) 
y_pred_2008 <- forecast(mod_20022007, h = 12)
plot(forecast(mod_20022007, h = 12))
y_pred_2008 <- y_pred_2008$mean

# Guardamos los valores reales de 2008
# Hay que crearse otro fichero con los datos
ejercicio42008.xlsx <- read_excel("datos/Ejercicio-4-2008.xlsx", col_names=FALSE)
datos <- ejercicio42008.xlsx[with(ejercicio42008.xlsx, order(ejercicio42008.xlsx$...1)), ]
data_2008 <- ts(datos$...2, frequency = 12, start=c(2008, 1), end = c(2008, 12)); 
y_real_2008 <- data_2008/monthdays(data_2008)

# Calculamos el error porcentual absoluto medio (MAPE) 
# cometido al predecir la serie para los doce meses del 
# año 2008
MAPE(y_pred_2008, y_real_2008) # 0.1458807

# Visualizamos en gráficas la comparativa
ejercicio4.xlsx <- read_excel("datos/Ejercicio-4.xlsx", col_names=FALSE)
datos <- ejercicio4.xlsx[with(ejercicio4.xlsx, order(ejercicio4.xlsx$...1)), ]
data <- ts(datos$...2, frequency = 12, start=c(2002, 1), end = c(2008, 12))
data <- data/monthdays(data)
p <- ggplot(data, aes(x, y), colour="blue") + geom_point() + geom_line(colour="blue") + 
            xlab("fecha") + ylab("valor") +
            ggtitle("Comparativa predicción 2008", subtitle = "rojo predicción y azul real")+ guides(shape=FALSE)
p <- p + geom_line(y_pred_2008, mapping=aes(x, y), colour="red") + geom_point()
p


# ------------------------------------------
# Parte 2: Periodo 2002-2018 y predecir 2019
# ------------------------------------------

# Leemos los datos
# ejercicio4.csv <- read.csv("datos/Ejercicio-4.csv", sep=";")
ejercicio4.xlsx <- read_excel("datos/Ejercicio-4.xlsx", col_names=FALSE)

# Como están desordenados de 2019 a 2002, les damos la vuelta
datos <- ejercicio4.xlsx[with(ejercicio4.xlsx, order(ejercicio4.xlsx$...1)), ]

# Convertimos los datos a serie temporal
data <- ts(datos$...2, frequency = 12, start=c(2002, 1), end = c(2018, 12)); data

# Como se trata de datos mensuales, lo dividimos por el numero de dias por mes
data <- data/monthdays(data)
plot.ts(data, main = "Periodo de 2002 a 2018")


# Realizamos un modelo de selección automática (PROBAR CON MODELO MANUAL)
mod_20022018 <- auto.arima(data, lambda = BoxCox.lambda(data)); 
mod_20022018 # BoxCox.lambda(data) = -0.9999242 - ARIMA(2,0,1)(0,1,1)[12] 

## Realizamos la PREDICCIÓN PARA 2019
## ..................................

# Calculamos la predicción para los 10 meses siguientes (todo 2019) 
y_pred_2019 <- forecast(mod_20022018, h = 10)
plot(forecast(mod_20022018, h = 10))
y_pred_2019 <- y_pred_2019$mean

# Guardamos los valores reales de 2019
ejercicio42019.xlsx <- read_excel("datos/Ejercicio-4-2019.xlsx", col_names=FALSE)
datos <- ejercicio42019.xlsx[with(ejercicio42019.xlsx, order(ejercicio42019.xlsx$...1)), ]
data_2019 <- ts(datos$...2, frequency = 12, start=c(2019, 1), end = c(2019, 10));
y_real_2019 <- data_2019/monthdays(data_2019)

# Calculamos el error porcentual absoluto medio (MAPE) 
# cometido al predecir la serie para los doce meses del 
# año 2008
MAPE(y_pred_2019, y_real_2019) # 0.05072626

# Visualizamos en gráficas la comparativa
ejercicio4.xlsx <- read_excel("datos/Ejercicio-4.xlsx", col_names=FALSE)
datos <- ejercicio4.xlsx[with(ejercicio4.xlsx, order(ejercicio4.xlsx$...1)), ]
data <- ts(datos$...2, frequency = 12, start=c(2002, 1), end = c(2019, 10))
data <- data/monthdays(data)
p <- ggplot(data, aes(x, y), colour="blue") + geom_point() + geom_line(colour="blue") + 
  xlab("fecha") + ylab("valor") +
  ggtitle("Comparativa predicción 2019", subtitle = "A subtitle")+ guides(shape=FALSE)
p <- p + geom_line(y_pred_2019, mapping=aes(x, y), colour="red") + geom_point()
p


# Supongo que predice mejor en 2008, ya que se veía notando la burbuja inmobiliaria y demás
# en la que estábamos, y a partir de los datos obtenidos actualmente nos movemos en un mercado
# que esta menos estabilizado.
# Como supongo que los datos tienes que ver con la economía, puedo decir:
# https://www.ine.es/prensa/pib_tabla_cne.htm
# https://www.ine.es/consul/serie.do?s=CNTR3180&c=2&nult=100
