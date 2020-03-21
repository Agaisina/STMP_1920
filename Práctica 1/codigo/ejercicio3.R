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

################################ EJERCICIO 3 #################################

# Elige el mejor modelo que se ajuste a la serie de datos del ejercicio 3 
# (no es necesario justificar el modelo elegido) y a continuación busca 2
# outliers. Sabiendo que los datos se corresponden con el '́Indices de Comercio
# al por Menor' proporciona la siguiente información

# Leemos los datos
ejercicio3.xlsx <- read_excel("datos/Ejercicio-3.xlsx", col_names=FALSE)

# En los datos tenemos información de todos los meses desde el año 2000 al 
# 2019 (falta el valor del noviembre y diciembre para 2019)

# Como están desordenados de 2019 a 2000, les damos la vuelta
datos <- ejercicio3.xlsx[with(ejercicio3.xlsx, order(ejercicio3.xlsx$...1)), ]

# Guardamos los datos en un objeto serie temporal y nos quedamos con la 
# segunda columna que es la contiene los valores de la serie
data <- ts(datos$...2, frequency = 12, start=c(2000, 1), end = c(2019, 11)); data

# Como se trata de datos mensuales, lo dividimos por el numero de dias por mes
data <- data/monthdays(data)
plot.ts(data, main = "Periodo de 2000 a 2019")


# -----------------
# PRIMERA ITERACIÓN
# -----------------

## PASO 1 - Ajustar un modelo ARIMA (o SARIMA) a los datos
## .......................................................

# Realizamos un modelo de selección automática (PROBAR CON MODELO MANUAL)
modelo1 <- auto.arima(data, lambda = BoxCox.lambda(data)); modelo1
#modelo1 <- auto.arima(data, lambda = "auto"); modelo1
# ARIMA(2,1,0)(0,1,2)[12] 
# BoxCox.lambda(data) = -0.01444658 


## PASO 2a - Identificar puntos atipicos en los residuos
## ....................................................

# Procedemos a obtener a una estimacion inicial robusta de la desviacion tipica 
# de los residuos. Necesitamos una estimacion robusta por la presencia de outlier.
# mad(res) es la mediana de las desviaciones a la mediana, ajustada para que 
# sea consistente con la desviacion tipica en poblaciones normales.
res1 <- residuals(modelo1)
sd.r1 <- mad(res1)
tsdisplay(res1)

# Definimos el intervalo de no atipicidad como: mean(res) +- Z_0.995 * sd.r
cotas <- qnorm(0.995) * sd.r1 * c(-1,1)
ts.plot(res1); 
abline(h=cotas, lty = 3);
title("Primera iteración para detectar outliers")

# Conjunto de puntos atipicos
which(res1 < cotas[1] | res1 > cotas[2]) # 36 59 99 102 107 108 123 148 152 153


## PASO 3a - Considerar el punto mas atipico 
## ........................................

# Probar a ajustar un modelo con un atipico aditivo (AO) en el punto más 
# extremo y otro con un cambio de nivel (LS) a partir del punto más extremo.
pos1 <- which.max(pmax(cotas[1]-res1, res1-cotas[2]))
pos1 # 153

# Calculamos los regresores para AO
x.AO1 <- rep(0, length(res1)); 
x.AO1[pos1] <- 1;

# Calculamos los regresores para LS
x.LS1 <- c(rep(0, pos1-1), rep(1, length(res1) - pos1 + 1));

# Probamos a añadir en el modelo en el nuevo valor
modelo1.AO <- Arima(y = data, order = c(2,1,0), seasonal = c(2,1,1), xreg = x.AO1)
modelo1.LS <- Arima(y = data, order = c(2,1,0), seasonal = c(2,1,1), xreg = x.LS1)


## PASO 4a - Comparar los nuevos modelos entre sí y con el modelo original
## ......................................................................

# Para comparar nos basamos en el criterio BIC y en el coeficiente
## ¿POR QUÉ SALEN NEGATIVOS LOS BICS?
BIC(modelo1); BIC(modelo1.AO); BIC(modelo1.LS) 
coeftest(modelo1); coeftest(modelo1.AO); coeftest(modelo1.LS) 
# Es significativo el LS, no el AO, ya que no tiene ninguna estrellita

# Detectamos un atipico aditivo (LS) en 153
# Validamos el modelo MA(1) con LS[153]
res1.LS <- residuals(modelo1.LS)
Box.test(res1.LS, type = c("Box-Pierce", "Ljung-Box"), lag = 30, fitdf = sum(modelo1.LS$LS[1:2]))
t.test(res1.LS)

# Los residuos son independientes y tienen media cero. 
# Por lo tanto el modelo modelo1.LS es correcto y mejor que el actual modelo1


## PASO 5a - Mantener el mejor de los modelos 
## .........................................

# Este paso, si se precisa es necesario volver al punto 2

# Añadimos el LS(153)
xreg1 <- matrix(x.LS1, ncol = 1);
colnames(xreg1) <- paste("LS", pos1, sep="")
modelo.primera <- Arima(y = data, order = c(2,1,0), seasonal = c(0,1,2), xreg = xreg1)
res1.LS <- residuals(modelo.primera)
tsdisplay(res1.LS)

# No se detecta la necesidad de alterar el modelo ARIMA

# Probar la metodologia Box-Jenkins 
shapiro.test(res1.LS) 
jarque.bera.test(res1.LS) 

# Sin embargo los residuos siguen teniendo una distribucion no normal 
# Volvemos al PASO 2


# -----------------
# SEGUNDA ITERACIÓN
# -----------------

## PASO 2b - Identificar puntos atipicos en los residuos
## .....................................................

# Procedemos a obtener a una estimacion inicial robusta de la desviacion 
# tipica de los residuos. Necesitamos una estimacion robusta por la presencia 
# de outlier. mad(res) es la mediana de las desviaciones a la mediana, ajustada 
# para que sea consistente con la desviacion tipica en poblaciones normales
sd.r2 <- mad(res1.LS)
tsdisplay(res1.LS)

# Definimos el intervalo de no atipicidad como: mean(res) +- Z_0.995 * sd.r
cotas <- qnorm(0.995) * sd.r2 * c(-1,1)
ts.plot(res1.LS);
abline(h=cotas, lty = 3);
title("Segunda iteración para detectar outliers")

# Conjunto de puntos atipicos
which(res1.LS < cotas[1] | res1.LS > cotas[2]) # 59 99 107 108 148 152


## PASO 3b - Considerar el punto mas atipico 
## .........................................

# Probar a ajustar un modelo con un atipicos aditivo (AO) en el punto mas 
# extremo y otro con un cambio de nivel (LS) a partir del punto más extremo

# Punto mas atipico
pos2 <- which.max(pmax(cotas[1]-res1.LS, res1.LS-cotas[2]))
pos2 # 99

# Calculamos los regresores para AO
x.AO2 <- rep(0, length(res1.LS)); x.AO2[pos2] <- 1;

# Calculamos los regresores para LS
x.LS2 <- c(rep(0, pos2-1), rep(1, length(res1.LS) - pos2 + 1));

# Probamos a añadir en el modelo en el nuevo valor
modelo2.AO <- Arima(y=data, order = c(2,1,0), seasonal = c(0,1,2), xreg = cbind(xreg1, x.AO2))
modelo2.LS <- Arima(y=data, order = c(2,1,0), seasonal = c(0,1,2), xreg = cbind(xreg1, x.LS2))


## PASO 4b - Comparar los nuevos modelos entre si y con el modelo original
## .......................................................................

# Para comparar nos basamos en el criterio BIC y en el coeficiente
BIC(modelo.primera); BIC(modelo2.AO); BIC(modelo2.LS); 
coeftest(modelo.primera); coeftest(modelo2.AO); coeftest(modelo2.LS);

# Vemos como para no sale significativo el 99 ni con LS ni con AO, 
# entonces pasamos al siguiente outlier sin incluir el 99 en el modelo


# TERCERA ITERACIÓN
# -----------------

# ¿Y si quitamos el outlier 99 y cogemos el siguiente? Ya que con el 99
# no nos da significativo ningún regresor en ningún coeficiente

## PASO 2c - Identificar puntos atipicos en los residuos
## .....................................................

# Procedemos a obtener a una estimacion inicial robusta de la desviacion 
# tipica de los residuos. Necesitamos una estimacion robusta por la presencia 
# de outlier. mad(res) es la mediana de las desviaciones a la mediana, ajustada 
# para que sea consistente con la desviacion tipica en poblaciones normales
sd.r2 <- mad(res1.LS)
tsdisplay(res1.LS)

# Definimos el intervalo de no atipicidad como: mean(res) +- Z_0.995 * sd.r
cotas <- qnorm(0.995) * sd.r2 * c(-1,1)
ts.plot(res1.LS);
abline(h=cotas, lty = 3);
title("Tercera iteración para detectar outliers")

# Conjunto de puntos atipicos
# Quitamos el 99 que nos está dando que no es significativo
atipicos.new <- res1.LS
atipicos.new[99] <- 0
which(atipicos.new < cotas[1] | atipicos.new > cotas[2]) # 59 107 108 148 152

  
## PASO 3c - Considerar el punto mas atípico 
## .........................................

# Probar a ajustar un modelo con un atipicos aditivo (AO) en el punto mas 
# extremo y otro con un cambio de nivel (LS) a partir del punto más extremo

# Punto mas atipico
# Quitamos el 99 que nos está dando que no es significativo
pos3 <- which.max(pmax(cotas[1]-atipicos.new, atipicos.new-cotas[2]))
pos3 # 107

# Calculamos los regresores para AO
x.AO3 <- rep(0, length(res1.LS)); x.AO3[pos3] <- 1;

# Calculamos los regresores para LS
x.LS3 <- c(rep(0, pos3-1), rep(1, length(res1.LS) - pos3 + 1));

# Probamos a añadir en el modelo en el nuevo valor
modelo3.AO <- Arima(y=data, order = c(2,1,0), seasonal = c(0,1,2), xreg = cbind(xreg1, x.AO3))
modelo3.LS <- Arima(y=data, order = c(2,1,0), seasonal = c(0,1,2), xreg = cbind(xreg1, x.LS3))


## PASO 4b - Comparar los nuevos modelos entre si y con el modelo original
## .......................................................................

# Para comparar nos basamos en el criterio BIC y en el coeficiente
BIC(modelo.primera); BIC(modelo3.AO); BIC(modelo3.LS)
coeftest(modelo.primera); coeftest(modelo3.AO); coeftest(modelo3.LS)

# Detectamos un atipico aditivo (LS) en 107
# Validamos el modelo MA(1) con LS[107]
res3.LS <- residuals(modelo3.LS)
#Box.test(res3.LS, type = c("Box-Pierce", "Ljung-Box"), lag = 30, fitdf = sum(modelo3.LS$LS[1:2]))
Box.test(res3.LS, type = "Ljung-Box", lag = 30, fitdf = sum(modelo3.LS$LS[1:2]))
t.test(res3.LS)

# Los residuos son independientes y tienen media cero. Por lo tanto el modelo 
# modelo3.LS es correcto y mejor que el actual modelo1.LS


## PASO 5b - Mantener el mejor de los modelos y mejorarlo usando la metodologia Box-Jenkins 
## ........................................................................................

xreg3 <- cbind(xreg1, x.LS3); 
colnames(xreg3)[ncol(xreg3)] <- paste("LS", pos3, sep="")
modelo.tercero <- Arima(y = data, order = c(2,1,0), seasonal = c(0,1,2), xreg = xreg3)
res3.LS <- residuals(modelo.tercero)
tsdisplay(res3.LS)

# No se detecta la necesidad de alterar el modelo ARIMA.
shapiro.test(res3.LS)
jarque.bera.test(res3.LS)

# Sin embargo los residuos siguen teniendo una distribución no normal
# Pero sólo se nos pide dos outliers

# p-value LS 153 : 0.0002731
# p-value LS 107 : 0.00001042


# .......................



# --------------------------------
# DETECCION AUTOMATICA DE OUTLIERS
# --------------------------------

modelo.tso <- tso(y = data, types = c("AO", "LS", "TC"), discard.method = "bottom-up", 
               tsmethod = "auto.arima",
               args.tsmethod = list(ic = "bic", d = 1, max.p = 2, max.q = 1, 
                                    seasonal = T, stepwise = F, parallel = T))
modelo.tso
plot(modelo.tso)

# El p-valor que se pide es el primer p-valor que se obtiene cuando incluyes 
# el outlier en el modelo (en el pantallazo que te mando a continuación, el 
# p-valor es el primero que te sale al incluir el 153. En tal caso es este valor, 
# antes de incluir ningún otro outlier, ya que como dices luego va cambiando su 
# valor. Cuando se calculan los outliers de forma automática no tenemos este p-valor.



# El Índice de Comercio al por Menor (ICM) tiene como objetivo conocer la 
# evolución de las ventas y el empleo en el sector del comercio minorista 
# en España y cumplir con las exigencias que establece EUROSTAT en el 
# estudio coyuntural relativo a este sector
# La información se recoge de una muestra de 12.500 empresas ubicadas en 
# todo el territorio nacional, de las que se obtienen datos por cuestionario, 
# teléfono, fax o web, de las ventas brutas mensuales y del número de ocupados 
# referido al último día de cada mes
# https://www.ine.es/prensa/icm_prensa.htm
# Tasa de paro: https://www.ine.es/jaxiT3/Datos.htm?t=4247
# Gráfico de comercios al por menor: comercios al por menor españa - Buscar con Google

# LS 153 2012:09 -0.1490 -4.110 -> 
# https://www.libertaddigital.com/economia/espana-destruira-en-marzo-de-2009-todo-el-empleo-creado-en-los-ultimos-18-meses-1276344372/
# 2008:1 el paro crece exponecialmente, la crisis econónica fue en 2008
# 2009:03 se estanca un poco el crecimiento del paro, pueden ser contrataciones para semana santa
# 2012:09 reforma laboral - https://www.wto.org/spanish/news_s/pres12_s/pr658_s.htm
# también tiene que ver con el PIB
# https://www.google.com/search?q=pib+per+capita+espa%C3%B1a&safe=active&client=safari&rls=en&sxsrf=ACYBGNSovOKzNJdXdMm9GkPqk4qzm0H1ng:1580581017876&source=lnms&tbm=isch&sa=X&ved=2ahUKEwjiru-Q-7DnAhXfAmMBHRZkCXcQ_AUoAXoECBMQAw&biw=1680&bih=884#imgdii=JPgs9j_HnUCriM:&imgrc=ywBSPE3P6uM_EM:
# http://www.rtve.es/noticias/20120302/claves-reforma-laboral-despido-mas-barato-nuevas-bonificaciones/502961.shtml



