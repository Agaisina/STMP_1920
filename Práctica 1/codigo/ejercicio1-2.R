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
library(caschrono)

################################ EJERCICIO 1 #################################

# Leemos los datos - ambos son el mismo fichero (cuidado con la 1ª linea)
# ejercicio1.csv <- read.csv("datos/Ej_1_Serie_1.csv", sep=";") # hay un NA 
# ejercicio1.xlsx <- read.xls("datos/Ej_1_Serie_1.xlsx", header = FALSE)
ejercicio1.xlsx <- read_excel("datos/Ej_1_Serie_1.xlsx", col_names=FALSE)


# QUEREMOS AJUSTAR UN MODELO ARIMA
# --------------------------------

# Como vamos a usar un XLSX es importante saber que los datos están de 2019 
# a 2002, por tanto nos interesa darles la vuelta
datos <- ejercicio1.xlsx[with(ejercicio1.xlsx, order(ejercicio1.xlsx$...1)), ]

# En los datos tenemos información de todos los meses desde el año 2002 al 
# 2019 (falta el valor del diciembre para 2019)

# Guardamos los datos en un objeto serie temporal y nos quedamos con la 
# segunda columna que es la contiene los valores de la serie
data <- ts(datos$...2, frequency = 12, start=c(2002, 1), end = c(2019, 11)); data

# Como se trata de datos mensuales, donde la S es igual a 12 
# lo dividimos por el numero de dias por mes
data <- data/monthdays(data)
ts.plot(data, main="Serie 1")

# Vemos la descomposición de la serie
seriescomponents <- decompose(data)
plot(seriescomponents)


# --------------
# ESTACIONALIDAD
# --------------

# Representación gráfica de la serie original, de la FAS y de la FAP
tsdisplay(data)


# ---------------
# ESTACIONARIEDAD
# ---------------

# "En caso de que sospechemos que es necesario aplicar diferencias y logaritmos, primero
# se aplicarán logaritmos y segundo las diferencias, sobre la serie ya en logaritmos"

# ARIMA requiere que la serie temporal sea estacionaria.

## Estacionariedad en media
## ........................

# Representación gráfica de la serie original (tendencia creciente/decreciente) 
# y también de la FAS y la FAP. En el caso de que el proceso no sea estacionario 
# los valores en la FAS y/o la FAP decrecen muy lentamente
tsdisplay(data)

# Numéricamente: Test de Dickey-Fuller de raices unitarias. La existencia de 
# raíces unitarias es un indicativo de que el proceso no es estacionario 
# en media y de la necesidad de aplicar diferencias para conseguir 
# la estacionaridad
adf.test(data, alternative = "stationary") # p-value = 0.4837 > 0.05 
# aceptas no estacionaria - no es estacionaria en media

# Si primero haces la diferencia estacional, te dice que es estacional
nsdiffs(data) # 1 (D grande - estacionalidad)

# Si se sabe que una serie tiene una raiz unitaria, la serie puede ser 
# diferenciada para que sea estacionaria
# Aplicamos una diferencia
ndiffs(data) # 1
data.dif <- diff(data)
plot.ts(data, main="Serie original")
plot.ts(data.dif, main="Serie con una diferencia")

## van juntas la D grande y la d pequeña


## Estacionariedad en varianza
## ........................

# Representación gráfica de la serie original (a medida de crecen los retardos 
# la variabilidad aumenta) y también de la FAS y de la FAP En el caso de que 
# el proceso sea no estacionario en varianza los valores en la fas y/o fap 
# decrecen muy lentamente.
tsdisplay(data)

# Numéricamente. Tenemos que comprobar si los datos presentan heterocedasticidad. 
# Para ello primero, verificamos que la varianza sea homogénea (H0: homocedasticidad)
# - Breusch-Pagan, para datos normales.
# - Koenker-Basset, generaliza Breusch-Pagan para datos no normales.

# Para elegir el contraste más adecuado, verificamos normalidad en los datos

# QQ plot
qqnorm(data); qqline(data) # Se observa como los datos no se ajustan a la recta

# Shapiro-Wilk (H0: normalidad)
shapiro.test(data)          
# Obtenemos que el p-value = 0.000162, menor a 0.05, por tanto se rechaza H0

# Jarque-Bera (H0: normalidad)
jarque.bera.test(data)      
# Obtenemos que el p-value = 0.06452, menor a 0.05, por tanto no se rechaza H0

# Jarque-Bera dice que si los datos son normales, pero no le damos importancia

# Acabamos de llegar a la conclusión de que ¿sí? hay normalidad en los datos. 
# Por tanto, Breusch-Pagan no se puede aplicar ya que es para datos normales. 
# Aplicamos Koenkess-Basset
bptest(lm(data ~ seq(length(data))), studentize = T)
# Nos da un p_valor = 0.02797 que es menor que 0.05, entonces confirmamos que
# los datos presentan heterocedasticidad. Para corregirla es necesario hacer 
# uso de la transformacion de Box-Cox

# En caso de que la serie no sea estacionaria en varianza usaremos la transformación
# Box-Cox
?BoxCox.lambda

# Primero, se hace selección automática del parámetro de transformacion de 
# Box-Cox
lambda1 <- BoxCox.lambda(data, method = "guerrero", lower = -1, upper = 1); lambda1
lambda2 <- BoxCox.lambda(data, method = "loglik", lower = -1, upper = 1); lambda2

# A continuación, defimos una función que minimiza
approxLambda <- function(lambda){
  seq(-1,1,.5)[which.min(abs(seq(-1,1,.5) - lambda)) ]  
}

approxLambda(lambda1) # lambda = -1
approxLambda(lambda2) # lambda = -1

# Visualizamos los datos transformados:
ts.plot(BoxCox(data, lambda = approxLambda(lambda1)))
ts.plot(BoxCox(data, lambda = approxLambda(lambda2)))

# Elegimos lambda = -1, ya que no se presenta diferencia entre ambos
# lambda -1 me dice que haga la inversa
lambda = approxLambda(lambda2)
data.t <- BoxCox(data, lambda = lambda)
ts.plot(data, main="Serie original")
ts.plot(data.t, main="Serie después de aplicar BoxCox") # esta es la modificada de BoxCox haciendo la inversa
# Como sale en ambas -1, no tengo que modificar

# Con esto solucionamos la estacionaridad en media 

### Arreglar estacionariedad y estacionalidad

# Podemos usar la funcion nsdiffs() para calcular el numero de diferencianciones estacionales necesarias:
nsdiffs(data.t) # 1

# Primero hago una diferencia estacional y luego paso los test
d12data.t <- diff(data.t, frequency(data.t)) # frequency(data.t) = 12 # Quitar estacionalidad

# Y ahora vamos a pasar los test de estacionariedad
adf.test(d12data.t, alternative = "stationary") # p-value = 0.4259 > 0.05 - se acepta no estacionaria
# esto quiere decir que no es una estacionaria
kpss.test(d12data.t) # p-value = 0.08085 > 0.05 - se acepta estacionaria 
pp.test(d12data.t, alternative = "stationary", type = "Z(t_alpha)") # p-value = 0.01 < 0.05 - se acepta estacionaria 

# Ya es estacionaria en media, pasamos dos de los tres métodos

tsdisplay(d12data.t)
plot.ts(d12data.t, main="Serie diferenciada")

# Veamos la diferencia
ndiffs(d12data.t) # 1
diff <- diff(d12data.t)
plot.ts(diff, main="Serie final")
ndiffs(diff) # 0

tsdisplay(diff)

# Veamos la descomposición de la serie
diffcomponents <- decompose(diff)
plot(diffcomponents)


# ----------------------------
# IDENTIFICACION DE PARAMETROS
# ----------------------------

# Consideramos los modelos SARIMA (p,d,q)(P,D,Q)s 
# (cuando hay estacionalidad). En este caso s=12 (serie mensual)

# El numero de diferencias y de diferencias estacionales necesarias 
# para que la series sea estacionaria determina los parametro "d" 
# y "D", respectivamente.
D <- nsdiffs(data.t)
if(D > 0){
  d <- ndiffs(diff(data.t, lag = frequency(data.t), differences = D))
}else{
  d <- ndiffs(data.t)
}
paste(d,D)

# Observamos ACF y PACF para determinar los valores de p, q, P y Q:
tsdisplay(diff)

# Probamos con un modelo SARIMA(0,1,1)(4,1,4)12 y observamos sus residuos:
# fit <- Arima(y = data, order = c(p,1,1), seasonal = c(0,1,Q), lambda = lambda)

# Modelo 1
fit1 <- Arima(y = data, order = c(1,1,1), seasonal = c(0,1,1), lambda = lambda)
coeftest(fit1)
res1 <- residuals(fit1)
tsdisplay(res1)

# Modelo 2
fit2 <- Arima(y = data, order = c(1,1,1), seasonal = c(0,1,2), lambda = lambda)
coeftest(fit2)
res1 <- residuals(fit2)
tsdisplay(res2)

# Modelo 3
fit3 <- Arima(y = data, order = c(2,1,1), seasonal = c(0,1,1), lambda = lambda)
coeftest(fit3)
res3 <- residuals(fit3)
tsdisplay(res3)

# Modelo 4
fit4 <- Arima(y = data, order = c(2,1,1), seasonal = c(0,1,2), lambda = lambda)
coeftest(fit4)
res4 <- residuals(fit4)
tsdisplay(res4)

# Modelo automático
fit.auto <- auto.arima(data, lambda = BoxCox.lambda(data));fit.auto
coeftest(fit.auto)
res.auto <- residuals(fit.auto)
tsdisplay(res.auto)

# Se recomienda elegir modelos que cumplan con todas las (o el mayor número de) propiedades.
# En el caso de tener mas de un modelo que cumpla con las propiedades, se recomienda aplicar el principio de parsimonia: se elige el modelo mas sencillo.
# Suponiendo que todos los modelos cumplan con las condiciones sobre los residuos:
c(AIC(fit1), AIC(fit2), AIC(fit3), AIC(fit4))
c(BIC(fit1), BIC(fit2), BIC(fit3), BIC(fit4))



> coeftest(fit)

z test of coefficients:
  
  Estimate Std. Error z value  Pr(>|z|)    
ma1  -0.683596   0.038295 -17.851 < 2.2e-16 ***
  sma1 -0.806970   0.060559 -13.325 < 2.2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

solo si p y q es 1

MA es -0.683596  y como en modulo es menor que 1, es invertible

para AR estacionaridad


para p y q mayor a 2 pagina 3 del tema 2


# ---------------------
# VALIDACION DEL MODELO
# ---------------------

# Los residuos del modelo tienen que cumplir con las siguientes propiedades:
# - Los residuos no son correlados. Si hay correlacion, entonces queda 
#   informacion que se puede usar para mejorar la prediccion. Esto se puede 
#   averiguar con una prueba de independencia, como las pruebas de Box-Pierce 
#   y de Ljung-Box (mejor). En ambas pruebas, H0: independencia (incorrelacion).
# - Los residuos tienen media cero.

# Modelo 1
Box.test.2(residuals(fit1), nlag=30, type="Ljung-Box")
Box.test(res, type = "Box-Pierce", lag = frequency(res)*2, fitdf = sum(fit$arma[1:2]))

# Modelo 2
Box.test.2(residuals(fit2), nlag=30, type="Ljung-Box")

# Modelo 3
Box.test.2(residuals(fit3), nlag=30, type="Ljung-Box")

# Modelo 4
Box.test.2(residuals(fit4), nlag=30, type="Ljung-Box")


## 2. Hacemos uso de "Box-Pierce"

# Modelo 1
Box.test(res1, type = "Box-Pierce", lag = 30, fitdf = sum(fit1$arma[1:2]))

# Modelo 2
Box.test(res2, type = "Box-Pierce", lag = 30, fitdf = sum(fit2$arma[1:2]))

# Modelo 3
Box.test(res3, type = "Box-Pierce", lag = 30, fitdf = sum(fit3$arma[1:2]))

# Modelo 4
Box.test(res4, type = "Box-Pierce", lag = 30, fitdf = sum(fit4$arma[1:2]))


## 3. Hacemos uso de "Ljung-Box"

# Modelo 1
Box.test(res1, type = "Ljung-Box", lag = 30, fitdf = sum(fit1$arma[1:2]))

# Modelo 2
Box.test(res2, type = "Ljung-Box", lag = 30, fitdf = sum(fit2$arma[1:2]))

# Modelo 3
Box.test(res3, type = "Ljung-Box", lag = 30, fitdf = sum(fit3$arma[1:2]))

# Modelo 4
Box.test(res4, type = "Ljung-Box", lag = 30, fitdf = sum(fit4$arma[1:2]))


## 4. Sacamos la media de los residuos

# Modelo 1
mean(res1)
# Modelo 2
mean(res2)
# Modelo 3
mean(res3)
# Modelo 4
mean(res4)


# Ademas de estas propiedades esenciales, es util (pero no necesario) que los residuos cumplan con lo siguiente:
# - Los residuos se distribuyen de forma normal.
# - Los residuos tienen varianza constante.
# Estas propiedades hacen que el calculo de los intervalos de prediccion sea mas facil.
  
## Ver si los residuos se distribuyen de forma normal

# Modelo 1
qqnorm(res1, main = "Normal Q-Q Plot - Modelo 1"); qqline(res1)
shapiro.test(res1) # p-value = 0.0001154
jarque.bera.test(res1) # p-value = 0.0002828

# Modelo 2
qqnorm(res2, main = "Normal Q-Q Plot - Modelo 2"); qqline(res2)
shapiro.test(res2) # p-value = 0.001756
jarque.bera.test(res2) # p-value = 0.004049

# Modelo 3
qqnorm(res3, main = "Normal Q-Q Plot - Modelo 3"); qqline(res3)
shapiro.test(res3) # p-value = 0.000205
jarque.bera.test(res3) # p-value = 5.088e-05

# Modelo 4
qqnorm(res4, main = "Normal Q-Q Plot - Modelo 4"); qqline(res4)
shapiro.test(res4) # p-value = 4.909e-05
jarque.bera.test(res4) # p-value = 2.422e-06

## Ver si los residuos tienen varianza constante.

# Modelo 1
bptest(lm(res1 ~ seq(length(res1))), studentize = T) # p-value = 0.5348

# Modelo 2
bptest(lm(res2 ~ seq(length(res2))), studentize = T) # p-value = 0.8572

# Modelo 3
bptest(lm(res3 ~ seq(length(res3))), studentize = T) # p-value = 0.8035

# Modelo 4
bptest(lm(res4 ~ seq(length(res4))), studentize = T) # p-value = 0.5534

# Se recomienda elegir modelos que cumplan con todas las (o el mayor número de) propiedades.
# En el caso de tener mas de un modelo que cumpla con las propiedades, se recomienda aplicar el principio de parsimonia: se elige el modelo mas sencillo.
# Suponiendo que todos los modelos cumplan con las condiciones sobre los residuos:
c(AIC(fit1), AIC(fit2), AIC(fit3), AIC(fit4))
c(BIC(fit1), BIC(fit2), BIC(fit3), BIC(fit4))

# Comprobamos las dos propiedades:

# mirar BIC
# mirar residuos

autoplot(fit)

# Dibujamos los residuos
ts.plot(res)
tsdisplay(res)

Box.test(res1, type = "Box-Pierce", lag = 30, fitdf = sum(fit1$arma[1:2]))
# Hacemos uso de "Box-Pierce", el p_valor es mayor a 0.06843 por 
# tanto aceptamos que son incorrelados, más p y q chiquititas

# incorrelación
Box.test(res, type = "Ljung-Box", lag = frequency(res)*30, fitdf = sum(fit$arma[1:2]))
# Hacemos uso de "Ljung-Box", el p_valor no es mayor a 0.04635 por 
# tanto aceptamos que la media es nula




# Para ver la varianza tenemos que ver 
bptest(lm(res ~ seq(length(res))), studentize = T) # p-value = 0.665 - no es constante la varianza

coeftest(fit)


# se cog el modelo que tenga menor y mayor


# Para ver lo de las raíces unitarias 
autoplot(fit)

# En nuestro caso, no todas las propiedades se verifican.
# Si alguna de estas propiedades no se verifica, entonces el modelo puede mejorarse.

# Ademas de estas propiedades esenciales, es util (pero no necesario) que los residuos cumplan con lo siguiente:
# - Los residuos se distribuyen de forma normal.
# - Los residuos tienen varianza constante.
# Estas propiedades hacen que el calculo de los intervalos de prediccion sea mas facil.

# Ahora vamos a ver qué pasa con la normalidad.
qqnorm(res); qqline(res)

x <- seq(-0.03,0.02, length = length(data))
hist(res, prob = T, col = "red"); points(x, dnorm(x), type = "l")
x <- seq(-3,3, length = 1000)

shapiro.test(res)
jarque.bera.test(res)
# Los p_valor no son mayores a 0.05, por lo tanto no podemos aceptar normalidad

# es true porque los datos no - Koenker-Basser
bptest(lm(res ~ seq(length(res))), studentize = T) # p-value = 0.665 - aceptas que no es estacionario



adf.test(res, alternative = "stationary")
kpss.test(res)
pp.test(res, alternative = "stationary", type = "Z(t_alpha)")

# NOTA: es posible que la presencia de outliers en los datos haga que no sea 
# posible satisfacer alguna de las propiedades. Veremos como identificar outliers mas adelante...

# ------------------------------
# SELECCION AUTOMATICA DE MODELO
# ------------------------------

sarima.auto <- auto.arima(data, lambda = -1)
sarima.auto

# Recuerda: solo podemos comparar por AIC y BIC modelos que tengan los mismos valores.


################################ EJERCICIO 2 #################################

## Ecuación
## ........

coeftest(res2)

# Para ver lo de las raíces unitarias 
autoplot(fit2)

## Condiciones de estacionaredad
## .............................

# AR es -0.542521 y como en modulo es menor que 1, es estacionario


## Condiciones de invertibilidad
## .............................

# MA es -0.542521 y como en modulo es menor que 1, es invertible


## Condiciones de significatividad
## ...............................


## Ausencia de autocorrelación entre los parámetros
## ................................................
# https://rdrr.io/cran/caschrono/man/cor.arma.html
cor.arma(fit2)

