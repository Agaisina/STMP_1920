library(haven)
data <- read_sas("brentPriceData.sas7bdat")
head(data)

# Gráfico de la serie
graficoInicial <- ggplot(aes(x= Date, y = close), data = data) +
  geom_line(color = '#d84519', size = 1) + 
  xlab('FECHA') + ylab('data')
ggplotly(graficoInicial)

#------------------------------------------------
#
#         METODOLOGÍA BOX-JENKINS
#
#------------------------------------------------
#
#       1. FASE DE IDENTIFICACIÓN
#
#------------------------------------------------

# 1.1. ¿La serie es estacionaria en varianza?

# Primero, verificamos que la varianza sea homogenea (H0: homocedasticidad):
# - Breusch-Pagan, para datos normales.
# - Koenker-Basset, generaliza Breusch-Pagan para datos no normales.

# Para elegir el contraste mas adecuado, verificamos normalidad en los datos:
# - QQ plot
# - Shapiro-Wilk (H0: normalidad)
# - Jarque-Bera (H0: normalidad)

lim = length(data$close)-10
data.ts <- ts(data$close[1:lim])

validation <- ts(data$close[lim:length(data$close)])

qqnorm(data.ts); qqline(data.ts)
shapiro.test(data.ts)    # p-value = 1.212e-05 ==> Se rechaza normalidad
library(tseries)
jarque.bera.test(data.ts)  # p-value = 0.01505 ==> Se rechaza normalidad

# Los datos no son normales. No se puede aplicar Breusch-Pagan:
# bptest(lm(data ~ seq(length(data))), studentize = F)
# Aplicamos Koenker-Basser:

library(lmtest)
bptest(lm(data.ts ~ seq(length(data.ts))), studentize = T) # p-value = 4.003e-14 ==> Se rechaza homocedasticidad

# Los datos presentan heterocedasticidad.
# Procedemos a corregir la heterocedasticidad usando la transformación de Box-Cox.

# Selección automática del parámetro de transformación de Box-Cox
box_cox <- boxcox(close ~ Date, data = data, lambda = c(0, 0.5, 1))
lambda <- box_cox$x[which.max(box_cox$y)]
lambda

# Como lambda casi 1, no hacemos nada, se queda la misma

#------------------------------------------------
# 1.2. ¿La serie es estacionaria en media?

# Formalmente, se pueden usar las siguientes pruebas para determinar la existencia o no de raices unitarias en una serie temporal:
# - Dickey-Fuller aumentada (H0: existe una raiz unitaria en la serie - serie no estacionaria)
# - Kwiatkowski-Phillips-Schmidt-Shin (KPSS) (H0: la serie es estacionaria)
# - Phillips-Perron (H0: existe una raiz unitaria en la serie - serie no estacionaria). La prueba de Phillips-Perron es menos eficiente en muestras finitas que la prueba de Dickey-Fuller aumentada.

adf.test(data.ts)   # Se acepta H0: serie no estacionaria
kpss.test(data.ts)  # Se rechaza H0: serie estacionaria
pp.test(data.ts)    # Se acepta H0: serie no estacionaria

# Se hace necesario entonces diferenciar la serie
# Podemos usar el siguiente comando para saber cuántas veces es necesario diferenciarla
ndiffs(data.ts)   # Se indica que es necesario diferenciarla 1 vez

data_diff <- diff(data.ts)   # Diferenciamos la serie
plot.ts(data_diff)           # Graficamos la serie diferenciada

# Comprobamos si esta serie diferenciada es estacionaria

# ¿Estacionaria en varianza?

qqnorm(data_diff); qqline(data_diff)
shapiro.test(data_diff)    # p-value < 2.2e-16 ==> Se rechaza normalidad

jarque.bera.test(data_diff)  # p-value < 2.2e-16 ==> Se rechaza normalidad

# Los datos no son normales, aplicamos Koenker-Basser
bptest(lm(data_diff ~ seq(length(data_diff))), studentize = T) 
# p-value = 0.7339 ==> Se acepta homocedasticidad, luego es estacionaria en varianza

# ¿Estacionaria en media?
adf.test(data_diff)   # Se rechaza H0: serie no estacionaria
kpss.test(data_diff)  # Se acepta H0: serie estacionaria
pp.test(data_diff)    # Se rechaza H0: serie no estacionaria

# ¡Hemos conseguido estacionaridad en media y varianza!

#------------------------------------------------
# 1.3. Estimación de los órdenes (p,d,q) x (P,D,Q)

# Sabemos que hay que diferenciar la serie 1 vez, por tanto, d = 1

ajuste1 <- arima(data.ts,
                 order = c(0,1,0),
                 method = "ML")
ajuste1

# Evaluamos este primer modelo 
residuos_ajuste1 <- ajuste1$residuals

Acf(residuos_ajuste1, lag.max = 25, xlab = "Retardo")   # Primer retardo significativo
Pacf(residuos_ajuste1, lag.max = 25, xlab = "Retardo")  # Primer retardo significativo

checkresiduals(residuos_ajuste1)

# Media = 0:
mean(residuos_ajuste1)
t.test(residuos_ajuste1)  # p-value = 0.9194 ==> Se acepta H0: Media = 0

# Incorrelación:
tsdisplay(residuos_ajuste1)
Box.test(residuos_ajuste1, type = "Box-Pierce", lag = frequency(residuos_ajuste1)*2, fitdf = sum(ajuste1$arma[1:2]))
Box.test(residuos_ajuste1, type = "Ljung-Box", lag = frequency(residuos_ajuste1)*2, fitdf = sum(ajuste1$arma[1:2]))

# Para ambos test se rechaza H0: Correlación ==> Modelo válido

# ¿Ruido blanco?

# Homocedasticidad:
bptest(lm(residuos_ajuste1 ~ seq(length(residuos_ajuste1))), studentize = T) 
# p-valor = 0.7491 ==> Se acepta H0: Homocedasticidad

# Normalidad:
library(e1071)

skewness(residuos_ajuste1)   # -0.05864763 ==> Muy ligeramente asimétrica hacia la izquierda
kurtosis(residuos_ajuste1)   # 4.052974 ==> Distribución leptocúrtica
hist(residuos_ajuste1, col="blue")

jarque.bera.test(residuos_ajuste1)    # p-valor < 2.2e-16 ==> No hay normalidad

# Nuestro primer modelo es válido, pero mejorable. 
# No tenemos ruido blanco, ya que, aunque se da homocedasticidad en
# los residuos, no tenemos normalidad.



#--------------------------------------
# Ajustamos un segundo modelo

ajuste2 <- arima(data.ts,
                 order = c(1,1,0),
                 method = "ML")
ajuste2
coeftest(ajuste2)

# Evaluamos este segundo modelo
residuos_ajuste2 <- ajuste2$residuals

Acf(residuos_ajuste2, lag.max = 25, xlab = "Retardo")   # No hay retardos significativos
Pacf(residuos_ajuste2, lag.max = 25, xlab = "Retardo")  # No hay retardos significativos

checkresiduals(residuos_ajuste2)

# Media = 0:
mean(residuos_ajuste2)
t.test(residuos_ajuste2)  # p-value = 0.912 ==> Se acepta H0: Media = 0

# Incorrelación:
tsdisplay(residuos_ajuste2)
Box.test(residuos_ajuste2, type = "Box-Pierce", lag = frequency(residuos_ajuste2)*2, fitdf = sum(ajuste2$arma[1:2]))
Box.test(residuos_ajuste2, type = "Ljung-Box", lag = frequency(residuos_ajuste2)*2, fitdf = sum(ajuste2$arma[1:2]))

# Para ambos test se acepta H0: Correlación ==> Modelo no válido
# Nuestro segundo modelo no es válido ya que no se da incorrelación en los residuos



#--------------------------------
# Ajustamos un tercer modelo

ajuste3 <- arima(data.ts,
                 order = c(0,1,1),
                 method = "ML")
ajuste3
coeftest(ajuste3)

# Evaluamos este tercer modelo
residuos_ajuste3 <- ajuste3$residuals

Acf(residuos_ajuste3, lag.max = 25, xlab = "Retardo")   # No se dan retardos significativos
Pacf(residuos_ajuste3, lag.max = 25, xlab = "Retardo")  # No se dan retardos significativos

checkresiduals(residuos_ajuste3)

# Media = 0:
mean(residuos_ajuste3)
t.test(residuos_ajuste3)   # p-value = 0.9116 ==> Se acepta H0: Media = 0

# Incorrelación:
tsdisplay(residuos_ajuste3)
Box.test(residuos_ajuste3, type = "Box-Pierce", lag = frequency(residuos_ajuste3)*2, fitdf = sum(ajuste3$arma[1:2]))
Box.test(residuos_ajuste3, type = "Ljung-Box", lag = frequency(residuos_ajuste3)*2, fitdf = sum(ajuste3$arma[1:2]))
# En ambos tests se acepta H0: Correlación ==> Modelo no válido
# Nuestro modelo no es válido ya que no se da incorrelación en los residuos

#-----------------------------------
# Ajustamos un cuarto modelo

ajuste4 <- arima(data.ts,
                 order = c(1,1,1),
                 method = "ML")
ajuste4
coeftest(ajuste4)   # Los coeficientes no salen como significativos

# Evaluamos el modelo
residuos_ajuste4 <- ajuste4$residuals

Acf(residuos_ajuste4, lag.max = 25, xlab = "Retardo")    # No hay retardos significativos
Pacf(residuos_ajuste4, lag.max = 25, xlab = "Retardo")   # No hay retardos significativos

checkresiduals(residuos_ajuste4)

# Media = 0:
mean(residuos_ajuste4)
t.test(residuos_ajuste4)  # p-value = 0.9133 ==> Se acepta H0: Media = 0

# Incorrelación:
tsdisplay(residuos_ajuste4)
Box.test(residuos_ajuste4, type = "Box-Pierce", lag = frequency(residuos_ajuste4)*2, fitdf = sum(ajuste4$arma[1:2]))
Box.test(residuos_ajuste4, type = "Ljung-Box", lag = frequency(residuos_ajuste4)*2, fitdf = sum(ajuste4$arma[1:2]))
# En ambos tests se rechaza H0: Correlación ==> Modelo válido

# Tenemos modelo válido, veamos si tenemos ruido blanco

# Homocedasticidad de la varianza:
bptest(lm(residuos_ajuste4 ~ seq(length(residuos_ajuste4))), studentize = T) 
# p-valor = 0.7931 ==> Se acepta H0: Homocedasticidad

# Normalidad:
skewness(residuos_ajuste4)   # -0.03051244 ==> Muy ligeramente asimétrica hacia la izquierda
kurtosis(residuos_ajuste4)   # 4.010615 ==> Distribución leptocúrtica
hist(residuos_ajuste4, col="blue")

jarque.bera.test(residuos_ajuste4)    # p-valor < 2.2e-16 ==> No hay normalidad


# En resumidas cuentas, contamos con dos modelos válidos:
ajuste1  # ARIMA(0,1,0)
ajuste4  # ARIMA(1,1,1)

# En ambos no tenemos ruido blanco porque no se da normalidad.
# Veamos si tratando los outliers conseguimos ruido blanco.

# Empezamos con el primer modelo.

#-----------------------------------------
# 1.4. ANÁLSIS DE INTERVENCIONES

# Outliers Modelo 1

library(tsoutliers)
library(dplyr)

# Buscamos que sean muy significativos (|t-ratio|>3)

listaOutliers <- locate.outliers(residuos_ajuste1,
                                 pars = coefs2poly(ajuste1),
                                 types = c("AO", "LS", "TC"),cval=3)
listaOutliers$abststat=abs(listaOutliers$tstat)
arrange(listaOutliers,desc(listaOutliers$abststat))

outliers <- outliers(c("TC", "AO", "AO"), c(1155,1154,137))
outliersVariables <- outliers.effects(outliers, length(residuos_ajuste1))
x<-as.data.frame(outliersVariables)

ajuste1conOutliers <- arima(data.ts,
                            order = c(0,1,0),
                            method = "ML",
                            xreg = outliersVariables)
ajuste1conOutliers
coeftest(ajuste1conOutliers)   # El primer AO no es significativo

Acf(ajuste1conOutliers$residuals, lag.max = 25, xlab = "Retardo")  # El primer retardo es significativo
Pacf(ajuste1conOutliers$residuals, lag.max = 25, xlab = "Retardo") # El primer retardo es signficativo

# Ajustamos el primer modelo sin contar el primer AO
outliers <- outliers(c("TC","AO"), c(1155,137))
outliersVariables <- outliers.effects(outliers, length(residuos_ajuste1))
x <- as.data.frame(outliersVariables)

ajuste1conOutliers <- arima(data.ts,
                            order = c(0,1,0),
                            method = "ML",
                            xreg = outliersVariables)
ajuste1conOutliers
coeftest(ajuste1conOutliers)

Acf(ajuste1conOutliers$residuals, lag.max = 25, xlab = "Retardo")  # El primer retardo es significativo

# Evaluación del modelo 
residuos_ajuste1conOutliers <- ajuste1conOutliers$residuals

checkresiduals(residuos_ajuste1conOutliers)
Acf(residuos_ajuste1conOutliers, lag.max = 25, xlab = "Retardo")  # Primer retardo significativo
Pacf(residuos_ajuste1conOutliers, lag.max = 25, xlab = "Retardo") # Primer retardo significativo

# Media = 0:
mean(residuos_ajuste1conOutliers)
t.test(residuos_ajuste1conOutliers)  # p-value = 0.9164 ==> Se acepta H0: Media = 0

# Incorrelación:
tsdisplay(residuos_ajuste1conOutliers)
Box.test(residuos_ajuste1conOutliers, type = "Box-Pierce", lag = frequency(residuos_ajuste1conOutliers)*2, fitdf = sum(ajuste1conOutliers$arma[1:2]))
Box.test(residuos_ajuste1conOutliers, type = "Ljung-Box", lag = frequency(residuos_ajuste1conOutliers)*2, fitdf = sum(ajuste1conOutliers$arma[1:2]))
# En ambos test se rechaza H0: Correlación ==> Modelo válido

# Veamos si tenemos ruido blanco

# Homocedasticidad de la varianza:
bptest(lm(residuos_ajuste1conOutliers ~ seq(length(residuos_ajuste1conOutliers))), studentize = T) 
# p-valor = 0.1998 ==> Se acepta H0: Homocedasticidad

# Normalidad:
skewness(residuos_ajuste1conOutliers)  # -0.364154 ==> Asimétrica hacia la izquierda
kurtosis(residuos_ajuste1conOutliers)  # 2.035665 ==> Leptocúrtica
hist(residuos_ajuste1conOutliers,col='blue')

jarque.bera.test(residuos_ajuste1conOutliers)  # p-value < 2.2e-16 ==> Se rechaza H0: Normalidad


#-----------------------------------------
# Outliers Modelo 4

listaOutliers <- locate.outliers(residuos_ajuste4,
                                 pars = coefs2poly(ajuste4),
                                 types = c("AO", "LS", "TC"),cval=3)
listaOutliers$abststat=abs(listaOutliers$tstat)
arrange(listaOutliers,desc(listaOutliers$abststat))

outliers <- outliers(c("TC", "AO"), c(1155,137))
outliersVariables <- outliers.effects(outliers, length(residuos_ajuste4))
x<-as.data.frame(outliersVariables)

ajuste4conOutliers <- arima(data.ts,
                            order = c(1,1,1),
                            method = "ML",
                            xreg = outliersVariables)
ajuste4conOutliers
coeftest(ajuste4conOutliers)   

Acf(ajuste4conOutliers$residuals, lag.max = 25, xlab = "Retardo")  # No hay retardos significativos
Pacf(ajuste4conOutliers$residuals, lag.max = 25, xlab = "Retardo") # No hay retardos significativos

# Evaluación del modelo 
residuos_ajuste4conOutliers <- ajuste4conOutliers$residuals

# Media = 0:
mean(residuos_ajuste4conOutliers)
t.test(residuos_ajuste4conOutliers)  # p-value = 0.912 ==> Se acepta H0: Media = 0

# Incorrelación:
tsdisplay(residuos_ajuste4conOutliers)
Box.test(residuos_ajuste4conOutliers, type = "Box-Pierce", lag = frequency(residuos_ajuste4conOutliers)*2, fitdf = sum(ajuste4conOutliers$arma[1:2]))
Box.test(residuos_ajuste4conOutliers, type = "Ljung-Box", lag = frequency(residuos_ajuste4conOutliers)*2, fitdf = sum(ajuste4conOutliers$arma[1:2]))
# En ambos test se rechaza H0: Correlación ==> Modelo válido

# Veamos si tenemos ruido blanco

# Homocedasticidad de la varianza:
bptest(lm(residuos_ajuste4conOutliers ~ seq(length(residuos_ajuste4conOutliers))), studentize = T) 
# p-valor = 0.2319 ==> Se acepta H0: Homocedasticidad

# Normalidad:
skewness(residuos_ajuste4conOutliers)  # -0.3492537 ==> Asimétrica hacia la izquierda
kurtosis(residuos_ajuste4conOutliers)  # 2.035879 ==> Leptocúrtica
hist(residuos_ajuste4conOutliers,col='blue')

jarque.bera.test(residuos_ajuste4conOutliers)  # p-value < 2.2e-16 ==> Se rechaza H0: Normalidad


# En ninguno de los modelos se consigue ruido blanco

# Veamos si realizando las intervenciones de otra forma conseguimos ruido blanco

listaOutliers <- locate.outliers(residuos_ajuste1,
                                 pars = coefs2poly(ajuste1),
                                 types = c("AO", "LS", "TC"),cval=3)
listaOutliers$abststat=abs(listaOutliers$tstat)
arrange(listaOutliers,desc(listaOutliers$abststat))

outliers <- outliers(c("TC", "LS", "LS", "LS", "LS", "TC", "LS", "LS", "LS", "LS","LS", "TC", "LS"), 
                     c(1155, 97, 857, 41, 1081, 952, 1124, 945, 138, 135,950,36,453))
outliersVariables <- outliers.effects(outliers, length(residuos_ajuste1))
x<-as.data.frame(outliersVariables)

ajuste1conOutliers <- arima(data.ts,
                            order = c(0,1,0),
                            method = "ML",
                            xreg = outliersVariables)
ajuste1conOutliers
coeftest(ajuste1conOutliers)

Acf(ajuste1conOutliers$residuals, lag.max = 25, xlab = "Retardo")  # Primer retardo significativo
Pacf(ajuste1conOutliers$residuals, lag.max = 25, xlab = "Retardo") # Primer retardo significativo

# Evaluación del modelo 

# Media = 0:
mean(residuos_ajuste1conOutliers)
t.test(residuos_ajuste1conOutliers)  # p-value = 0.9164 ==> Se acepta H0: Media = 0

# Incorrelación:
tsdisplay(residuos_ajuste1conOutliers)
Box.test(residuos_ajuste1conOutliers, type = "Box-Pierce", lag = frequency(residuos_ajuste1conOutliers)*2, fitdf = sum(ajuste1conOutliers$arma[1:2]))
Box.test(residuos_ajuste1conOutliers, type = "Ljung-Box", lag = frequency(residuos_ajuste1conOutliers)*2, fitdf = sum(ajuste1conOutliers$arma[1:2]))
# En ambos test se rechaza H0: Correlación ==> Modelo válido

# Veamos si tenemos ruido blanco

# Homocedasticidad de la varianza:
bptest(lm(residuos_ajuste1conOutliers ~ seq(length(residuos_ajuste1conOutliers))), studentize = T) 
# p-valor = 0.1998 ==> Se acepta H0: Homocedasticidad

# Normalidad:
qqnorm(residuos_ajuste1conOutliers);qqline(residuos_ajuste1conOutliers)
skewness(residuos_ajuste1conOutliers)  # -0.3494818 ==> Asimétrica hacia la izquierda
kurtosis(residuos_ajuste1conOutliers)  # 2.040348 ==> Leptocúrtica
hist(residuos_ajuste1conOutliers,col='blue')

jarque.bera.test(residuos_ajuste1conOutliers)  # p-value < 2.2e-16 ==> Se rechaza H0: Normalidad

# Veamos con el 4

ajuste4conOutliers <- arima(data.ts,
                            order = c(1,1,1),
                            method = "ML",
                            xreg = outliersVariables)
ajuste4conOutliers
coeftest(ajuste4conOutliers)

Acf(ajuste4conOutliers$residuals, lag.max = 25, xlab = "Retardo")  # Primer retardo significativo
Pacf(ajuste4conOutliers$residuals, lag.max = 25, xlab = "Retardo") # Primer retardo significativo

# Evaluación del modelo 

# Media = 0:
mean(residuos_ajuste4conOutliers)
t.test(residuos_ajuste4conOutliers)  # p-value = 0.912 ==> Se acepta H0: Media = 0

# Incorrelación:
tsdisplay(residuos_ajuste4conOutliers)
Box.test(residuos_ajuste4conOutliers, type = "Box-Pierce", lag = frequency(residuos_ajuste4conOutliers)*2, fitdf = sum(ajuste4conOutliers$arma[1:2]))
Box.test(residuos_ajuste4conOutliers, type = "Ljung-Box", lag = frequency(residuos_ajuste4conOutliers)*2, fitdf = sum(ajuste4conOutliers$arma[1:2]))
# En ambos test se rechaza H0: Correlación ==> Modelo válido

# Veamos si tenemos ruido blanco

# Homocedasticidad de la varianza:
bptest(lm(residuos_ajuste4conOutliers ~ seq(length(residuos_ajuste4conOutliers))), studentize = T) 
# p-valor = 0.2319 ==> Se acepta H0: Homocedasticidad

# Normalidad:
qqnorm(residuos_ajuste4conOutliers);qqline(residuos_ajuste4conOutliers)
skewness(residuos_ajuste4conOutliers)  # -0.3492537 ==> Asimétrica hacia la izquierda
kurtosis(residuos_ajuste4conOutliers)  # 2.035879 ==> Leptocúrtica
hist(residuos_ajuste4conOutliers,col='blue')

jarque.bera.test(residuos_ajuste4conOutliers)  # p-value < 2.2e-16 ==> Se rechaza H0: Normalidad

#--------------------------------
# Nos quedamos con los modelos:
ajuste1
ajuste1conOutliers   # Tiene menor AIC que ajuste1
ajuste4
ajuste4conOutliers   # Es el que tiene menor AIC


prediccion_ajuste1 <- as.data.frame(predict(ajuste1, n.ahead=10))
View(prediccion_ajuste1)
prediccion_ajuste1conOutliers <- as.data.frame(predict(ajuste1conOutliers,n.ahead=10))
View(prediccion_ajuste1conOutliers)
prediccion_ajuste4 <- as.data.frame(predict(ajuste4, n.ahead=10))
View(prediccion_ajuste4)
prediccion_ajuste4conOutliers <- as.data.frame(predict(ajuste4conOutliers,n.ahead=10))
View(prediccion_ajuste4conOutliers)

forecast(ajuste1, lead=10)
forecast(ajuste1conOutliers, lead=10)
forecast(ajuste4, lead=10)
forecast(ajuste4conOutliers, lead=10)

