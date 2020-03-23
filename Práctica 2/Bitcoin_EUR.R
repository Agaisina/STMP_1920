# PRÁCTICA SERIES TEMPORALES

# Gema Correa Fernández
# Ágata De Isidro Navarro
# Alberto Ruíz-Arteaga González

# ---------------------------------------------------------------------

# Importamos las librerías
library(plotly)
library(MASS)
library(forecast)
library(lmtest)
library(tseries)
library(aTSA)
library(rugarch)
library(tsoutliers)
library(dplyr)
library(e1071)
library(car)

# ---------------------------------------------------------------------

# Se limpia el espacio de trabajo
rm(list = ls())

# Se asigna el directorio donde están los datos
path = "datos"
setwd(path)


# ---------------------------------------------------------------------

# Carga de datos "Bitcoin EUR (BTC-EUR)"
# https://es.finance.yahoo.com/quote/BTC-EUR/

bitcointEUR <- read.csv("BTC-EUR-2014_sinultimos7_dias.csv")

# Los datos con que vamos a trabajar tienen distintos valores (apertura, 
# máximo, mínimo, cierre y cierre ajustado). 
# Nosotros vamos a utilizar el Cierre Ajustado (Valor recomendado)

bitcointEUR <- bitcointEUR[c('Date','Adj.Close')] 
bitcointEUR$Date <- as.Date(bitcointEUR$Date,format="%Y-%m-%d")
View(bitcointEUR)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Ajustar un modelo ARIMA(p,d,q) a la media de serie para conseguir RB, 
# justificando la necesidad de hacer transformación, diferencias, 
# posibles órdenes p y q y posibles intervenciones. 
# Obtener la serie de los residuales cuadráticos.

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Gráfico de la serie
graficoInicial <- ggplot(aes(x= Date, y = Adj.Close), data = bitcointEUR) +
  geom_line(color = '#d84519', size = 1) + 
  xlab('FECHA') + ylab('bitcointEUR')
ggplotly(graficoInicial)

# Se evalúa la necesidad de transformar las series para 
# hacerla estacionaria en varianza 

# Calculamos la transformada de BoxCox
box_cox <- boxcox(Adj.Close ~ Date, data = bitcointEUR, lambda = c(0, 0.5, 1))
lambda <- box_cox$x[which.max(box_cox$y)]; lambda

# Como lambda es 0, se toman logaritmos
# https://www.statisticshowto.datasciencecentral.com/box-cox-transformation/
bitcointEUR$log_Adj.Close <- log(bitcointEUR$Adj.Close)

# Graficamos (ver el comportamiento después de haber aplicado logaritmos)
graficoInicial <- ggplot(aes(x= Date, y = log_Adj.Close), data = bitcointEUR) +
  geom_line(color = '#d84519', size = 1) + 
  xlab('FECHA') + ylab('Ibex35')
ggplotly(graficoInicial)

# Convertimos a serie temporal
bitcointEUR.ts <- as.ts(bitcointEUR$log_Adj.Close)

# Funciones de autocorrelación simple y parcial
Acf(bitcointEUR.ts, lag.max = 25, xlab = "Retardo", 
    main= "Función de autocorrelación simple")
Pacf(bitcointEUR.ts, lag.max = 25, xlab = "Retardo", 
     main = "Función de autocorrelación parcial")

# En la ACF se nos sale fuera de la banda todos los retardos 
# (siempre se sale el primero). Nos sugiere un MA(infinito)
# En la PAFC se sale el primer retardo, ajustaremos un modelo ARIMA p=1

# Ajustamos un modelo ARIMA donde p=1 (Ver comportamiento AR(1))
ajuste1 <- arima(bitcointEUR.ts, order = c(1,0,0), method = "ML")
ajuste1
coeftest(ajuste1) # Como ar1 = 0.99968983, es necesario diferenciar

# Como ar1 es casi uno pues quitamos el ar y diferenciamos
# Pero para asegurarnos de que queremos diferenciar aplicamos
# el Test de Dickey-Fuller
adf.test(bitcointEUR.ts) 
# Como p-value > 0.05, aceptamos H0: no estacionario, 
# por lo tanto hay que diferenciar

# Como el parámetro del AR1 está muy cerca de 1, lo sustituimos
# por una diferencia
ajuste2 <- arima(bitcointEUR.ts, order = c(0,1,0), method = "ML")
ajuste2

# Analizamos el comportamiento de este ajuste 
Acf(ajuste2$residuals, lag.max = 25, xlab = "Retardo",
    main= "Función de autocorrelación simple")

# En el Acf vemos que los retardos de los residuos no están dentro de la banda
# Sin embargo, no nos sugiere ningún orden lógico de arima
# por lo tanto la siguiente vía es analizar las posibles intervenciones

# Nuestra serie tiene una naturaleza muy inestable, debido a que es un mercado 
# reciente y de alto riesgo por lo que muchos de los datos serán considerados
# como atípicos. Las intervenciones que analizaremos corresponderán a valores 
# críticos muy altos, del orden > 10
listaOutliers <- locate.outliers(ajuste2$residuals,pars = coefs2poly(ajuste2),
                                 types = c("LS", "TC", "AO"), cval=10)

listaOutliers

# Extraemos los "tipos" e "índices" de los oitliers obtenidos
outliers <- outliers(listaOutliers$type, listaOutliers$ind)
outliersVariables <- outliers.effects(outliers, length(ajuste2$residuals))
x <- as.data.frame(outliersVariables)

# Ajustamos de nuevo la serie añadiendo dichas intervenciones
ajuste2conOutliers <- arima(bitcointEUR.ts, order = c(0,1,0), method = "ML",
                            xreg = outliersVariables)
coeftest(ajuste2conOutliers)
Acf(ajuste2conOutliers$residuals, lag.max = 25, xlab = "Retardo",
    main= "Función de autocorrelación simple")

# Los retardos prácticamente se encuentran todos dentro de la banda
# Vimos cómo añadiendo más outliers (hasta 10 outliers) conseguíamos
# que todos los retardos estuviesen perfectamente dentro de la banda
# Sin embargo, teniendo presente el principio de parsimonia y al no
# observar mejoras notables decidimos incorporar únicamente 
# 3 outliers al ajuste

# Vamos a estudiar el ruido blanco de los residuos
residuosDif <- ajuste2conOutliers$residuals

# Graficamos la QQ-Plot para ver la normalidad de los residuos,
# como ya hemos comentado anteriormente nuestra serie está llena de outliers
# por lo que en las colas nos alejamos mucho de la normalidad
qqPlot(residuosDif)

# Vamos a ver la asimetría, curtosis y el histograma
skewness(residuosDif) # -0.266838
kurtosis(residuosDif) # 4.097776
hist(residuosDif, nclass=10, col="blue") 
# El histograma se comporta aproximadamente como una distribución normal

jarque.bera.test(residuosDif) # p-value < 2.2e-16
# Conseguimos ruido más blanco, aunque no consigamos la perfecta normalidad
# Podemos continuar con el ajuste del GARCH



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Ajustar un modelo GARCH(r,s) a la volatilidad de la serie:
# Justificando los órdenes propuestos a partir de los gráficos f.a.s. y 
# f.a.p. de la serie de los residuales cuadráticos para conseguir 
# tras el ajuste de ellos un proceso de RB. En caso necesario, 
# valorar también la necesidad de incluir posibles intervenciones 
# en dicho modelo

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Ahora vamos a ver el análisis de la volatilidad de la serie
# Para ello, se define la serie de los residuales al cuadrado y 
# se adjunta el campo fecha para tener identificados los datos
residuos <- as.data.frame(cbind(bitcointEUR$Date, residuosDif))

# Se elevan al cuadrado los residuos
residuos$residuos2 <- residuos$residuosDif*residuos$residuosDif

# Graficamos con los residuos al cuadrado
graficoInicial <- ggplot(aes(x=bitcointEUR$Date, y = residuos2), data = residuos) +
  geom_line(color = '#d84519', size = 1) + 
  xlab('FECHA') + ylab('bitcointEUR')
ggplotly(graficoInicial)

# Arch-test (Homocedasticidad residuos)
# Por tanto rechazamos la H0: homocedasticidad, así que
# hay heterocedasticidad, por lo que resulta adecuado hacer un 
# ajuste de tipo GARCH
arch.test(ajuste2conOutliers)

# Comenzamos con el ajuste GARCH

Acf(residuos$residuos2, lag.max = 25, xlab = "Retardo",
    main= "Función de autocorrelación simple")

Pacf(residuos$residuos2, lag.max = 25, xlab = "Retardo",
     main = "Función de autocorrelación parcial")

# Según la Acf se debe proponer un ARCH de orden alto o un GARCH(1,1) 
# (modelo convencional) por apreciarse infinitas autocorrelaciones
# distintas de 0 en la fas y en la fap

# Ajustamos un GARCH(1,1)
garch1_1<-ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     variance.model=list(model="sGARCH", garchOrder=c(1,1)))
ugarchfit(garch1_1, ajuste2conOutliers$residuals)

# Vamos a analizar el comportamiento del ajuste
# teniendo en cuenta las salidas de los test asociados
# https://books.google.es/books?id=sf-jDwAAQBAJ&pg=PT40&lpg=PT40&dq=igarch+ugarchspec&source=bl&ots=9ly6ijVG5h&sig=ACfU3U2MyKCzvsAAsZPmIuDu_4hxDQbTFA&hl=es&sa=X&ved=2ahUKEwi-nei0r4joAhUx4YUKHcfSC_wQ6AEwAnoECAoQAQ#v=onepage&q=igarch%20ugarchspec&f=false

# P-Values de los valores estimados < 0.05 (Todos los parámetro son significativos)
# Nótese parámetro alpha p-valor < 0.05 entonces no presenta efectos asimétricos
# significativos, por lo que no hay efecto apalancamiento (se estudiará más adelante)

# P-Values Ljung-Box > 0.05 por lo que es un proceso de ruido blanco 

# Weighted ARCH LM Test > 0.05 por lo que no hay correlación entre residuos

# Nyblom stability test > 0.05 por lo que los valores constantes

# Sign Bias Test > 0.05 por lo que el modelo correctamente especificado. 
# No se requiere parámetros adicionales

# Adjusted Pearson Goodness-of-Fit Test 
# p-valor > 0.05 por lo que la distribución elegida no se sostiene (en este caso
# la distribución normal). Existen asimetrías 
# Habría que poner otra distribución como 'std'

# Ajustamos el mismo GARCH(1,1) con la distribución "STD"
# https://www.rdocumentation.org/packages/rugarch/versions/1.4-2/topics/ugarchspec-methods

garch1_1_std<-ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                         variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                         distribution = "std")
ugarchfit(garch1_1_std, ajuste2conOutliers$residuals)

# Este modelo cumple todas las hipótesis que acabamos de comentar y 
# la distribución está mejor ajustada que antes



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Analizar el efecto “apalancamiento” y, en caso de existir, 
# proponer un GARCH no lineal

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

residuos <- as.data.frame(cbind(bitcointEUR$Date,residuosDif))
residuos2 <- residuos$residuosDif*residuos$residuosDif
residuos_1 <- lag(residuos$residuosDif,1)
residuos_2 <- lag(residuos$residuosDif,2)
residuos_3 <- lag(residuos$residuosDif,3)
residuos_4 <- lag(residuos$residuosDif,4)
residuos_5 <- lag(residuos$residuosDif,5)
paraEfectoApalancamiento <- as.data.frame(cbind(residuos2,residuos_1,
                                                residuos_2,residuos_3,
                                                residuos_4,residuos_5))
modeloEfectoApalancamiento <- lm(residuos2 ~ residuos_1+residuos_2
                                 +residuos_3+residuos_4+residuos_5, 
                                 data = paraEfectoApalancamiento)
summary(modeloEfectoApalancamiento)

# Sabemos que si todos los parámetros son negativos, las noticias malas
# implicarán mayor crecimiento en la volatilidad que las buenas.
# Sin embargo, al no ser significativos (nuestro caso) no podemos
# afirmar esta relación, por lo tanto no existe efecto apalancamiento 

# Como no existe efecto apalancamiento, no sería necesario
# incluir efectos asimétricos en el modelo (GARCH no lineal)
# No obstante, a partir de la salida del GARCH (1,1) con 
# distribución "std" vemos que el modelo se puede mejorar ya que 
# los parámetros alpha1 (0.119587) y beta1 (0.879413) suman casi 1,
# es recomendable ajustar con un IGARCH
# https://www.academia.edu/35825887/Modelo_GARCH_para_la_volatilidad_de_las_criptomonedas_Bitcoin_y_Ether

# Ajuste IGARCH(1,1) con std
igarch1_1_std<-ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                          variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                          distribution = 'std')

ugarchfit(igarch1_1_std, ajuste2conOutliers$residuals)

# Se cumplen todas las hipótesis y comprobamos que el comportamiento de este modelo
# es mejor

# Probamos con otros modelos no lineales para asegurarnos si no es necesario ajustar
# un modelo de este tipo. Por ejemplo un EGARCH

# Ajuste EGARCH(1,1) con std
egarch1_1_std<-ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                          variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                          distribution = "std")

ugarchfit(egarch1_1_std, ajuste2conOutliers$residuals)

# Haciendo un modelo que captura asimetría, el p-valor del parámetro alpha > 0.05
# por lo que no sería significativo lo que nos sugiere que no hace falta 
# este tipo de modelos



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Ajustar un modelo ARIMA(p,d,q) + GARCH(r,s) en el que se haga la estimación
# conjunta de los parámetros

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Las transformaciones previas del modelo ARIMA hay que realizarlas 
# antes de ajustar el modelo conjunto 

# Logaritmo
log_bitcoin <- log(bitcointEUR$Adj.Close)

# Diferencia 
retorno_bitcoin <- diff(log_bitcoin)

# Convertir a serie temporal
retorno_bitcoin.ts <- as.ts(retorno_bitcoin)
length(retorno_bitcoin.ts)

# Al diferenciar se elimina el primer registro
# Es preciso entonces eliminar el primer registro de explicativas
outliersVariablesParaRetornos<-outliersVariables[-1,]
dim(outliersVariablesParaRetornos)
coeftest(ajuste2conOutliers)

# En nuestro caso el orden del modelo ARMA sería (0,0) por lo que el modelo
# que vamos a ajustar conjuntamente es el IGARCH (1,1) con "std" más los 
# outliers que obtuvimos previamente

# ARIMA(0,d,0) + outliers + IGARCH(1,1)

igarch1_1_outliers<-ugarchspec(mean.model = list(armaOrder = c(0,0),
                                                 external.regressors = outliersVariablesParaRetornos, 
                                                 include.mean=TRUE),
                               variance.model=list(model="iGARCH",
                                                   garchOrder=c(1,1)),
                               start.pars = list(
                                   mxreg1=0.217516,  
                                   mxreg2=-0.207483,  
                                   mxreg3=-0.175394,
                                   mu = 0.001848,
                                   omega=0.000014,
                                   alpha1=0.119685,
                                   beta1=0.880315,
                                   shape=3.323272), 
                               distribution = 'std')

ugarchfit(igarch1_1_outliers,retorno_bitcoin.ts)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Analizando si la volatilidad afecta a la estimación en media de la serie 
# (GARCH-M) y, en caso de ser así, dejando sobre la ecuación el parámetro que 
# cuantifique dicho efecto.

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# IGARCH M (Inclusión de una función de premio al riesgo) 

# Para hacer un IGARCH-M tenemos que meter el parámetro archm = TRUE, 
# que mete la volatilidad para tenerlo en cuenta en la media

igarch1_1_M<-ugarchspec(mean.model = list(armaOrder = c(0,0),
                                          external.regressors = outliersVariablesParaRetornos, 
                                          include.mean=TRUE, archm=TRUE),
                        variance.model=list(model="iGARCH",
                                            garchOrder=c(1,1)),
                        start.pars = list(
                            mxreg1=0.217516,  
                            mxreg2=-0.207483,  
                            mxreg3=-0.175394,
                            mu = 0.001849,
                            omega=0.000014,
                            alpha1=0.119625,
                            beta1=0.880375,
                            shape=3.318982
                          ), distribution = 'std')

ugarchfit(igarch1_1_M,retorno_bitcoin.ts)

# Como el parámetro archm tiene un p-valor > 0.05,
# No se necesita este parámetro por lo que no sería conveniente 
# introducir premio al riesgo


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# Calculando una predicción de la serie en media para la última
# semana junto con un intervalo de confianza mediante simulaciones
# hechas con el modelo final.

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# El modelo está ajustado sin utilizar los datos de la última semanas, 
# es decir desde "2014-10-22" hasta "2020-02-29"

# Cargamos los datos de la última semana ("2020-03-01" - "2020-03-07")
# para una posible comparación 
bitcointEUR_ultimos7 <- read.csv("BTC-EUR-2014_7dias.csv")
bitcointEUR_ultimos7 <- bitcointEUR_ultimos7[c('Date','Adj.Close')]
bitcointEUR_ultimos7$Date <- as.Date(bitcointEUR_ultimos7$Date,format="%Y-%m-%d")

# Predicciones para el mejor modelo (IGARCH(1,1) + ARMA(0,0) + OUTLIERS)
# Lamentablemente, no podemos predecir con este modelo porque internamente
# R tiene un "error" al predecir el intervalo de confianza
# del parámetro estimado beta1, así que hacemos este apartado
# con un modelo GARCH(1,1) + ARMA(0,0) + Outliers

# GARCH(1,1) + ARMA(0,0) + OUTLIERS

garch1_1_outliers<-ugarchspec(mean.model = list(armaOrder = c(0,0),
                                          external.regressors = outliersVariablesParaRetornos, 
                                          include.mean=TRUE, archm=FALSE),
                        variance.model=list(model="sGARCH",
                                            garchOrder=c(1,1)),
                        start.pars = list(
                            mxreg1=0.217516,  
                            mxreg2=-0.207483,  
                            mxreg3=-0.175394,
                            mu = 0.001847,
                            omega=0.000015,
                            alpha1=0.119587,
                            beta1=0.879413,
                            shape=3.334947), 
                        distribution = 'std')

modelo_predecir <- ugarchfit(garch1_1_outliers,retorno_bitcoin.ts)

# Se prolonga el valor de las explicativas (outliers)
outliersVariablesParaRetornos_7=tail(outliersVariablesParaRetornos,7)

# Hacemos predicciones a horizonte 7 días
prediccion_modelo <- ugarchforecast(modelo_predecir,n.ahead=7,data=retorno_bitcoin.ts,
                                    external.forecasts = list(mregfor=outliersVariablesParaRetornos_7))

# Visualizamos los pronósticos de volatilidad fuera de la muestra
# ya que implican predicciones del rendimiento que no se han utilizado al 
# estimar el modelo
prediccion_modelo 

# Simulaciones. No basamos en las siguientes referencias
# https://cran.r-project.org/web/packages/qrmtools/vignettes/ARMA_GARCH_VaR.html
# https://mran.microsoft.com/snapshot/2015-11-17/web/packages/qrmtools/vignettes/ARMA_GARCH_VaR.html

# Hacemos 1000 predicciones con el modelo ajustado
n = 1000

# Especificamos el modelo que vamos a utilizar
fspec <- getspec(modelo_predecir) 

# Conjunto de parámetros del modelo ajustado
setfixed(fspec) <- as.list(coef(modelo_predecir)) 

# Número de pasos a predecir (Horizonte 7)
m = 7

# Nivel de significación del intervalo de confianza
alpha = 0.05

# Extraer los resultados de las predicciones 
# Extraer X_t predicha (= media condicionada mu_t; Nota: E[Z] = 0)
mu.predict <- fitted(prediccion_modelo) 
# Extraer sigma_t predicha
sig.predict <- sigma(prediccion_modelo)
# Extraer VaR_alpha predicho
VaR.predict <- as.numeric(quantile(prediccion_modelo, probs = alpha)) 
# Extraemos los grados de libertad del modelo predicho
nu <- modelo_predecir@fit$coef[["shape"]]

# Bootstraping para construir los intervalos de confianza para VaR_alpha
B <- 1000
# Simular futuros caminos
X.sim.obj <- ugarchpath(fspec, n.sim = m, m.sim = B) 

# Calcular VaR_alpha simuladas y sus correspondientes intervalos de confianza (simulados)
# Nota: Cada serie es ahora una matriz (m, B) (cada columna es un camino de longitud m)

# Extraemos X_t simulada
X.sim <- fitted(X.sim.obj)
# Extraemos sigma_t
sig.sim <- sigma(X.sim.obj) 
# Extraer epsilon_t
eps.sim <- X.sim.obj@path$residSim
VaR.sim <- (X.sim - eps.sim) + sig.sim * sqrt((nu-2)/nu) * qt(alpha, df = nu)
# Calculamos el intervalo de confianza predicho
VaR.CI <- apply(VaR.sim, 1, function(x) quantile(x, probs = c(0.025, 0.975)))
VaR.CI
