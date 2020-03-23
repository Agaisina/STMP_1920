# Práctica de Series Temporales

https://es.finance.yahoo.com/quote/BTC-EUR/

- Gema Correa Fernández
- Ágata De Isidro Navarro
- Alberto Ruíz-Arteaga González

## Parte de 2 personas

Para la parte realizada que es común para el grupo del dos personas tenemos:
- Bitcoin_EUR.R: fichero en R con las soluciones y explicaciones
- BTC-EUR-2014_7dias.csv: CSV con los datos de Bitcoin sin los últimos siete días
- BTC-EUR-2014_sinultimos7_dias.csv:  CSV con los datos de Bitcoin con sólo los últimos siete días a predecir

## Parte de 3 personas

Para la parte realizada al ser un grupo de tres personas:
- Petroleo.R: fichero que contiene el ajuste del ARIMA para la serie del petróleo, con la metodología Box-Jenkins completamente detallada y justificado cada paso
- brentPriceData.sas7bdat: serie a añadir en nuestro auste ARMA+GARCH
- bitcoins-petroleo.sas: este fichero obtiene la correlación cruzada entre BTC y petróleo para ver si se puede aplicar una función de transferencia
- salida-bitcoins-petroleoSAS.pdf: salida en PDF de la ejecución de bitcoins-petroleo.sas
- tabla.csv: tabla en formato CSV que tiene en la primera columna fecha, en la segunda precio bitcoin y en la tercera precio del petróleo
- tabla.sas7bdat: tabla en formato sas7bdat que tiene en la primera columna fecha, en la segunda precio bitcoin y en la tercera precio del petróleo
- garch-variable-exogena.sas: codigo en SAS que ajusta el modelo GARCH obtenido en la primer parte con una variable exógena (petróleo)
- salida-garch-variable-exogenaSAS.pdf: salida en PDF de la ejecución de garch-variable-exogena.sas

Una vez obtenido el modelo ARIMA que modelizará la serie temporal que hace referencia a los datos históricos del precio del petróleo, se intenta expresar esta serie temporal `X_t` como variable explicativa exógena de la serie temporal `Y_t` que hace referencia al precio del Bitcoin. Si bien, como se puede observar en el gráfico de correlaciones cruzadas de ambas series, no obtenemos ningún retardo significativo, por lo que no hay motivos para pensar que podemos aplicar un modelo de función de transferencia entre dichas series.

Téngase en cuenta que, si hubiéramos obtenido algún tipo de significancia en dicho gráfico. La forma de actuar sería la siguiente:
  1. El primer retardo significativo nos proporciona la b.
  2. El número de retardos en los que se acentúa dicha significatividad nos proporcionaría la m.
  3. El número de retardos en los que decae la significatividad hasta que deja de ser importante nos proporciona la n.

Una vez obtenidos dichos parámetros, especificaríamos dicha relación entre nuestras series mediante el siguiente código en SAS:

~~~
proc arima data=<tabla_entrada>;
  identify var = <Y_t> (<diferencias_Yt>)
  crosscorr = (<variable_exogena><diferencias_Xt>);
  estimate p=() q=();
  input = b$(1 ... m) / (1 ... n)<variable_exogena>
run;
quit;
~~~
