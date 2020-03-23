**libname practica "C:\Users\usuario\Documents\Máster TECI\STMP\data";

proc import datafile='C:\Users\usuario\Documents\Máster TECI\STMP\data\tabla.csv' out=tabla dbms=csv replace;
run;

proc arima data = work.tabla;
	identify var = oil_price(1);
    estimate p=(1) q=(1) plot;
    identify var = BTC_price(1)
	crosscorr = oil_price(1); **(número de diferencias de vuestro modelo);
    estimate p=(0) q=(0) noint; 
	** p = (número de p de vuestro modelo);
	** q = (número de q de vuestro modelo);
	**(el noint en caso de que no tenga intercept vuestro modelo);
run;
quit;
