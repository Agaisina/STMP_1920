libname practica "C:\Users\usuario\Documents\Máster TECI\STMP\data";

proc import datafile='C:\Users\usuario\Documents\Máster TECI\STMP\data\tabla.csv' out=tabla dbms=csv replace;
run;

proc arima data=WORK.TABLA;
     identify var = BTC_Price(1);
     estimate plot noint;
     forecast out=PRACTICA.salidaARIMA id = Date;
run;
quit;

data PRACTICA.salidaARIMA;
     set PRACTICA.salidaARIMA;
     residualCuadrado = residual**2;
run;

data concatenation;
	set PRACTICA.salidaARIMA WORK.TABLA ;
run;


proc autoreg data=work.concatenation ;	
	model residual=/ garch=(q = (1), p = (1)) noint;
	hetero oil_price;
run;
quit;
