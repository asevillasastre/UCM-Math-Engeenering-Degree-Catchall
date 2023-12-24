##########################################
### EJERCICIO 2 HOJA 1 ANTONIO SEVILLA ###
##########################################

#Espero no sea un inconveniente entregar el ejercicio implementado directamente en R.
#Si es necesario lo puedo pasar a pseudocódigo.

#primeramente defino la función de densidad triangular dada
dtriang = function(x){(x-2)*(2<=x & x<3) + (4-x)*(3<=x & x<4)}

rtriang=function(n){
  muestrax=numeric(n); u=runif(1);
  for(i in 1:n){muestrax[i]=ifelse(u[i]<0.5,2+sqrt(2*u[i]),4-sqrt(2*(1-u[i])))}
  return(muestrax)}

#ahora calculo la función de distribución
ptriang=function(x){(x^2/2-2*x+2)*(2<=x & x<3)+(4*x-x^2/2-7)*(3<=x & x<4)+1*(x>=4)}

#defino los metodos llegada...
Llegada=function(N,TM,TS,TANT,SUMA){
  N=N+1
  #genero DL
  DL=runif(1,3,5); TL=TM+DL; TP=0
  if(N==1){
    #para obtener el tipo de producto
    TP=ifelse(runif(1)<0.35,1,2)
    if(TP==1){DS1=rnorm(1,4,1); TS=TM+DS1}
    else{DS2=rtriang(1); TS=TM+DS2}
  }
  SUMA=SUMA+(N-1)*(TM-TANT); TANT=TM
  return(c(N,TL,TS,TANT,TP,SUMA))
}

#... y servicio
Servicio=function(N,TM,TANT,TP,SUMA){
  N=N-1
  if(N==0){TS=Inf}
  else{
    if(TP==1){DS1=rnorm(1,4,1); TS=TM+DS1}
    else{DS2=rtriang(1); TS=TM+DS2}
  }
  SUMA=SUMA+(N+1)*(TM-TANT); TANT=TM
  return(c(N,TS,TANT,SUMA))
}

#procedo con la simulacion siendo:
#N=número de pedidos
#TM=reloj de simulación
#TL=instante próximo pedido
#TS=instanto próximo final de servicio
#Tmax=tiempo máximo de simulación
N=0
TM=0
TS=Inf
Tmax=40
SUMA=0
TANT=0
DL=runif(1,3,5)
TL=DL

while(TM<Tmax){
  #actualizo el reloj de simulación
  TM=min(TL,TS)
  #cambio estado
  Estado = -1*(TM==TL)+1*(TM==TS)
  #si está en -1 se produce una llegada
  if(Estado==-1){
    #invoco al metodo llegada y cambio los datos
    k=Llegada(N,TM,TS,TANT,SUMA)
    k[1]=N
    k[2]=TL
    k[3]=TS
    k[4]=TANT
    k[5]=TP
    k[6]=SUMA
  }
  #ídem para servicio si Estado == -1
  if(Estado==1){
    k=Servicio(N,TM,TANT,TP,SUMA)
    k[1]=N
    k[2]=TS
    k[3]=TANT
    k[4]=SUMA
  }
}
