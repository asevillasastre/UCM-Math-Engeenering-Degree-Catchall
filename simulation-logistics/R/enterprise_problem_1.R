
#Calculo la funci�n de densidad
dtriang=function(x){
  (x-2)*(2<=x & x<3)+(4-x)*(3<=x & x<4)
}

rtriang=function(n){
  muestrax=numeric(n)
  u=runif(1)
  for(i in 1:n){
    muestrax[i]=ifelse(u[i]<0.5,2+sqrt(2*u[i]),4-sqrt(2*(1-u[i])))
  }
  return(muestrax)
}

muestrax=rtriang(100)
hist(muestrax,freq=FALSE)
curve(dtriang(x),add=TRUE,col="red",lwd=2)

#Calculo la funci�n de distribuci�n
ptriang=function(x){
  (x^2/2-2*x+2)*(2<=x & x<3)+(4*x-x^2/2-7)*(3<=x & x<4)+1*(x>=4)
}

curve(ptriang(x),col="red",lwd=2,xlim=c(0,5))
plot(ecdf(muestrax),add=TRUE)

ks.test(muestrax,"ptriang")

Llegada=function(N,TM,TS,TANT,SUMA){
  N=N+1
  #Genero DL
  DL=runif(1,3,5)
  TL=TM+DL
  TP=0
  if(N==1){
    TP=ifelse(runif(1)<0.35,1,2) #Indica el tipo de producto
    if(TP==1){
      DS1=rnorm(1,4,1)
      TS=TM+DS1
    }else{
      DS2=rtriang(1)
      TS=TM+DS2
    }
  }
  SUMA=SUMA+(N-1)*(TM-TANT)
  TANT=TM
  return(c(N,TL,TS,TANT,TP,SUMA))
}#end function

Servicio=function(N,TM,TANT,TP,SUMA){
  N=N-1
  if(N==0){
    TS=Inf
  }else{
    if(TP==1){
      DS1=rnorm(1,4,1)
      TS=TM+DS1
    }else{
      DS2=rtriang(1)
      TS=TM+DS2
    }
  }
  SUMA=SUMA+(N+1)*(TM-TANT)
  TANT=TM
  return(c(N,TS,TANT,SUMA))
}#end function

#########################
#Notaci�n
#N=n�mero de pedidos
#TM=reloj de simulaci�n
#TL=instante pr�ximo pedido
#TS=instanto pr�ximo final de servicio
#Tmax=tiempo m�ximo de simulaci�n
#########################
#Inicio
N=0
TM=0
TS=Inf
Tmax=40
SUMA=0
TANT=0

set.seed(101)
#Genero el pr�ximo pedido
DL=runif(1,3,5)
TL=DL

while(TM<Tmax){
  #Actualizo el reloj de simulaci�n
  TM=min(TL,TS)
  
  Estado=-1*(TM==TL)+1*(TM==TS)
  if(Estado==-1){
    k=Llegada(N,TM,TS,TANT,SUMA)
    k[1]=N
    k[2]=TL
    k[3]=TS
    k[4]=TANT
    k[5]=TP
    k[6]=SUMA
  }
  if(Estado==1){
    k=Servicio(N,TM,TANT,TP,SUMA)
    k[1]=N
    k[2]=TS
    k[3]=TANT
    k[4]=SUMA
  }
}#end while
print(c("El programa ha parado en el instante"),TM)
N_ult=ifelse(Estado==-1,N-1,N)
print(c("En el momento de parada hab�a ",N_ult,"pedidos sin atender"))
print(c("El n�mero medio de personas en el sistema ha sido",SUMA/TM))