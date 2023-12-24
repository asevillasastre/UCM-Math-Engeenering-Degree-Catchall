#############################
### Antonio Sevilla ej4h4 ###
#############################

#Generamos los instantes de fallo mediante una triangular
instante_fallo = function(){
  #Generamos una uniforme
  u=runif(1,0,1)
  #Aplicando el método de la transformada inversa
  result = ifelse(u<1/3, (3+sqrt(3*u))/4, (sqrt(u+(88/12))+(sqrt(3)/2))*3/sqrt(3))
  return (result)
}

###################################################

Llegada = function(robot,N,R,A,SUMA,TM,TFR,TANT){
  N=N+1
  
  if(N==1){
    u2=runif(1,0, 1)
    if(robot==1){DS=-10*log(u2)} else{DS=-(60/7)*log(u2)}
    if(R==0){TS=TM+DS}; if(R==1){TS=TFR+DS}
  }
  else{
    if(R==1){A=A+TM-TANT}
  }
  u1=runif(1,0,1)
  DL=-12*log(u1)
  TL=TM+DL
  
  SUMA=SUMA+(N-1)*(TM-TANT)
  TANT=TM
  return(c(N,A,SUMA,TS,TL,TANT))
}

Salida = function(robot,N,SUMA,TM,TANT){
  N=N-1
  
  if(N==0){TS=Inf}
  else{
    u2=runif(1,0,1)
    if(robot==1){DS=-10*log(u2)} else{DS=-(60/7)*log(u2)}
    TS=TM+DS
  }
  
  SUMA=SUMA+(TM-TANT)*N;
  TANT=TM
  return(c(N,SUMA,TS,TANT))
}

Averia = function(N,A,SUMA,TM,TANT){
  if(N==0){A=A+TM-TANT}
  R=1
  
  TAR=Inf
  DFR=15
  TFR=TM+DFR

  SUMA=SUMA+(TM-TANT)*(N+1)
  TANT=TM
  return(c(N,A,SUMA,TAR,TFR,TANT))
}

Reparacion = function(A,SUMA,TM,TANT){
  R=0
  
  DAR=instante_fallo()
  TFR=Inf
  TAR=TM+DAR
  
  SUMA=SUMA+(TM-TANT)*N
  A=TM-TANT+A
  TANT=TM
  return(c(R,A,SUMA,TANT,TFR))
}

##########################################

#DL: instante entre llegadas
#DS: instante de servicio
#DAR: instante entre averías
#TL: Instante de la próxima llegada
#TS: Instante de la próxima salida
#TAR: Instante de la próxima avería
#TFR: Instante de la próxima reparacion
#R: Booleano que indica la operatividad
#A: instante inactivo

TM=0
TANT=0
SUMA=0
TMAX=120

N=0
TS=Inf
TFR=Inf

R=0
A=0

u1=runif(1,0,1)
DL=-12*log(u1)
TL=DL
DAR=instante_fallo()
TAR=DAR

while(TM<TMAX){
  TM=min(TL,TS,TAR,TFR)
  
  if(TM==TL){k=Llegada(robot,N,R,A,SUMA,TM,TFR,TANT)
  N=k[1]
  A=k[2]
  TS=k[4]
  SUMA=k[3]
  TL=k[5]
  TANT=k[6]}
  
  if(TM==TS){k=Salida(robot,N,SUMA,TM,TANT)
  N=k[1]
  SUMA=k[2]
  TS=k[3]
  TANT=k[4]}
  
  if(TM==TAR){k=Averia(N,A,SUMA,TM,TANT)
  N=k[1]
  A=k[2]
  SUMA=k[3]
  TAR=k[4]
  TFR=k[5]
  TANT=k[6]}
  
  if(TM==TFR){k=Reparacion(A,SUMA,TM,TANT)
  R=k[1]
  A=k[2]
  SUMA=k[3]
  TFR=k[5]
  TANT=k[4]}}

A = A+(TM-TANT)*(1-R)
if(robot==1) {coste = (2*1000)+(2000*(120-A)/60)+(8000*SUMA/60)} else {coste = (2*1500)+(4000*(120-A)/60)+(8000*SUMA/60)}
