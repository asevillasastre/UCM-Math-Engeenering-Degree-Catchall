###programa principal

####subrutinas####

LAMBDA1 = 1/14
LAMBDA2 = 3/13
LAMBDA3 = 3/28
LAMBDA4 = 1/3
MU1 = 180
MU2 = 120
MU3 = 350
MU4 = 60

Llegada1 = function(NMAX,N1,TS1,TM){
  NMAX = NMAX - 1
  N1 = N1 + 1
  if (N1 >= 10 & TS1 == Inf){
    TS1 = TM + rnorm(1,MU1,30)
  }
  TL1 = TM + rexp(1,MU1)
  if (NMAX > 0) {TL1 = TM + rexp(1,LAMBDA1)}
  if (NMAX == 0) {TL1 = Inf}
  return(c(NMAX,N1,TS1,TL1))}

Llegada2 = function(NMAX,N2,TS2,TM){
  NMAX = NMAX - 1
  N2 = N2 + 1
  if (N2 >= 15 & TS2 == Inf){
    TS2 = TM + rnorm(1,MU2,30)
  }
  TL2 = TM + rexp(1,MU2)
  if (NMAX > 0) {TL2 = TM + rexp(1,LAMBDA2)}
  if (NMAX == 0) {TL2 = Inf}
  return(c(NMAX,N2,TS2,TL2))}

Llegada3 = function(NMAX,N3,TS3,TM){
  NMAX = NMAX - 1
  N3 = N3 + 1
  if (N3 >= 20 & TS3 == Inf){
    TS3 = TM + rnorm(1,MU3,30)
  }
  TL3 = TM + rexp(1,MU3)
  if (NMAX > 0) {TL3 = TM + rexp(1,LAMBDA3)}
  if (NMAX == 0) {TL3 = Inf}
  return(c(NMAX,N3,TS3,TL3))}

Llegada4 = function(NMAX,N4,TS4,TM){
  NMAX = NMAX - 1
  N4 = N4 + 1
  if (N4 >= 5 & TS4 == Inf){
    TS4 = TM + rnorm(1,MU4,30)
  }
  TL4 = TM + rexp(1,MU4)
  if (NMAX > 0) {TL4 = TM + rexp(1,LAMBDA4)}
  if (NMAX == 0) {TL4 = Inf}
  return(c(NMAX,N4,TS4,TL4))}

Salida1 = function(NMAX,N1,TS1,TM,ACABADAS,ACABADAS1){
  NMAX = NMAX + 10
  N1 = N1 - 10
  ACABADAS = ACABADAS + 10
  ACABADAS1 = ACABADAS1 + 10
  if (N1 < 10){TS1 = Inf}
  if (N1 >= 10){TS1 = TM + rnorm(1,MU1,30)}
  if (NMAX > 0) {TL2 = TM + rexp(1,LAMBDA2); TL1 = TM + rexp(1,LAMBDA1); TL3 = TM + rexp(1,LAMBDA3); TL4 = TM + rexp(1,LAMBDA4)}
  if (NMAX == 0) {TL1 = Inf}
  return(c(NMAX,N1,TS1,ACABADAS,TL1,ACABADAS1))
}

Salida2 = function(NMAX,N2,TS2,TM,ACABADAS,ACABADAS2){
  NMAX = NMAX + 15
  N2 = N2 - 15
  ACABADAS = ACABADAS + 15
  ACABADAS2 = ACABADAS2 +15
  if (N2 < 15){TS2 = Inf}
  if (N2 >= 15){TS2 = TM + rnorm(1,MU2,30)}
  if (NMAX > 0) {TL2 = TM + rexp(1,LAMBDA2); TL1 = TM + rexp(1,LAMBDA1); TL3 = TM + rexp(1,LAMBDA3); TL4 = TM + rexp(1,LAMBDA4)}
  if (NMAX == 0) {TL1 = Inf}
  return(c(NMAX,N2,TS2,ACABADAS,TL2,ACABADAS2))
}

Salida3 = function(NMAX,N3,TS3,TM,ACABADAS,ACABADAS3){
  NMAX = NMAX + 20
  N3 = N3 - 20
  ACABADAS = ACABADAS + 20
  ACABADAS3 = ACABADAS3 +20
  if (N3 < 20){TS3 = Inf}
  if (N3 >= 20){TS3 = TM + rnorm(1,MU3,30)}
  if (NMAX > 0) {TL2 = TM + rexp(1,LAMBDA2); TL1 = TM + rexp(1,LAMBDA1); TL3 = TM + rexp(1,LAMBDA3); TL4 = TM + rexp(1,LAMBDA4)}
  if (NMAX == 0) {TL3 = Inf}
  return(c(NMAX,N3,TS3,ACABADAS,TL3,ACABADAS3))
}

Salida4 = function(NMAX,N4,TS4,TM,ACABADAS,ACABADAS4){
  NMAX = NMAX + 5
  N4 = N4 - 5
  ACABADAS = ACABADAS + 5
  ACABADAS4 = ACABADAS4 + 5
  if (N4 < 5){TS4 = Inf}
  if (N4 >= 5){TS4 = TM + rnorm(1,MU4,30)}
  if (NMAX > 0) {TL2 = TM + rexp(1,LAMBDA2); TL1 = TM + rexp(1,LAMBDA1); TL3 = TM + rexp(1,LAMBDA3); TL4 = TM + rexp(1,LAMBDA4)}
  if (NMAX == 0) {TL4 = Inf}
  return(c(NMAX,N4,TS4,ACABADAS,TL4,ACABADAS4))
}

#inicializamos las variables
NMAX = 3000
TMAX = 21600
TM = 0
TS1 = Inf
TL1 = LAMBDA1
TS2 = Inf
TL2 = LAMBDA2
TS3 = Inf
TL3 = LAMBDA3
TS4 = Inf
TL4 = LAMBDA4
N1 = 0
N2 = 0
N3 = 0
N4 = 0
ACABADAS = 0
ACABADAS1 = 0
ACABADAS2 = 0
ACABADAS3 = 0
ACABADAS4 = 0
LIBRES = 0
TANT = 0

while(TM < TMAX){
  #Actualizamos el reloj de simulación
  TANT = TM
  TM = min(TL1,TS1,TL2,TS2,TS3,TL3,TS4,TL4)
  LIBRES = LIBRES + (NMAX)*(TM-TANT)
  if (TM == TL1) {
    K = Llegada1(NMAX,N1,TS1,TM)
    NMAX = K[1]
    N1 = K[2]
    TS1 = K[3]
    TL1 = K[4]
  }
  if (TM == TL2) {
    K = Llegada2(NMAX,N2,TS2,TM)
    NMAX = K[1]
    N2 = K[2]
    TS2 = K[3]
    TL2 = K[4]
  }
  if (TM == TL3) {
    K = Llegada3(NMAX,N3,TS3,TM)
    NMAX = K[1]
    N3 = K[2]
    TS3 = K[3]
    TL3 = K[4]
  }
  if (TM == TL4) {
    K = Llegada4(NMAX,N4,TS4,TM)
    NMAX = K[1]
    N4 = K[2]
    TS4 = K[3]
    TL4 = K[4]
  }
  if (TM == TS1) {
    K = Salida1(NMAX,N1,TS1,TM,ACABADAS,ACABADAS1)    
    NMAX = K[1]
    N1 = K[2]
    TS1 = K[3]
    ACABADAS = K[4]
    TL1 = K[5]
    ACABADAS1 = K[6]
  }
  if (TM == TS2) {
    K = Salida2(NMAX,N2,TS2,TM,ACABADAS,ACABADAS2)    
    NMAX = K[1]
    N2 = K[2]
    TS2 = K[3]
    ACABADAS = K[4]
    TL2 = K[5]
    ACABADAS2 = K[6]
  }
  
  if (TM == TS3) {
    K = Salida3(NMAX,N3,TS3,TM,ACABADAS,ACABADAS3)    
    NMAX = K[1]
    N3 = K[2]
    TS3 = K[3]
    ACABADAS = K[4]
    TL3 = K[5]
    ACABADAS3 = K[6]
  }
  
  if (TM == TS4) {
    K = Salida4(NMAX,N4,TS4,TM,ACABADAS,ACABADAS4)    
    NMAX = K[1]
    N4 = K[2]
    TS4 = K[3]
    ACABADAS = K[4]
    TL4 = K[5]
    ACABADAS4 = K[6]
  }
}


ACABADAS
ACABADAS/3000
TM
N1
N2
N3
N4
TS1
TS2
TS3
TS4
NMAX
ACABADAS1
ACABADAS2
ACABADAS3
ACABADAS4
LIBRES
LIBRES/21600
