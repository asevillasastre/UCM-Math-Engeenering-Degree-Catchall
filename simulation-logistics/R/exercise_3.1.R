#Ejercicio 1 (Hoja 3)
#b) Método de la inversa
dy=function(x){
4/3*x*(1/2<x & x<1) + 4/3*(2-x)*(1<=x & x<=3/2)
}

#Función generadora de n observaciones de la truncada
ry=function(n){
u=runif(n)
x=ifelse(u<1/2,sqrt((u+1/6)*3/2),2-sqrt(7/4-3/2*u))
return(x)
}

muestray=ry(1000)
hist(muestray,freq=FALSE)
curve(dy(x),add=TRUE,col=2,lwd=2)

py=function(x){
(2/3*x^2-1/6)*(1/2<=x & x<1)-2/3*((x-2)^2-7/4)*(1<=x & x<3/2)+1*(x>=3/2)
}

ks.test(muestray,"py")

#b) Trunco a partir de la original
#Repasar práctia triangular
rtriang=function(n){
u=runif(n)                                 
x=ifelse(u<=1/2,sqrt(2*u),2-sqrt(2*(1-u)))
}

n=1000
muestray2=numeric()
while(length(muestray2)<n){
aux=rtriang(1)
if(1/2<aux & aux<3/2){muestray2=c(muestray2,aux)}
}

hist(muestray2,freq=FALSE)
curve(dy(x),add=TRUE,col=2,lwd=2)

ks.test(muestray2,"py")

