#Ejercicio 6
#Generar 1000 valores de Z=N(0,1)

dx=function(x){
  #2*dnorm(x)
  2/sqrt(2*pi)*exp(-x^2/2)
}

c=2/sqrt(2*pi)*exp(1/2)
curve(dx(x),xlim=c(0,10),ylim=c(0,2))
curve(c*dexp(x),add=TRUE,col="red",lwd=3)

contador=0
Fin=1000
muestrax=numeric()
while(length(muestrax)<Fin){
  y=rexp(1)
  u=runif(1)
  #if(u<=dx(y)/(c*dexp(y))){muestrax=c(muestrax,y)}
  if(u<=exp(-y^2+y-1/2)){muestrax=c(muestrax,y)}
  contador=contador+1
}

hist(muestrax,freq=FALSE)
curve(dx(x),add=TRUE,col="red",lwd=3)

pnorm2=function(x){
  pnorm(x)*2-1
}
ks.test(muestrax,"pnorm2")

muestraz=numeric()
for(i in 1:Fin){
  muestraz[i]=ifelse(runif(1)<1/2,muestrax[i],-muestrax[i])
}

hist(muestraz,freq=FALSE)
curve(dnorm(x),add=TRUE,col="red",lwd=3)

ks.test(muestraz,"pnorm")