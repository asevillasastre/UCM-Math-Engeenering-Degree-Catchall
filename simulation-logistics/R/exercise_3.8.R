#Ejercicio 8
#help(rbeta)
curve(dbeta(x,4,3),xlim=c(-0.5,1.5))
c=60*(2/5)^2*(1-2/5)^3
curve(c*dunif(x),add=TRUE,col="red")

contador=0
Fin=1000
muestrax=nuemeric()
while(length(muestrax)<Fin){
  y=runif(1)
  x=runif(1)
  if(u<=(y^2*(1-y)^3)/((2/5)^2*(1-2/5)^3)){muestrax=c(muestrax,y)}
  contador=contador+1
}

#El número de iteraciones teórico es c
c
print(c("El número de iteracones estimado es",contador/1000))
#La tasa teórica de aceptación es 1/c
1/c
print(c("La tasa de aceptación estimada es",1000/contador))

hist(muetrax,freq=FALSE)
curve(dbeta(x,3,4),add=TRUE,col="red",lwd=3)

ks.test(muestrax,"pbeta",3,4)