#Ejercicio 3
install.packages("rmutil")
library(rmutil)

set.seed(1)
n=100
u1=runif(100)
u2=runif(100)
x=ifelse(u1<1/2,log(u2),-log(1-u2))

hist(x,freq=FALSE,ylim=c(0,0.6))
curve(dlaplace(x),add=TRUE,col="red",lwd=2)

ks.test(x,"plaplace")
