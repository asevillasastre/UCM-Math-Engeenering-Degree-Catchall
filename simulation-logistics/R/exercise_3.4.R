#Ejercicio 4 de la Hoja 3

ej_4_i=function(m,n){
  u=runif(1)
  x=m+floor((n-m+1)*u)
  return(x)
}

ej_4=funcion(n,m1,n1,m2,n2){
  u=runif(n)
  x=numeric()
  for(i in 1:n){
    x[i]=ifelse(u[i]<1/4,ej_4_i(m1,n1),ej_4_i(m2,n2))
  }
  return(x)
}

n=100000
m1=1
n1=5
m2=6
n2=10

x=ej_4(m,m1,n1,m2,n2)
hist(x,freq=FALSE,breas=c(0.5:10.5))
  qaw