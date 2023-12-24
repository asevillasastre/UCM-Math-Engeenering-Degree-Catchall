setwd("C:/Users/Sempai/Desktop/SLOG")
datos = read.table("SIMLOG7 datos_ajuste.txt", head = TRUE, dec = ",")
names(datos)
u = datos$u; v = datos$v; w = datos$w; x = datos$x; y = datos$y; z = datos$z

par(mfrow = c(1,2))
hist(x, freq = FALSE)
L=1/mean(x)
curve(dexp(x,L), add=TRUE, col = "green", lwd=3)
ks.test(x, "pexp", L)

hist(y, freq=FALSE, ylim=c(0,0.25))
L=mean(y); LL = sd(y);
curve(dnorm(x,L,LL), add=TRUE, col="red", lwd=3)
ks.test(y,"pnorm",L,LL)