Xrm(list=ls())
library(MASS)
# GENERATE MATRIX Z[,sim], DEFINISI AWALNYA Z=MATRIX(0,r,e)
# GENERATE VECTOR V[sim], DEFINISI AWAL V=NULL
# NOTASI YG DIGENERATE JGN SMPAI BENTROK DG FUNGSI YANG DIBUAT

nsim=1000
n=100
x=seq(1,n)
e=matrix(0,n,nsim)
y=matrix(0,n,nsim)
S=NULL
yhat5=matrix(0,n,nsim)
rmse.yhat5=NULL
yhat10=matrix(0,n,nsim)
rmse.yhat10=NULL
yhat30=matrix(0,n,nsim)
rmse.yhat30=NULL

for(sim in 1 : nsim){
  set.seed(sim*10^6)
  e[,sim]=rnorm(n, 0, 0.3)
  y[,sim]=log(x)+e[,sim]
  S[sim]=var(log(x))/var(e[,sim])
  yhat5[,sim]=ksmooth(x,y[,sim],"normal",bandwidth=5)$y
  rmse.yhat5[sim]=sqrt(mean((y[,sim]-yhat5[,sim])^2))
  yhat10[,sim]=ksmooth(x,y[,sim],"normal",bandwidth=10)$y
  rmse.yhat10[sim]=sqrt(mean((y[,sim]-yhat10[,sim])^2))
  yhat30[,sim]=ksmooth(x,y[,sim],"normal",bandwidth=30)$y
  rmse.yhat30[sim]=sqrt(mean((y[,sim]-yhat30[,sim])^2))
 }
y
S
SNR <- mean(S)
SNR
rmse.yhat5
rmse5 <- mean(rmse.yhat5)
rmse5
rmse10 <- mean(rmse.yhat10)
rmse10
rmse30 <- mean(rmse.yhat30)
rmse30

# usahakan di bawah 10, semakin kecil SNR model semakin sulit untuk ditaksir, SNR besar model mudah

# plot sampel pertama untuk 3 bandwith
plot(x,y[,1])

lines(ksmooth(x,y[,1],"normal",bandwidth=5),col="red",lwd=3)
lines(ksmooth(x,y[,1],"normal",bandwidth=10),col="green", lwd=3)
lines(ksmooth(x,y[,1],"normal",bandwidth=30),col= "blue",lwd=3)


par(mfrow=c(1,2))
plot(x, y[,1])
lines(ksmooth(x,y[,1],"normal",bandwidth=10),col=2, lwd=3)
plot(x, y[,1000])
lines(ksmooth(x,y[,1000],"normal",bandwidth=10),col=3, lwd=3)







