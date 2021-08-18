rm(list=ls())

n=100
x = seq(1,n)

nsim=1000
y = matrix(0, n,nsim)
e = matrix(0, n,nsim)
yhat_NW5 = matrix(0, n,nsim)
rmse_yhat_NW5 = NULL

yhat_NW10 = matrix(0, n,nsim)
rmse_yhat_NW10 = NULL

yhat_NW30 = matrix(0, n,nsim)
rmse_yhat_NW30 = NULL

snr = NULL
for(sim in 1:nsim){
	set.seed(sim*10^6)
	e[,sim] = rnorm(n,0,0.3)
	y[,sim] = log(x)+e[,sim]
	snr[sim] = var(log(x))/var(e[,sim])
	
	yhat_NW5[,sim] = ksmooth(x, y[,sim], kernel="normal", 
				bandwidth=5, range.x=range(x), x.points=x)$y
	rmse_yhat_NW5[sim] = sqrt(sum((y[,sim]-yhat_NW5[,sim])^2)/n)
	
	yhat_NW10[,sim] = ksmooth(x, y[,sim], kernel="normal", 
				bandwidth=10, range.x=range(x), x.points=x)$y
	rmse_yhat_NW10[sim] = sqrt(sum((y[,sim]-yhat_NW10[,sim])^2)/n)
	
	yhat_NW30[,sim] = ksmooth(x, y[,sim], kernel="normal", 
				bandwidth=30, range.x=range(x), x.points=x)$y
	rmse_yhat_NW30[sim] = sqrt(sum((y[,sim]-yhat_NW30[,sim])^2)/n)

}

mean_snr = mean(snr)
mean_snr

mean_rmse_yhat_NW5 = mean(rmse_yhat_NW5)
mean_rmse_yhat_NW10 = mean(rmse_yhat_NW10)
mean_rmse_yhat_NW30 = mean(rmse_yhat_NW30)


mean_rmse_yhat_NW5
mean_rmse_yhat_NW10
mean_rmse_yhat_NW30
boxplot(rmse_yhat_NW5, rmse_yhat_NW10, rmse_yhat_NW30)		


plot(x,y[,1])	
	lines(x,yhat_NW5[,1],col = "red", lwd=3, lty=1)
	lines(x,yhat_NW10[,1],col = "blue", lwd=3, lty=2)
	lines(x,yhat_NW30[,1],col = "brown", lwd=3, lty=3)
legend("bottomright", c(expression(h==5), expression(h==10), expression(h==30)),
	ncol=1, col=c("red","blue","brown"), lwd=c(2,2,2), lty=c(1,2,3))
	
	
	
par(mfrow=c(1,2))
plot(x,y[,100])	
	lines(x,yhat_NW10[,100],col = "red", lwd=3, lty=1)
	
plot(x,y[,1000])
	lines(x,yhat_NW10[,1000],col = "blue", lwd=3, lty=1)

	

