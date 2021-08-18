rm(list=ls())

library(splines)
n=100
x = seq(1,n)

nsim=1000
y = matrix(0, n,nsim)
e = matrix(0, n,nsim)
yhat_smooth = matrix(0, n,nsim)
mse_yhat_smooth = NULL

yhat_bsplines = matrix(0, n,nsim)
mse_yhat_bsplines = NULL

for(sim in 1:nsim){
	set.seed(sim*10^6)
	e[,sim] = rnorm(n,0,0.3)
	y[,sim] = log(x)+e[,sim]
	
		
	yhat_smooth[,sim] = smooth.spline(x, y[,sim])$y
	yhat_bsplines[,sim] = predict(lm(y[,sim] ~ bs(x, degree=3)))
		
	mse_yhat_smooth[sim] = sqrt(sum((y[,sim]-yhat_smooth[,sim])^2)/n)
	mse_yhat_bsplines[sim] = sqrt(sum((y[,sim]-yhat_bsplines[,sim])^2)/n)
	

}

rmse_yhat_smooth = mean(mse_yhat_smooth)
rmse_yhat_bsplines = mean(mse_yhat_bsplines)

rmse_yhat_smooth
rmse_yhat_bsplines

### plot


plot(x,y[,50])	
	lines(x,yhat_smooth[,50],col = "red", lwd=3, lty=1)
	lines(x,yhat_bsplines[,50],col = "blue", lwd=3, lty=2)
	legend("bottom", c("Smoothing spline", "bsplines"),
	ncol=1, col=c("red","blue"), lwd=c(3,3), lty=c(1,2))
	


plot(x,y[,100])	
	lines(x,yhat_smooth[,100],col = "red", lwd=3, lty=1)
	lines(x,yhat_bsplines[,100],col = "blue", lwd=3, lty=2)
	legend("bottom", c("Smoothing spline", "bsplines"),
	ncol=1, col=c("red","blue"), lwd=c(3,3), lty=c(1,2))

	
