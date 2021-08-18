rm(list=lines(smooth.spline(x, y))ls())

n=100
x = seq(1,n)
sim = 1
	set.seed(sim*10^6)
	e = rnorm(n,0,0.3)
	y = log(x)+e
	snr = var(log(x))/var(e)
	
## Smoothing spline

plot(x,y)
lines(smooth.spline(x, y))
lines(smooth.spline(x, y), col="red", lwd=3, lty=1)





## B-SPLINES
library(splines)
#bsx = bs(x, degree=3)

BS_reg <- lm(y ~ bs(x, degree=3))

plot(x,y)
lines(x, predict(BS_reg), col="blue", lwd=3, lty=1)
lines(smooth.spline(x, y), col="red", lwd=3, lty=2)
legend("bottom", c("B-splines", "Smoothing spline"),ncol=1, 
	col=c("blue","red"), 
		lwd=c(2,2), lty=c(1,2))

