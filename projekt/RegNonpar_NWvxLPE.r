rm(list=ls())

n=100
x = seq(1,n)
sim = 1
	set.seed(sim*10^6)
	e = rnorm(n,0,0.3)
	y = log(x)+e
	snr = var(log(x))/var(e)

library(KernSmooth)	

## bandwidth 5
plot(x,y)
	lines(ksmooth(x, y, kernel="normal", bandwidth=5, 
		range.x=range(x), x.points=x),col = "red", lwd=3, lty=1)
	lines(locpoly(x, y, degree=2, kernel="normal", 
		bandwidth=5, range.x=range(x)), col = "blue", lwd=3, lty=2)
	legend("bottom", c("NWE", "LPE"),ncol=1, col=c("red","blue"), 
		lwd=c(2,2), lty=c(1,2))

## bandwidth 10
plot(x,y)
	lines(ksmooth(x, y, kernel="normal", bandwidth=10, 
		range.x=range(x), x.points=x),col = "red", lwd=3, lty=1)
	lines(locpoly(x, y, degree=2, kernel="normal", 
		bandwidth=10, range.x=range(x)), col = "blue", lwd=3, lty=2)
	legend("bottom", c("NWE", "LPE"),ncol=1, col=c("red","blue"), 
		lwd=c(2,2), lty=c(1,2))
		
## bandwidth 30
library(KernSmooth)	
plot(x,y)
	lines(ksmooth(x, y, kernel="normal", bandwidth=30, 
		range.x=range(x), x.points=x),col = "red", lwd=3, lty=1)
	lines(locpoly(x, y, degree=2, kernel="normal", 
		bandwidth=30, range.x=range(x)), col = "blue", lwd=3, lty=2)
	legend("bottom", c("NWE", "LPE"),ncol=1, col=c("red","blue"), 
		lwd=c(2,2), lty=c(1,2))


	