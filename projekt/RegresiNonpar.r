rm(list=ls())

library(MASS)
data(mcycle)

times = mcycle[,1]
accel = mcycle[,2]

yhat = ksmooth(times, accel, "normal", bandwidth = 2)

#yhat = locpoly(times, accel, degree=2, kernel="normal", bandwidth=5, 
#		range.x=range(times), binned=TRUE)

## Nadaraya-Watson
plot(times,accel)
    lines(ksmooth(times, accel, "normal", bandwidth = 2), col = "red", lwd=3, lty=1)
    lines(ksmooth(times, accel, "normal", bandwidth = 5), col = "blue", lwd=3, lty=2)
	
plot(times,accel)
    lines(ksmooth(times, accel, "box", bandwidth = 2), col = "red", lwd=3, lty=1)
    lines(ksmooth(times, accel, "box", bandwidth = 5), col = "blue", lwd=3, lty=2)

plot(times,accel)
    lines(ksmooth(times, accel, "normal", bandwidth = 5), col = "red", lwd=3, lty=1)
    lines(ksmooth(times, accel, "box", bandwidth = 5), col = "blue", lwd=3, lty=2)
    
 
### LOCAL POLYNOMIAL

library("KernSmooth")
plot(times,accel)
	lines(locpoly(times, accel, degree=2, kernel="normal", bandwidth=2), col="red", lwd=3, lty=1)
	lines(locpoly(times, accel, degree=2, kernel="normal", bandwidth=5), col="blue", lwd=3, lty=2)
	
plot(times,accel)
	lines(locpoly(times, accel, degree=2, kernel="evanechnikov", bandwidth=2), col="red", lwd=3, lty=1)
	lines(locpoly(times, accel, degree=2, kernel="evanechnikov", bandwidth=5), col="blue", lwd=3, lty=2)
	
	
###NWE vs LPE

plot(times,accel)
	lines(ksmooth(times, accel, kernel="normal", bandwidth=2), col="red", lwd=3, lty=1)
	lines(locpoly(times, accel, degree=1, kernel="normal", bandwidth=2), col="blue", lwd=3, lty=2)	
	

plot(times,accel)
	lines(ksmooth(times, accel, kernel="normal", bandwidth=5), col="red", lwd=3, lty=1)
	lines(locpoly(times, accel, degree=1,  kernel="normal", bandwidth=5, range.x=range(times), binned=FALSE), col="blue", lwd=3, lty=2)	
	
	
	