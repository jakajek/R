#data simulasi
set.seed(666)
calcrho<-function(rho,rho1,rho2) {
   rho*(1-rho1*rho2)/sqrt((1-rho1^2)*(1-rho2^2))
 }
 
 burn.in<-16
 n<-120 
 rho<-0.4
 rho1<-0.7
 rho2<-0.4
 q12<-calcrho(rho,rho1,rho2)
 eps<-mvrnorm(n+burn.in,mu=c(12.4,15.9),Sigma=cbind(c(1,0.9),c(0.9,1)))
 
 X11<-arima.sim(list(ar=rho1),n,innov=eps[burn.in+1:n,1],start.innov=eps[1:burn.in,1])
 X22<-arima.sim(list(ar=rho2),n,innov=eps[burn.in+1:n,2],start.innov=eps[1:burn.in,2])
 cor(X11,X22)
olah<-cbind(X11,X22)
###########
#plot data#
###########
par(mfrow=c(2,2))
plot.ts(X1, ylab="(%)", main="data1", col="blue")
abline(h=mean(X1),col="red")
plot.ts(X2, ylab="(%)", main="data2", col="blue")
abline(h=mean(X2),col="red")
###############
#data terpusat#
###############
data.trans<-olah
olah1<-data.trans[,1]-mean(data.trans[,1])
olah2<-data.trans[,2]-mean(data.trans[,2])
olah11<-matrix(olah1,ncol=1)
olah22<-matrix(olah2,ncol=1)
#in sample
X1<-olah11[1:108]			 #Bali
X2<-olah22[1:108]			 #batam

###########################
#mengecek stasioner varian#
###########################
b1 <- BoxCox.lambda(X1,lower=0)
nilai<-formatC(b1,digits=5,format="f")
nilai					#saat nilai dibawah 1 maka harus ditransformasi
b2 <- BoxCox.lambda(X2,lower=0)
nilai<-formatC(b2,digits=5,format="f")
nilai					#saat nilai dibawah 1 maka harus ditransformasi
###############
#uji stasioner#
###############
adf.test(X1)			#p-value>alpa berarti belum stasioner
adf.test(X2)			#p-value>alpa berarti belum stasioner
#########################
#pengecekan ACF dan PACF#
#########################
acf(X1,lag=100)			#data yang digunakan adalah data yg stasioner,klo X1 sudah stasioner pakai X1
acf(X2,lag=100)
#############
# MACF MPACF#
#############
data.ts <- cbind(X1,X2)
acf(data.ts)			# membuat plot acf data secara simultan
pacf(data.ts)			# membuat plot pacf data secara simultan
##############################################
#penentuan orde berdasarkan nilai SC terkecil#
##############################################
VARselect(data.ts, lag.max=10, type= "none") # Pilih dari nilai SC terkecil
#############
#uji Granger#
#############
library(lmtest)
grangertest(X1 ~ X2, order = 1) #p-value<alpa berarti ada hub kausalitas
grangertest(X2 ~ X1, order = 1) #p-value<alpa berarti ada hub kausalitas
	
####################
#estimasi parameter#
####################
var1<-VAR(data.ts, p=1, type="const")
summary(var1)
############################
#asumsi heteroskedastisitas#
############################
arch1 <- arch.test(var1,lags.multi = 10,multivariate.only = TRUE)
arch1						#p-value<residu mengandung efek ARCH					
###############
#MODEL ARCH(1)#
###############
residu<-resid(var1)
ARCH1<-garch(residu[,1],order=c(0,1))
summary(ARCH1)                     # Diagnostic tests
ARCH2<-garch(residu[,2],order=c(0,1))
summary(ARCH2) 
####################
#matriks pengolahan#
####################

###########
#matriks Y#
###########
n1=length(X1)
n2=length(X2)
nn=n1+n2
yy<-c()
yyy<-c()
for (i in 1:n1){
	yyy<-c(X1[i],X2[i])
	yy<-c(yy,yyy)
	}
y<-yy[3:nn]
Y<-matrix(y,ncol=1)
###########
#matriks X#
###########
n1=length(X1)
n2=length(X2)
nn=n1+n2
x<-c()
xx<-c()
xxx<-c()
xxxx<-c(0,0,0)
xxxxx<-c()
xxxxxx<-c()
for(i in 1:n1){
	xxx<-c(1,X1[i],X2[i])
	xxxxx<-c(xxx,xxxx)
	xxxxxx<-c(xxxx,xxx)
	xx<-c(xxxxx,xxxxxx)
	x<-c(x,xx)
	}
x1<-matrix(x,ncol=6,byrow=TRUE)
nn
X<-x1[1:990,]

	 
##################
#varian bersyarat#
##################
######
#Bali#
######
koef1<-ARCH1$coef
koef.ARCH1<-as.matrix(koef1,ncol=2)
V.bali2=koef.ARCH1[1]/(1-koef.ARCH1[2])
nnn=length(residu[,1])
s<-c()
for(i in 2:nnn){
	g=i-1
	sss=(residu[g,1])^2
	ss=koef.ARCH1[1]+(sss*koef.ARCH1[2])
	s=c(s,ss)
	}
V.bali<-c(V.bali2,s)
#######
#Batam#
#######
koef2<-ARCH2$coef
koef.ARCH2<-as.matrix(koef2,ncol=2)
V.batam2=koef.ARCH2[1]/(1-koef.ARCH2[2])
nnn=length(residu[,2])
s1<-c()
for(i in 2:nnn){
	g=i-1
	sss1=(residu[g,2])^2
	ss1=koef.ARCH2[1]+(sss1*koef.ARCH2[2])
	s1=c(s1,ss1)
	}
V.batam<-c(V.batam2,s1)
##########
#kovarian#
##########
gabung<-c(V.bali,V.batam)
kovarian<-diag(gabung)

#####################
#MODEL VAR(1)ARCH(1)#
#####################
inv.kov<-solve(kovarian)
b1<-t(X)%*%inv.kov
b2<-b1%*%X
b3<-solve(b2)
b4<-b1%*%Y
BET<-b3%*%b4
BETA<-matrix(BET,ncol=1)
########
#residu#
########
Bali.res<-c()
nX1<-length(X1)
for(i in 2:nX1){
	h=i-1
	Bali=BETA[1]+(BETA[2]*X1[h])+(BETA[3]*X2[h])
	Bali.re=X1[i]-Bali
	Bali.res=c(Bali.res,Bali.re)
	}
Batam.res<-c()
nX2<-length(X2)
for(i in 2:nX2){
	h=i-1
	Batam=BETA[4]+(BETA[5]*X1[h])+(BETA[6]*X2[h])
	Batam.re=X1[i]-Batam
	Batam.res=c(Batam.res,Batam.re)
	}
Bx<-matrix(Bali.res,ncol=1)
Bx1<-matrix(Batam.res,ncol=1)
residu<-cbind(Bx,Bx1)

################################
#Diagnostik MODEL VAR(1)ARCH(1)#
################################

################
#uji Normalitas#
################
library(royston)
library(MVN)
royston.test(residu)
##################
#Uji Autokorelasi#
##################
library(portes)
portest(residu[,1],lags=seq(1,10,1),test=c("BoxPierce"), MonteCarlo=FALSE)
portest(residu[,2],lags=seq(1,10,1),test=c("BoxPierce"), MonteCarlo=FALSE)
##########
#Uji ARCH#
##########
library(FinTS)
ArchTest (residu, lags=10, demean = FALSE)

###############
#Akurasi Model#
###############
######
#MAPE#
######

MAPE<-function(y,f){
	n=length(y)
	A=1
	B=0
	for(i in 1:n){	
		A=abs((y[i]-f[i])/y[i])
		B=B+A
		}
	MAPE=B/n*100
	print(MAPE)
	}
#out sample
X11<-olah11[497:508]			#Bali
X22<-olah22[497:508]		      #batam
N<-length(olah11)			 
nX11<-length(X11)
Bali.pre<-c()
Batam.pre<-c()
jj=N-nX11+1
for(i in jj:N){
	jjj=i-1
	Bali.p=BETA[1]+(BETA[2]*olah11[jjj])+(BETA[3]*olah22[jjj])
	Bali.pre=c(Bali.pre,Bali.p)
	Batam.p=BETA[4]+(BETA[5]*olah11[jjj])+(BETA[6]*olah22[jjj])
	Batam.pre=c(Batam.pre,Batam.p)
	}
#MAPE BALI#
y=X11
f=Bali.pre
MAPE(y,f)
#MAPE BATAM#
y=X22
f=Batam.pre
MAPE(y,f)

