#PERTEMUAN 6#

#Grafik 1
#1
cars <- c(1, 3, 6, 4, 9)
plot(cars)
#2
plot(cars, type="o", col="blue")
title(main="Autos", col.main="red", font.main=4)
#3
trucks <- c(2, 5, 4, 5, 12)
plot(cars, type="o", col="blue", ylim=c(0,12))
lines(trucks, type="o", pch=22, lty=2, col="red")
title(main="Autos", col.main="red", font.main=4)

#Grafik 2
##Histogram
x1<-c(1,2,3,4,4,6,7,10,9) #data
hist(x1) #plot histogram
#probabilitas
hist(x1,prob=T)
#densitas
lines(density(x1),col="red")
#tambah rug plot
rug(x1,col="blue")

##Boxplot
boxplot(x1)
title(main="Contoh Boxplot x1")
boxplot(x1,main="Contoh Boxplot Horizontal x1",horizontal=T)

#Boxplot >1
x1=c(1,2,3,4,4,6,7,10,9)
x2=c(2,2,2,3,4,2,3,4,1)
x3=c(2,3,7,4,8,6,9,11,2)
#1
boxplot(x1,x2,x3)
title(main="three boxplots")
#atau
bp3<-data.frame(x1,x2,x3)
boxplot(bp3)
title(main="three boxplots")

#Barplot
barplot(x1)
#1
barplot(x1,col=rainbow(20))
#2
#barplot(x1,col=rainbow(20), horizontal=T)

#Barplot Data Grup
A=c(11.7,18.1,26.9,41,66)
B=c(8.7,11.7,20.3,30.9,54.3)
C=c(15.4,24.3,37,54.6,71.1)
D=c(8.4,13.6,19.3,35.1,50)
mydata=data.frame(A,B,C,D)
rownames(mydata)=c("50-54","55-59","60-64","65-69","70-74")
mydata2=as.matrix(mydata)
#1
barplot(mydata2)
#2
barplot(mydata2,col=rainbow(5),legend=T)
#3
#barplot(mydata,col=rainbow(5),legend=T,beside=T)

#Scatter Plot
x1=c(1,2,3,4,4,6,7,10,9)
plot(x1)
#1
plot(x1,type="l")
#2
par(mfrow=c(3,2))
plot(x1,type="p",main="type p:
points")
plot(x1,type="l",main="type l:
lines")
plot(x1,type="b",main="type b:
both")
plot(x1,type="o",main="type o:
overplot")
plot(x1,type="h",main="type h:
histogram")
plot(x1,type="s",main="type s:
steps")

#Plotting Symbols
x<-rep(1:5,times=5) #vektor panjang=25
y<-rep(1:5,each=5) #vektor panjang=25
##xpd bertugas untuk mengontrol clipping grafik
par(xpd=T)
##pch adalah vektor 1:25, cex untuk ukuran symbol, bg (background colour)
plot(x,y,type="p",pch=1:25,cex=3,axes=F,ann=F,bty="n",bg="red")
title(main="Data symbols 1:25")
text(x,y,labels=1:25,pos=1,offset=1)

#1
x<-rep(1:5,times=5)
y<-rep(1:5,each=5)
plot(x,y,type="p",pch=3,
lwd=x,cex=y,xlim=c(0,5),
ylim=c(0,5),bty="n", ann=F)
title(main="R plottingsymbols: size and width",
xlab="Width (lwd)",
ylab="Size (cex)")

#2
function ()
{x<-rep(1:5,times=5)
y<-rep(1:5,each=5)
plot(x,y,type="p",pch=3,lwd=x,
cex=y,col=1:25,xlim=c(0,5),
ylim=c(0,5),bty="n", ann=F)
title(main="R plotting symbols: size and
width",xlab="Width (lwd)",
ylab="Size (cex)")
}

#2-1
x<-rep(1:5,times=5)
y<-rep(1:5,each=5)
plot(x,y,type="p",pch=3,lwd=x,cex=y,
col=1:25,
xlim=c(0,5),ylim=c(0,5)
,bty="n", ann=F)
title(main="R plotting symbols: size and
width",
xlab="Width (lwd)",ylab="Size (cex)")

#3
x<-rep(1:5,times=5)
y<-rep(1:5,each=5)
plot(x,y,type="p",pch=3,lwd=x,cex=y,
col=rainbow(25),
xlim=c(0,5),ylim=c(0,5),bty="n", ann=F)
title(main="R plottingsymbols: size and width",
xlab="Width (lwd)",ylab="Size (cex)")

#4
x<-rep(1:5,times=5)
y<-rep(1:5,each=5)
plot(x,y,type="p",pch=3,lwd=x,cex=y,
col=heat.colors(25),
xlim=c(0,5),ylim=c(0,5),bty="n", ann=F)
title(main="R plottingsymbols: size and width",
xlab="Width (lwd)",ylab="Size (cex)")

#5
x<-rep(1:5,times=5)
y<-rep(1:5,each=5)
plot(x,y,type="p",pch=3,lwd=x,cex=y,
col=grey(1:25),
xlim=c(0,5),ylim=c(0,5),bty="n", ann=F)
title(main="R plotting symbols: size and width",
xlab="Width (lwd)",ylab="Size (cex)")


#Plot 2 Variabel
tb=c(160, 165, 175, 180, 185)
bb=c(55,60,70,75,80) 
plot(x=bb,y=tb)
#atau
plot(bb~tb)

#pch
plot(bb~tb, pch=16)
#lines
plot(bb~tb, type="l")
#dot
plot(bb~tb, type="l",lty="dotted")


#Type par(lty)
x<-1:10
plot(x,y=rep(1,10),ylim=c(1,6.5),
type="l",lty=1,axes=F,ann=F)
title(main="Line Types")
for(i in 2:6)
{lines(x,y=rep(i,10),lty=i)
}
axis(2,at=1:6,tick=F,las=1)
linenames<-paste("Type",1:6,":",c("solid",
"dashed","dotted","dotdash","longdash",
"twodash"))
text(2,(1:6)+0.25,labels=linenames)
box()


#Menambahkan garis pada plot
tb=c(160, 165, 175, 180, 185)
bb=c(55,60,70,75,80)
plot(bb,tb)
reg<-lm(tb~bb)
abline(reg,
col="red",lty=2)
#1
plot(bb,tb)
reg<-lm(tb~bb)
abline(reg, col="red",lty=2)
abline(v=60,col="red")


#Legend pada Plot
y1=c(2, 5, 4, 5, 12)
y2=c(2,3,4,5,4)
plot(y1, type="o", col="blue",
ylim=c(0,12),xlab="tahun",ylab="jumlah")
lines(y2, type="o", pch=22, lty=2, col="red")
title(main="Contoh aja", col.main="red", font.main=4)
legend(1, 6, c("A","B"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2)