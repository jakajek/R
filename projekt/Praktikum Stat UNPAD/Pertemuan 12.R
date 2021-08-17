############
##KORELASI##
############
#1
#INPUT DATA
x<-c(8,8,12,12,16,16,20,20,24,24)
y<-c(8,6,6,10,8,14,14,12,16,12)
#PLOT DATA
plot(x,y)
abline(lm(y~x), col="red")
#KORELASI DEFAULT R
cor(x,y, method="pearson")
#KORELASI PACKAGE HMISC
install.packages("Hmisc")
install.packages("stringi")
library(stringi)
library(Hmisc)
rcorr(x,y, type="pearson")

#2
xi<-c(106,86,100,101,99,103,97,113,112,110)
yi<-c(7,0,27,50,28,29,20,12,6,17)
cor(xi,yi, method="spearman")
rcorr(xi,yi, type="spearman")

#3
p<-c(2,5,1,7,9,10,3,8)
j<-c(5,7,8,1,3,6,2,4)
p=ordered(p)
j=ordered(j)
install.packages("pspearman")
library(pspearman)
spearman.test(p,j)
rcorr(p,j, type="spearman")

###########
##REGRESI##
###########
#1
data1<-read.csv("D:/JAKA/BAHAN AJAR/KOMPSTAT/CONTOH 1 - TOKO - SUDJANA.csv")
attach(data1)
plot(X,Y, pch=20)
abline(lm(Y~X), col="red")
#2
sum(X);sum(Y)
sum(X^2);sum(Y^2)
sum(X*Y)
mean(X);mean(Y)
b1=(sum(X*Y)-(sum(X)*sum(Y)/length(X)))/(sum(X^2)-(sum(X))^2/length(X));b1
b0=mean(Y)-b1*mean(X);b0
#3
fit<-lm(Y~X, data1);fit
summary(fit)
anova(fit)