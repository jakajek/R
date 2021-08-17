#Contoh Table
caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,
218,327,106,67),nrow=3,byrow=T);caff.marital

##Penamaan kolom dari sebuah tabel
colnames(caff.marital) <- c("0","1-150","151-300",">300")

##Penamaan baris dari sebuah tabel
rownames(caff.marital) <- c("Married","Prev.married","Single")
caff.marital


#Marginal table
margin.table(caff.marital,1)
margin.table(caff.marital,2)

#Membuat proporsi tabel
prop.table(caff.marital,1)
prop.table(caff.marital,2)

#Membuat grafik berdasarkan tabel
total.caff <- margin.table(caff.marital,2)
barplot(total.caff, col="white")
barplot(total.caff, col="red")
barplot(total.caff, col=heat.colors(4))
barplot(total.caff, col=rainbow(4))
barplot(caff.marital, col=rainbow(4))
barplot(caff.marital,col=rainbow(4),beside=T)
legend("topleft",c("Married","Prev.married","Single"),
cex=0.6, bty="n", fill=rainbow(3))

#Chi-Square Dadu
freq = c(22,21,22,27,22,36)
probs=rep(1/6,6)
chisq.test(freq,p=probs)

#Chi-Square Huruf
x = c(100,110,80,55,14)
probs = c(29, 21, 17, 17, 16)/100
chisq.test(x,p=probs)

#Chi-Square Test of Independence -kasus1-
yesbelt = c(12813,647,359,42)
nobelt = c(65963,4000,2642,303)
chisq.test(data.frame(yesbelt,nobelt))

#Contoh Kasus -2-
dietH <- matrix(c(4,30,40,2),nrow=2,byrow=T)
colnames(dietH) <- c("No","Yes")
rownames(dietH) <- c("No","Yes");dietH

dietL <- matrix(c(80,2,8,120),nrow=2,byrow=T)
colnames(dietL) <- c("No","Yes")
rownames(dietL) <- c("No","Yes");dietL

barplot(dietH,col=rainbow(4),beside=T)
legend("topleft",c("No","Yes"),cex=0.6,bty="n", fill=rainbow(3))
NoCoke = c(4,30)
YesCoke = c(40,2)
chisq.test(data.frame(NoCoke,YesCoke))

barplot(dietL,col=rainbow(5),beside=T)
legend("topleft",c("No","Yes"),cex=0.6,bty="n", fill=rainbow(4))
NoCoke = c(80,2)
YesCoke = c(8,120)
chisq.test(data.frame(NoCoke,YesCoke))

#Contoh Kasus -3-
x = c(50,41,85)
probs = c(4, 3, 9)/16
chisq.test(x,p=probs)

#Contoh KAsus -4-
set.seed(1971)
die.fair = sample(1:6,200,p=c(1,1,1,1,1,1)/6,replace=T)
die.bias = sample(1:6,100,p=c(.5,.5,1,1,1,2)/6,replace=T)
res.fair = table(die.fair);res.bias = table(die.bias)
rbind(res.fair,res.bias)
chisq.test(rbind(res.fair,res.bias))
