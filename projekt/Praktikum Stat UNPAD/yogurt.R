rm(list=ls(all=T))
setwd('D:\\R-nda\\Multivariat\\201412\\')

#library yang diperlukan: dummies, royston dan biotools
butuh <- c("dummies", "royston", "biotools", "car")
punya   <- butuh %in% rownames(installed.packages())
if(any(!punya)) install.packages(butuh[!has])

#panggil packages
require(dummies)
require(royston)
require(biotools)
require(car)

#panggil data
yogurt<-read.csv("yogurt.csv")

#a. pemeriksaan asumsi normalitas
y<-cbind(yogurt$Y1,yogurt$Y2)
x<-interaction(yogurt$X1,yogurt$X2)
royston.test(y)
yogurt$X1<-as.factor(yogurt$X1)
yogurt$X2<-as.factor(yogurt$X2)


#b. homogenitas matrix varian covarian
homogenitas <- boxM(y,x)
homogenitas

#c. tabel manova
yogurt.manova1 <- manova(cbind(Y1,Y2) ~ X1, data=yogurt)
summary(yogurt.manova1, intercept=T)
summary(yogurt.manova1, intercept=T, test="Pillai")
summary(yogurt.manova1, intercept=T, test="Wilks")
summary(yogurt.manova1, intercept=T, test="Hotelling-Lawley")
summary(yogurt.manova1, intercept=T, test="Roy")

yogurt.manova2 <- manova(cbind(Y1,Y2) ~ X2, data=yogurt)
summary(yogurt.manova2, intercept=T)
summary(yogurt.manova2, intercept=T, test="Pillai")
summary(yogurt.manova2, intercept=T, test="Wilks")
summary(yogurt.manova2, intercept=T, test="Hotelling-Lawley")
summary(yogurt.manova2, intercept=T, test="Roy")

yogurt.manova3 <- manova(cbind(Y1,Y2) ~ X1+X2, data=yogurt)
summary(yogurt.manova3, intercept=T)
summary(yogurt.manova3, intercept=T, test="Pillai")
summary(yogurt.manova3, intercept=T, test="Wilks")
summary(yogurt.manova3, intercept=T, test="Hotelling-Lawley")
summary(yogurt.manova3, intercept=T, test="Roy")




summary.aov(yogurt.manova1, intercept=T)
summary.aov(yogurt.manova2, intercept=T)
summary.aov(yogurt.manova3, intercept=T)

#tukey
a1 <- aov(Y1 ~ X1 + X2, data=yogurt)
(posthoc1 <- TukeyHSD(x=a1, conf.level=0.95, data=yogurt))

a2 <- aov(Y2 ~ X1 + X2, data=yogurt)
(posthoc2 <- TukeyHSD(x=a2, conf.level=0.95, data=yogurt))

