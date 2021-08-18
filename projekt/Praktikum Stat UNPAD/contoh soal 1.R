### Inferensial dalam model regresi sederhana
## Contoh soal 1
n=7
p=2
# Membuat matrix Y
y <- as.matrix(c(40, 50, 50, 70, 65, 65, 80))
y

# Membuat vektor x dan matrix X
x <-c(100, 200, 300, 400, 500, 600, 700)
x
X <- cbind(c(1,1,1,1,1,1,1), c(100, 200, 300, 400, 500, 600, 700))
X

# Membuat scatter plot
plot(x,y, xlab="Kadar Pupuk (kg/ha)", ylab="Hasil Panen (100kg/ha)", main="Scatter Plot")
lm <- lm(y~x)
abline(lm, col="red")

# Menaksir parameter
teta.topi <- solve(t(X)%*%X)%*%(t(X)%*%y)
teta.topi
beta.topi <- teta.topi[2,1]
beta.topi
alfa.topi <- teta.topi[1,1]
alfa.topi

# CI 95%
te <- qt(0.975, (n-p))
te
ss <- (t(y-X%*%teta.topi)%*%(y-X%*%teta.topi))/(n-p)
ss
ci.beta <- c(beta.topi-te*sqrt(ss/sum((x-mean(x))^2)), beta.topi+te*sqrt(ss/sum((x-mean(x))^2)))
ci.beta
ci.alfa <- c(alfa.topi-te*sqrt(((1/n)+(mean(x)^2)/sum((x-mean(x))^2))*ss), alfa.topi+te*sqrt(((1/n)+(mean(x)^2)/sum((x-mean(x))^2))*ss))
ci.alfa

# Statistik uji
beta.nol=0
alfa.nol=0
Tbeta.stu <- (beta.topi-beta.nol)/sqrt(ss/sum((x-mean(x))^2))
Tbeta.stu
Tbeta.F <- (beta.topi-beta.nol)^2/(ss/sum((x-mean(x))^2))
Tbeta.F
Talfa <- (alfa.topi-alfa.nol)/(((1/n)+(mean(x)^2)/sum((x-mean(x))^2))*ss)
Talfa

## inferensial untuk Y jika x fixed
xf=550
# Statistik uji
Tfix <- ((alfa.topi+(beta.topi*xf))-(alfa.nol+(beta.nol*xf)))/(sqrt(((1/n)+((xf-mean(x))^2)/sum((x-mean(x))^2))*ss))
Tfix

# CI
ci.fix <- c((alfa.topi+(beta.topi*xf))-te*sqrt(((1/n)+((xf-mean(x))^2)/sum((x-mean(x))^2))*ss), (alfa.topi+(beta.topi*xf))+te*sqrt(((1/n)+((xf-mean(x))^2)/sum((x-mean(x))^2))*ss))
ci.fix

# CI prediksi
ci.pred <- c((alfa.topi+(beta.topi*xf))-te*sqrt((1+(1/n)+((xf-mean(x))^2)/sum((x-mean(x))^2))*ss), (alfa.topi+(beta.topi*xf))+te*sqrt((1+(1/n)+((xf-mean(x))^2)/sum((x-mean(x))^2))*ss))
ci.pred


