## soal bakteri (regresi exponensial)
# membuat matriks y dan x
y <- as.matrix(c(32,47,65,92,132,190,275))
y
x <- as.matrix(c(0,1,2,3,4,5,6))


# membuat plot
plot(x,y)

# menghitung beta.topi
beta.topi <- sum((x-mean(x))*(log(y)-mean(log(y)))/sum((x-mean(x))^2))
beta.topi

# menghitung ln alfa
ln.alfa <- mean(log(y))-beta.topi*mean(x)
ln.alfa

# menghitung alfa.topi
alfa.topi <- exp(ln.alfa)
alfa.topi

## cara 2
# membuat matrix transformasi
y.t rans <- log(y)
alfa <- as.matrix(c(1,1,1,1,1,1,1))
X <- cbind(alfa,x)
X

teta.topi <- solve(t(X)%*%X)%*%(t(X)%*%y.trans)
teta.topi

alfa.topi <- exp(teta.topi[1,1])
alfa.topi