### Inferensial dalam model regresi multipel
## Contoh soal 2
n=7
p=3
# Membuat matrix Y
y <- as.matrix(c(40, 50, 50, 70, 65, 65, 80))
y

# Membuat matrix X
x1 <- as.matrix(c(100, 200, 300, 400, 500, 600, 700))
x2 <- as.matrix(c(10,20,10,30,20,20,30))
X <- cbind(c(1,1,1,1,1,1,1), x1, x2)
X

# Menaksir parameter
teta.topi <- solve(t(X)%*%X)%*%(t(X)%*%y)
teta.topi
B0 <- teta.topi[1,1]
B1 <- teta.topi[2,1]
B2 <- teta.topi[3,1]


# Membuat matrix kovarians
ss <- (t(y-X%*%teta.topi)%*%(y-X%*%teta.topi))/(n-p)
ss
ss.scalar <- drop(ss)
var.beta <- solve(t(X)%*%X)*ss.scalar
var.beta
var.B0 <- var.beta[1,1]
var.B1 <- var.beta[2,2]
var.B2 <- var.beta[3,3]

# CI 95% untuk beta 1
te <- qt(0.975, (n-p))
te
ci.B1 <- c(B1-te*sqrt(var.B1), B1+te*sqrt(var.B1))
ci.B1

# Uji H0: B1=0
B1.nol=0
T.B1 <- (B1-B1.nol)/sqrt(var.B1)
T.B1

# Uji H0: B2=0
B2.nol=0
T.B2 <- (B2-B2.nol)/sqrt(var.B2)
T.B2

# Korelasi B1 dan B2
cov.B1B2 <- var.beta[3,2]
cor.B1B2 <- cov.B1B2/(sqrt(var.B1)*sqrt(var.B2))
cor.B1B2

# Uji H0: B1=B2
C <- as.matrix(c(0,1,-1))
L <- t(C)%*%teta.topi
L
lambda=0
T <- (L-lambda)/sqrt((t(C)%*%solve(t(X)%*%X)%*%C)*ss.scalar)
T