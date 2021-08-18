## soal kafein
n=30
p=3
# Membuat matrix Y
y1 <- c(242, 245, 244, 248, 247, 248, 242, 244, 246, 242)
y2 <- c(248, 246, 245, 247, 248, 250, 247, 246, 243, 244)
y3 <- c(246, 248, 250, 252, 248, 250, 246, 248, 245, 250)
Y <- as.matrix(c(y1,y2,y3))
Y

# Membuat matrix X (complete model)
mu1 <- c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mu2 <- c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0)
mu3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)
X <- cbind(mu1,mu2,mu3)
X

# Menaksir parameter
teta.topi <- solve(t(X)%*%X)%*%(t(X)%*%Y)
teta.topi

# Membuat matrix X untuk H0: mu1=mu2=mu3 (restricted model)
X0 <- as.matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
X0

# Menaksir parameter RM
teta.nol <- solve(t(X0)%*%X0)%*%(t(X0)%*%Y)
teta.nol

# menghitung RSS
rss <- t(Y-X%*%teta.topi)%*%(Y-X%*%teta.topi)
rss

rss.nol <- t(Y-X0%*%teta.nol)%*%(Y-X0%*%teta.nol)
rss.nol

Fhit <- ((rss.nol-rss)/(p-1))/(rss/(n-p))
Fhit



