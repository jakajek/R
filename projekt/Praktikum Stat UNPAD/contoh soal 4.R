### Membandingkan 2 garis regresi
## Jawaban No. 1
y <- as.matrix(c(13.3, 8.9, 15.1, 10.4, 11.5, 14.2, 15.4, 13.1, 13.8))
y

a1 <- c(1,1,1,1,0,0,0,0,0)
b1 <- c(28,20,32,22,0,0,0,0,0)
a2 <- c(0,0,0,0,1,1,1,1,1)
b2 <- c(0,0,0,0,21,27,29,23,25)
x1 <- cbind(a1,b1,a2,b2)
x1

teta1.topi <- solve(t(x1)%*%x1)%*%(t(x1)%*%y)
teta1.topi

## Jawaban No. 2
b0 <- c(28,20,32,22,21,27,29,23,25)
x2 <- cbind(a1,a2,b0)
x2

teta2.topi <- solve(t(x2)%*%x2)%*%(t(x2)%*%y)
teta2.topi

rss <- t(y-x1%*%teta1.topi)%*%(y-x1%*%teta1.topi)
rss

rss2 <- t(y-x2%*%teta2.topi)%*%(y-x2%*%teta2.topi)
rss2

Fhit2 <- ((rss2-rss)/1)/(rss/5)
Fhit2

## Jawaban No. 3
a0 <- c(1,1,1,1,1,1,1,1,1)
x3 <- cbind(a0,b0)
x3

teta3.topi <- solve(t(x3)%*%x3)%*%(t(x3)%*%y)
teta3.topi

rss3 <- t(y-x3%*%teta3.topi)%*%(y-x3%*%teta3.topi)
rss3

Fhit3 <- ((rss3-rss)/2)/(rss/5)
Fhit3

