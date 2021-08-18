wais <- read.table("C:/Users/jakajek/Desktop/wais.txt", header = T)
y <- wais$senility
x <- wais$wais
iterations <- 2500
mu.beta <- c(0,0)
S.beta <- c(100,100)

prop.s <- c(0.1, 0.1)
beta <- matrix(nrow = iterations, ncol = 2)
acc.prob <- 0
current.beta <- c(0,0)
for (t in 1:iterations) {
  prop.beta <- rnorm(2, current.beta, prop.s)
  cur.eta <- prop.beta[1] + current.beta[2] * x
  prop.eta <- prop.beta[1] + prop.beta[2] * x
  loga <- (sum(y * prop.eta - log(1 +exp(prop.eta))) - sum((y * cur.eta - log(1 + exp(cur.eta))) + sum(dnorm(prop.beta, mu.beta, S.beta, log = T))-sum(dnorm(current.beta, mu.beta, S.beta, log = T))))
  u <- runif(1)
  u <- log(u)
  if (u < loga) {
    current.beta <- prop.beta
    acc.prob <- acc.prob + 1
  }
  beta[t,] <- current.beta
}
