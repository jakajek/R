##Dist Peluang

x<-rnorm(100) #100 simulasi berdistribusi normal
head(x,3);tail(x,3);length(x)
w<-rexp(1000, rate=.1) #1000 simulasi dari exp(teta=10)
head(w,3);tail(w,3);length(w)
dbinom(3, size=10, prob=.25) #p(x=3) untuk X~Bin(n=10, p=.25)
pbinom(3, size=10, prob=.25) #p(x<_3) diatas distribusi
pnorm(12, mean=10, sd=2) #p(x<_12) untuk X~N(mu=10, sigma=2)
qnorm(.75, mean=10, sd=2) #kuartil 3 dari N(mu=10, sigma=2)
qchisq(.10, df=8) #percentile 10 dari chisquare(8)
qt(.95, df=20) #percentile 95 dari t(20) 

#contoh1-binom
dbinom(4, size=12, prob=.2)
x<-c(1,2,3,4)
dbinom(x,12,.2)
pbinom(4,12,.2)
#plot
x<-c(0:10)
y<-dbinom(x,12,.2)
plot(x,y,type="h",lwd=30,col="grey")

#contoh2-poisson
dpois(16,lambda=12)
ppois(16,lambda=12)
ppois(16,lambda=12,lower=FALSE) #lower.tail=FALSE (lebih presisi); lower.tail=TRUE (kembali ke 1)
#grafik contoh2
x<-0:16 
pdf<-dpois(x,lambda=12)
plot(x, pdf, type="h", lwd=3, col="blue", 
  main="contoh grafik poisson")
abline(h=0, col="green2")
curve(dnorm(x, 12, sqrt(12)), lwd=2, col="red", add=T)

#contoh3-uniform
dunif(18,min=5,max=23)*(18-2)

#contoh4-integrasi montecarlo
integrand <- function(x) {x^2}
integrate(integrand, lower = 0, upper =1 )
#contoh integral definite
2*mean(exp(runif(1000000, min=0, max=2)^3))

#contoh5-normal
pnorm(84, mean=72, sd=15.2, lower.tail=F)
#plot
curve(dnorm,from=-3, to=3)
curve(pnorm(x,10,2),from=4,to=16)
curve(pnorm(x,72,15.2),from=84,to=100)
#plot-2
population_mean<-72
population_sd<-15.2
sd_to_fill<-1
lower_bound <- population_mean - population_sd * sd_to_fill
upper_bound <- population_mean + population_sd * sd_to_fill
 #1
x <- seq(-4, 4, length = 1000) * population_sd + population_mean
#2 
y <- dnorm(x, population_mean, population_sd)
#4
plot(x, y, type="n", xlab = "nilai", ylab = "", main = "nilai ujian komstat", axes = FALSE)
#5
lines(x, y)
#6
bounds_filter <- x >= lower_bound & x <= upper_bound
x_within_bounds <- x[bounds_filter]
y_within_bounds <- y[bounds_filter]
#7
x_polygon <- c(lower_bound, x_within_bounds, upper_bound)
y_polygon <- c(0, y_within_bounds, 0)
 
polygon(x_polygon, y_polygon, col = "red")
#8
probability_within_bounds <- pnorm(upper_bound, population_mean, population_sd) - pnorm(lower_bound, population_mean, population_sd)
#9
text <- paste("p(", lower_bound, "< nilai <", upper_bound, ") =", signif(probability_within_bounds, digits = 3))
#10
sd_axis_bounds = 5
axis_bounds <- seq(-sd_axis_bounds * population_sd + population_mean, sd_axis_bounds * population_sd + population_mean, by = population_sd)
axis(side = 1, at = axis_bounds, pos = 0)
