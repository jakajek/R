###############################
## import data wais;senility ##
###############################
wais <- read.table("C:/Users/jakajek/Desktop/wais.txt", header = T)

#-----------------------------------------------------------------#

ex.2.3.logregr<-function(y=wais$senility, x=wais$wais, prop.sd=c(0.2,0.2), iter=2500, beta0=c(0,0)){
  
  precision<-700
  Iterations<-iter
  mu.beta<-c(0,0); s.beta<-c(100,100) 
  prop.s<-prop.sd
  beta <- matrix(nrow=Iterations, ncol=2) 
  acc.prob <- 0 
  current.beta<-beta0
  for (t in 1:Iterations){  
    prop.beta<- rnorm( 2, current.beta, prop.s )   
    
    ## Hitung Linear Predictors ##
    cur.eta<-current.beta[1]+current.beta[2]*x 
    prop.eta<-prop.beta[1]+prop.beta[2]*x 
    
    ##################################################################
    ## Jika Linear Predictor lebih besar dibanding Precission, maka ##
    ## buatlah agar sama dengan Precision untuk mencegah p = 1      ##
    ##################################################################
    cur.eta[cur.eta>precision] <-precision
    prop.eta[prop.eta>precision]<-precision
    
    loga <-( sum(  y*prop.eta- log( 1 + exp(prop.eta) ) )
             -sum(  y*cur.eta - log( 1 + exp(cur.eta ) ) ) 
             +sum(dnorm(prop.beta,    mu.beta, s.beta, log=TRUE)) 
             -sum(dnorm(current.beta, mu.beta, s.beta, log=TRUE)) )
    
    
    u<-runif(1)  
    u<-log(u)  
    if( u < loga ) { 
      current.beta<-prop.beta	  
      acc.prob <- acc.prob+1 
    }  
    beta[t,]<-current.beta  
  }
  print(acc.prob/Iterations)
  return(beta)
} 

############################################################
## Selanjutnya kita akan coba generate MCMC nya.          ##
## Banyaknya iterasi ditentukan sebanyak 55.000 kali.     ##
## sesuai dengan contoh 2.3 bahwa iterasi sebanyak 55.000 ##
## dapat memastikan konvergensi dengan mengenyampingkan   ##
## inisial 8000 kali iterasi sebagai Burnin Period        ##
############################################################
beta1<-ex.2.3.logregr(iter=55000, prop.sd=c(0.2,0.2))

  

