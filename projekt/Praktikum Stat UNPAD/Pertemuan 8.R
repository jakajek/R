##1##
x=c(300,300,368,340,300,340,320,352,350)
#install.packages("TeachingDemos")
library(TeachingDemos)
z.test(x, mu = 300, sd = 60, conf.level = 0.9,"two.sided")
##1.1##
Ztest=function(x,m0,alpha,sigma)
{
n=length(x)
Zhit=sqrt(n)*(mean(x)-m0)/sigma
Ztabel=qnorm(1-alpha/2)
pvalue=2*(1-pnorm(Zhit))
hasil=c(Zhit,Ztabel,pvalue)
names(hasil)=c("Zhit","Ztabel","Pvalue")
return(hasil)
}
Ztest(x,m0=300,alpha=.95,sigma=60)

##2##
t.test(x, mu=300, alternative="two.sided", conf.level=0.95)
##2.1##
Ttest=function(x,m0,alpha)
{
n=length(x)
df=n-1
Thit=sqrt(n)*(mean(x)-m0)/sd(x)
Ttabel=qt(1-alpha/2,df)
pvalue=2*(1-pt(Thit,df))
hasil=c(Thit,Ttabel,pvalue)
names(hasil)=c("Thit","Ttabel","Pvalue")
return(hasil)
}
Ttest(x,m0=300,alpha=.95)

##3##
#prop.test(5,100,0.04,alternative="two.sided",conf.level=0.95)
##3.1##
Ptest=function(ps,p0,n,alpha)
{
Zhit=(ps-p0)/sqrt(p0*(1-ps)/n)
Ztabel=qnorm(1-alpha/2)
pvalue=2*(1-pnorm(Zhit))
hasil=c(Zhit,Ztabel,pvalue)
names(hasil)=c("Zhit","Ztabel","Pvalue")
return(hasil)
}
Ptest(ps=.05,p0=.04,n=500,alpha=0.05)


