##1##

A<-c(3.1,2.7,3.3,2.6,3)
B<-c(3.2,2.5,3,2.2,3.5)

Ftest=function(x1,x2,alpha)
{
 n1=length(x1)
 n2=length(x2)
 df1=n1-1
 df2=n2-1
 Fhit=var(x1)/var(x2)
 FU=qf(1-alpha,df1,df2)
 FL=1/FU
 hasil=c(Fhit,FL,FU)
names(hasil)=c("Fhit","FL","FU")
 return(hasil)
}

#1.1
t.test(A,B,alternative="two.sided",conf.level=.95)

##2##
s1<-c(9.98,9.88,9.84,9.99,9.94,9.84,9.86,10.12,9.9,9.91)
s2<-c(9.88,9.86,9.75,9.8,9.87,9.84,9.87,9.86,9.83,9.86)

t.test(s1,s2,paired=T,alternative="two.sided",conf.level=.95)

##3##
p1<-c(1,1,1,0,0,0,1,1,1)
p2<-c(1,1,1,1,0,0,0,1,1)
mytable<-table(p1,p2)
prop.test(mytable, alternative="two.sided")


