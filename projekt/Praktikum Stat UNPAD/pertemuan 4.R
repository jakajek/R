x1<-seq(10)
x2<-seq(0,1,by=0.1)
x3<-rep(1,3)
x4<-c(rep(1,3),rep(2,2),rep(-1,4))
x5<-rep("Small",3)

x<-c(2,4)
y<-c(6,8)
m1<-cbind(x,y);m1

m2<-matrix(c(10,20,30,40),ncol=2)
m3<-matrix(c(1,3,2,5,-1,2,2,3,9),ncol=3,byrow=T)

t(m3) 	#transpose dari m3
m3[2,3] 	#element dari m3 pada baris 2, kolom 3
m3[2,] 	#baris 2
m3[,3] 	#kolom 3
m3[-1,] 	#submatrix dari m3 tanpa baris pertama
m3[,-1] 	#submatrix dari m3 tanpa kolom pertama
m3[-1,-1] 	#submatrix dari m3 tanpa kolom pertama dan baris pertama
2*m1 	#perkalian scalar
m1+m2 	#penambahan matrix addition
m1 %*% m2 	#perkalian component matrix
solve(m1) 	#inverse matrix
diag(3) 	#membangun k by k identity matrix
diag(c(2,3,3)) #diagonal matrices
eigen(m2) 	#mencari eigen

x<-c(4,2,6)
y<-c(1,0,-1)

no1<-c(v.x=length(x),v.y=length(y));no1
no2<-c(v.x=sum(x),v.y=sum(y));no2
no3<-c(v.x=sum(x^3),v.y=sum(y^2));no3
no4<-x+y;no4
no5<-x-y;no5
no6<-x*y;no6
no7<-c(v.x=(x+3), v.y=(y+2));no7
no8<-c(v.x=(x^4), v.y=(y^2));no8





mat.x<-matrix(c(3,2,-1,-1), ncol=2, byrow=T);mat.x
mat.y<-matrix(c(1,4,0,0,1,-1), ncol=3, byrow=T);mat.y

2*mat.x
mat.x*mat.x
mat.x%*%mat.x
mat.x%*%mat.y
t(mat.y)
solve(mat.x)
mat.x[1,]
mat.x[2,]
mat.x[,2]
mat.y[1,2]
mat.y[,2:3]


v <- c(2,1,2,3,1,2,3,4,1,5,5,3,2,3)
Modus <- function(x) {
	ux <- unique(x)
	tab <- tabulate(match(x, ux))
	ux[tab == max(tab)]
}
Modus(v)

