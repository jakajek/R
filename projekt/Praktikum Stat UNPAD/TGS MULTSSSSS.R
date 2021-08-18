## HIERARKI ##
# INPUT DATA #
data.multi <- read.table("C:/Users/jakajek/Desktop/uio.txt", header = T)
data.uio1 <- scale(data.multi)
klaster1 <- hclust(dist(uio1), method = "single")
klaster2 <- hclust(dist(uio1), method = "complete")
klaster3 <- hclust(dist(uio1), method = "average")
## YANG DIPAKAI ##
klaster4 <- hclust(dist(uio1), method = "ward.D")

#########################################
## MENENTUKAN k TERGANTUNG INTUISI ??? ##
#########################################
plot(klaster1, hang = -1)
c.cut1 <- cutree(klaster1, 2)
rect.hclust(klaster1, k = 2, border = "red")
plot(klaster2)
c.cut2 <- cutree(klaster2, 3)
rect.hclust(klaster2, k = 3, border = "blue")
plot(klaster3)
c.cut3 <- cutree(klaster3, 2)
rect.hclust(klaster3, k = 2, border = "magenta")
## YANG DIPAKAI ##
plot(klaster4)
c.cut4 <- cutree(klaster4, 3)
rect.hclust(klaster4, k = 3, border = "green")


library(pvclust)
fit1 <- pvclust(uio1, method.hclust = "single", method.dist = "euclidian")
plot(fit1)
pvrect(fit1)

fit2 <- pvclust(uio1, method.hclust = "complete", method.dist = "euclidian")
plot(fit2)
pvrect(fit2)

fit3 <- pvclust(uio1, method.hclust = "average", method.dist = "euclidian")
plot(fit3)
pvrect(fit3)

## YANG DIPAKAI ##
fit4 <- pvclust(uio1, method.hclust = "ward.D", method.dist = "euclidian")
plot(fit4)
pvrect(fit4)


## NON-HIERARCHICAL - K-MEANS ##

#Determine number of clusters#
## CARA 1 ##
wss <- (nrow(data.uio1)-1)*sum(apply(data.uio1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data.uio1, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## CARA 2 ##
library(apcluster)
d.apclus <- apcluster(negDistMat(r=2), data.uio1)
cat("affinity propogation optimal number of clusters:", length(d.apclus@clusters), "\n")
heatmap(d.apclus)
plot(d.apclus, data.uio1)

## CARA 3 ##
library(mclust)
d_clust <- Mclust(as.matrix(data.uio1), G=1:20)
m.best <- dim(d_clust$z)[2]
cat("model-based optimal number of clusters:", m.best, "\n")
plot(d_clust)

## CARA 4 (YANG DIPAKAI) ##
library(NbClust)
nb <- NbClust(data.uio1, diss= NULL, distance = "euclidean", min.nc=2, max.nc=15, method = "kmeans")
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))


## PROFILING ANALYSIS : CLASICAL MDS (metric) (YANG DIPAKAI)##
mds.dist <- dist(data.uio1)
mds.fit <- cmdscale(mds.dist, eig = T, k = 2)
mds.fit
#PLOT
mds.x <- mds.fit$points[,1]
mds.y <- mds.fit$points[,2]
plot(mds.x, mds.y, xlab = "COORDINATE NAME 1", ylab = "COORDINATE NAME 2", main = "CLASSICAL MDS")
text(mds.x, mds.y, labels = row.names(data.uio1), cex = .7)

## PROFILING ANALYSIS : CLASICAL MDS (non-metric) ##
mds2.dist <- dist(data.uio1)
mds2.fit <- isoMDS(mds2.dist, k = 2)
mds2.fit
#PLOT
mds2.x <- mds2.fit$points[,1]
mds2.y <- mds2.fit$points[,2]
plot(mds2.x, mds2.y, main = "NON-METTRIC MDS", type = "p", pch = 20)
text(mds2.x, mds2.y, labels = row.names(data.uio1), cex = .7)
