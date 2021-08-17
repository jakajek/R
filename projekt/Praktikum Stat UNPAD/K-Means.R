#1 CLUSTERING#

library(ggplot2)
df <- data.frame(age = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 
                         39, 40, 41, 42, 44, 46, 47, 48, 49, 54),
                 spend = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 
                           44, 27, 29, 20, 28, 21, 30, 31, 23, 24)
)
ggplot(df, aes(x = age, y = spend)) +
  geom_point()

#2 IMPORT DATA#

library(dplyr)
PATH <-"https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/Computers.csv"
df <- read.csv(PATH) %>%
  select(-c(X, cd, multi, premium))
glimpse(df)

#From the summary statistics, you can see the data has large values.
#A good practice with k mean and distance calculation is to rescale the data so that the mean is equal to one and the standard deviation is equal to zero.

summary(df)

#The transformation reduces the impact of outliers and allows to compare a sole observation against the mean.
#If a standardized value (or z-score) is high, you can be confident that this observation is indeed above the mean (a large z-score implies that this point is far away from the mean in term of standard deviation.
#A z-score of two indicates the value is 2 standard deviations away from the mean.

rescale_df <- df %>%
  mutate(price_scal = scale(price),
         hd_scal = scale(hd),
         ram_scal = scale(ram),
         screen_scal = scale(screen),
         ads_scal = scale(ads),
         trend_scal = scale(trend)) %>%
  select(-c(price, speed, hd, ram, screen, ads, trend))

#3 TRAIN MODEL#

install.packages("animation")

set.seed(2345)
library(animation)
kmeans.ani(rescale_df[2:3], 3)
#Step 1: R randomly chooses three points
#Step 2: Compute the Euclidean distance and draw the clusters. You have one cluster in green at the bottom left, one large cluster colored in black at the right and a red one between them.
#Step 3: Compute the centroid, i.e. the mean of the clusters
#Repeat until no data changes cluster

pc_cluster <-kmeans(rescale_df, 5)
# pc_cluster$cluster: Indicates the cluster of each observation
#pc_cluster$centers: The cluster centres
#pc_cluster$totss: The total sum of squares
#pc_cluster$withinss: Within sum of square. The number of components return is equal to `k`
#pc_cluster$tot.withinss: Sum of withinss
#pc_clusterbetweenss: Total sum of square minus Within sum of square
#pc_cluster$size: Number of observation within each cluster 


#4 OPTIMAL K#

#Step 1: Construct a function to compute the total within clusters sum of squares
#Step 2: Run the algorithm times
#Step 3: Create a data frame with the results of the algorithm
#Step 4: Plot the results

#step1
kmean_withinss <- function(k) {
  cluster <- kmeans(rescale_df, k)
  return (cluster$tot.withinss)
}

kmean_withinss(2) #ex. 1

#step2
# Set maximum cluster 
max_k <-20 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

#step3
# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)

#step4
# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))
# K = 7
kls<-7

#Examine Cluster
pc_cluster_2 <-kmeans(rescale_df, kls)
pc_cluster_2$size
#The first cluster is composed of 608 observations, while the smallest cluster, number 4, has only 580 computers.
#It might be good to have homogeneity between clusters, if not, a thinner data preparation might be required. 

#cluster 2 has the highest price average among all the clusters.
center <-pc_cluster_2$centers
center

#step1-1 buat data frame
library(tidyr)
# create dataset with the cluster number
cluster <- c(1: 7)
center_df <- data.frame(cluster, center)
# Reshape the data
center_reshape <- gather(center_df, features, values, price_scal: trend_scal)
head(center_reshape)

#step2-1 reshape data
library(RColorBrewer)
# Create the palette
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

#step3-1 visualisasi
# Plot the heat map
ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic()
