getwd()
setwd("C:/Users/Sinan/Desktop/HW3")
data<-read.csv("EastWestAirlines.csv",header = TRUE)
library(class)
library(CRAN)
library(cluster)
library(purrr)
library(factoextra)
library(dplyr)
library(tidyr)
library(ggplot2)
#The goal is to try to identify clusters of passengers that have similar 
#charactersitics for the purpose of targeting different segments for different 
#types of mileage offers.

# a. Apply hierarchical clustering with Euclidean distance 
# and complete linkage. How many clusters appear to be appropriate?
fix(data)
data=data[,2:12]
data_scaled<-scale(data)

d <- dist(data_scaled, method = "euclidean")
hc1 <- hclust(d, method = "complete")

fviz_nbclust(data_scaled,FUN=hcut,method="silhouette",k.max = 20, nboot = 100)
plot(hc1, cex = 0.2, hang = -1)
rect.hclust(hc1,k=6)
sil=silhouette(clust,d)
windows()
fviz_silhouette(sil)

clust<-cutree(hc1, k =6)
unique(clust)

group= sapply(1:6, function(i, dat, clusters){ind = (clusters == i)
colMeans(dat[ind,])
}, data_scaled, clust)

group
# b. Compare the cluster centroids to characterize the different clusters and try to 
# give each cluster a label.
g24=cutree(hc1, k =5)
table(g24)
hc1
clust <- cutree(hc1, k = 15)
fviz_cluster(list(data = data, cluster = clust))


# c. To check the stability of the clusters, remove a random 
# 5% of the data (by taking a random sample of 95% of the records, namely 200 records),
# and repeat the analysis. Does the same picture emerge? Use 425 as the seed.
set.seed(425)
train=sample(1:3999,200)
random_data=data[train,]

scaled_rand<-scale(random_data)
rand_dist <- dist(scaled_rand, method = "euclidean")
hc2 <- hclust(rand_dist, method = "complete" )

fviz_nbclust(scaled_rand,FUN=hcut,method="silhouette",k.max = 20, nboot = 100)

plot(hc2, cex = 0.6, hang = -1)
rect.hclust(hc2,k=6)

# d. Use k-means algorithm with the number of clusters you
# found in part (a). Does the same picture emerge?



kmeans6<-kmeans(data_scaled,centers =6,nstart = 120 )
table(kmeans6$cluster)
table(clust)



# e. Which clusters would you target for offers, and 
# what type of offers would you target to customers in that cluster?
  






