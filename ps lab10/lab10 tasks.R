#libraries
library(dplyr)

#import computers data
computers = read.csv("computers(1).csv", header = TRUE, sep="," )
str(computers)
comp = computers %>% select(price,speed,hd,ram,screen,ads,trend)%>%
  mutate(price_scale = scale(price),speed_scale=scale(speed),
         hd_scale=scale(hd),ram_scale=scale(ram),
         screen_scale=scale(screen),ads_scale=scale(ads),trend_scale=scale(trend)
         )%>%select(-c(price,speed,hd,ram,screen,ads,trend))
str(comp)

#k-means clustering where k = 3
k3 = kmeans(comp,centers=3,nstart=25)
str(k3)
k3
#Within cluster sum of squares by cluster:
#[1] 9552.434 7599.473 9651.361
#(between_SS / total_SS =  38.8 %)

#optimal k
#function to compute total within-cluster sum of square
wcss=function(k){
  kmeans(comp,k,nstart=10)$tot.withinss
}
#compute and plot wss for k = 1 to k = 30
k.vhealues = 1:30
set.seed(100)
#apply wcss to all k values
wcss_k=sapply(k.values,wcss)
plot(k.values,wcss_k,type="b",pch=19,frame=FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#elbow point: k=7
#final clustering with k=7
set.seed(100)
k7=kmeans(comp,7,nstart=25)
k7
#Within cluster sum of squares by cluster:
#  [1] 2534.224 1392.690 2193.015 2819.608 2124.292 2773.744 2344.301
#(between_SS / total_SS =  63.1 %) greater this ratio, more distinct the clustering

#summarise at cluster level (add to original dataframe)
computers %>% select(price, speed, hd, ram, screen, ads, trend)%>%mutate(Cluster = k7$cluster) %>% 
  group_by(Cluster) %>% summarise_all("mean")

#hierachical clustering
#dissimilaity matrix
d = dist(comp,method="euclidean")
#hierarchical clustering using complete linkage
hc1=hclust(d,method="complete")
#plot obtained dendrogram
plot(hc1,cex=0.6,hang=-1)

#draw a border around the 7 clusters, option "border" specifies colours
rect.hclust(hc1,k=7,border=2:5)


