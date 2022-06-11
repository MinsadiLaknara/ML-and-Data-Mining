#Read the File
vehicles <- read.csv("D:/MY_STUFF/IIT/2nd year/2 sem/Machine Learning/cw/vehicles.csv")

#normalization 
v <- vehicles[1:18]
m <- apply(v,1,mean)
s <- apply(v,2,sd)
maxs <- apply(v, 2, max)    
mins <- apply(v, 2, min)
v <-scale(v, center = mins, scale = maxs - mins)

distance <- dist(v)
print(distance,digits = 3)
hc.c <- hclust(distance)
plot(hc.c)
plot(hc.c,hang = -1)

hc.a <- hclust(distance,method = "average")
plot(hc.a,hang = -1)
member.c <- cutree(hc.c,3)
member.a <-cutree(hc.a,3)
table(member.c,member.a)


aggregate(v, list(member.c),mean)
aggregate(vehicles[,-c(1,1)],list(member.c),mean)

library(cluster)
plot(silhouette(cutree(hc.c,3),distance))
#screen plot
wss <-(nrow(v)-1)*sum(apply(v,2,var))
for(i in 2:20) wss[i]<-sum(kmeans(v,centers=i)$withinss)
plot(1:20, wss,type = "b",xlab = "Number of Clusters",ylab = "within group ss")

#K-means clustering
kc <-kmeans(v,3)
plot(Rad.Ra~Comp,vehicles,col=kc$cluster)

