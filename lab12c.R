library(dendextend)

##some edits to the data set

UA.C <- USArrests
UA.C["Maryland", "UrbanPop"] <- 76.6

## also +/- 0.5 to restore the original  <n>.5  percentages
s5u <- c("Colorado", "Florida", "Mississippi", "Wyoming")
s5d <- c("Nebraska", "Pennsylvania")
UA.C[s5u, "UrbanPop"] <- UA.C[s5u, "UrbanPop"] + 0.5
UA.C[s5d, "UrbanPop"] <- UA.C[s5d, "UrbanPop"] - 0.5

##standardize features 
UA.S<-scale(UA.C)
head(UA.S)

############################
##HC with complete linkage##
############################

hc.complete<-hclust(dist(UA.S), method="complete")

##two styles of dendrograms
plot(hc.complete, main="HC with Complete Linkage")
plot(as.dendrogram(hc.complete), main="HC with Complete Linkage")

##############################
##diff colors for 4 clusters##
##############################

dend.complete.col4<-dendextend::color_labels(hc.complete, k=4)
plot(dend.complete.col4, main="HC with Complete Linkage, 4 Clusters")

######################
##Exploring Clusters##
######################

##see which cluster each state belongs to
cutree(hc.complete,4)

##find feature means of each cluster
grps.com<-cutree(hc.complete,4)

##create data frame to add clusters from HC in a column
x<-data.frame(UA.S,grps.com)

##centroid of each cluster
aggregate(x[,1:4],by=list(x$grps), mean)

##extract states in cluster 1
rownames(subset(x, grps.com==1))






