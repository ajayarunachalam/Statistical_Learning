# genomic data
library(ISLR)
nci.labs = NCI60$labs
nci.data = NCI60$data

# Each cell line is labelled with a cancer type
dim(nci.data)

# examine cancer types for cell lines
nci.labs[1:4]
table(nci.labs)

# PCA
pr.out = prcomp(nci.data, scale = TRUE)

# plot the first few principal component score vectors
Cols = function(vec){
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1,2))
plot(pr.out$x[,1:2], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z2")
plot(pr.out$x[,c(1,3)], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z3")

summary(pr.out)

# plot the variance explained
plot(pr.out)

# plot the PVE and cumulative PVE of each PC
pve = 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve, type="o", ylab = "PVE", xlab = "Principal Component", col="blue")
plot(cumsum(pve), ylab = "PVE cumulative", xlab = "Principal component", col="brown3")


# clustering the observations in NCI60 data

#scale the data
sd.data = scale(nci.data)

# hierarchical clustering with different linkage and euclidean distance as dissimilarity measure

par(mfrow = c(1,3))
data.dist = dist(sd.data)
plot(hclust(data.dist, method = "complete"), labels = nci.labs,
     main = "Complete Linkage", xlab = "", ylab = "", sub = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs,
     main = "Average Linkage", xlab = "", ylab = "", sub = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs,
     main = "Single Linkage", xlab = "", ylab = "", sub = "")

# get clusters from complete linkage dendrogram
hc.out = hclust(data.dist)
hc.clusters = cutree(hc.out, 4)
table(hc.clusters, nci.labs)

# plot the dendrogram
par(mfrow = c(1,1))
plot(hc.out, labels = nci.labs)
abline(h=139, col="red")

hc.out

# K-means clustering with K=4
set.seed(2)
km.out = kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)

# hierarchical clustering on few principal components
hc.out = hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels = nci.labs, main = "HC on first 5 PC")
table(cutree(hc.out, 4), nci.labs)
