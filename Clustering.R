# K-Means Clustering

# on simulated data
set.seed(2)
x = matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4

# kmeans clustering
km.out = kmeans(x, 2, nstart = 20)

# clusters
km.out$cluster

# plot the data clusters
plot(x, col=(km.out$cluster + 1), main = "K-Means Clustering with K=2",
     xlab = "", ylab = "", pch = 20, cex = 2)

# k=3
set.seed(4)
km.out = kmeans(x, 3, nstart = 20)
km.out

plot(x, col = (km.out$cluster+1), main = "K-Means Clustering with K=3",
     xlab = "", ylab = "", pch = 20, cex = 2)

# different nstart - multiple initial cluster assignments
set.seed(3)
km.out = kmeans(x,3,nstart = 1)
km.out$tot.withinss
km.out = kmeans(x,3,nstart = 20)
km.out$tot.withinss


# Hierarchical Clustering
# complete linkage
hc.complete = hclust(dist(x), method = "complete")
# average linkage
hc.average = hclust(dist(x), method = "average")
# single linkage
hc.single = hclust(dist(x), method = "single")

# plot the dengrograms
par(mfrow = c(1,3))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = 0.9)

# cluster labels for ech observation
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

# scale the observations for performing hierarchical clustering
xsc = scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical clustering with Scaled Features")

# correlation based distance
x = matrix(rnorm(30*3), ncol=3)
dd = as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with correlation based distances",
     xlab = "", sub = "")
