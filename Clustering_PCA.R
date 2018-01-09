states = row.names(USArrests)
states

names(USArrests)

# means
apply(USArrests, 2, mean)

# variances
apply(USArrests, 2, var)

# standardize and perform PCA
pr.out = prcomp(USArrests, scale = TRUE)

names(pr.out)

pr.out$center
pr.out$scale

# principal loading vector
pr.out$rotation

# principal component score vectors
dim(pr.out$x)

# plot the first two PC
biplot(pr.out, scale=0)

# Principal components are unique upto a sign change
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)

# std dev of each principal components
pr.out$sdev
pr.var = pr.out$sdev^2
pr.var

# proportion of variance explained by each PC
pve = pr.var/sum(pr.var)
pve

# plot the PVE and cumulative PVE
plot(pve, xlab = "Principal Components", ylab = "Proportion of Variance explained",
     ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = "Principal Components", ylab = "Cumulative PVE",
     ylim = c(0,1), type = "b")
