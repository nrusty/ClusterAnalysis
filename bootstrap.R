summary(protein)
vars.to.use <- colnames(protein)[-1]  
pmatrix <- scale(protein[,vars.to.use]) 
pcenter <- attr(pmatrix, "scaled:center")  
pscale <- attr(pmatrix, "scaled:scale")
#   Create the distance matrix.
d <- dist(pmatrix, method="euclidean") 
#   Do the clustering. 
pfit <- hclust(d, method="ward.D")   

#   Plot the dendrogram.
plot(pfit, labels=protein$X.U.FEFF.country) 
rect.hclust(pfit, k=5)
print_clusters <- function(labels, k) {             
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein[labels==i,c("X.U.FEFF.country","RedMeat","Fish","Fr.Veg")])
  }
}

# get the cluster labels
groups <- cutree(pfit, k=5)
print_clusters(groups, 5)
library(fpc)
kbest.p<-5  
cboot.hclust <- clusterboot(pmatrix,clustermethod=hclustCBI,
                            method="ward.D", k=kbest.p)
groups<-cboot.hclust$result$partition
print_clusters(groups, kbest.p)
cboot.hclust$bootmean 
cboot.hclust$bootbrd 
