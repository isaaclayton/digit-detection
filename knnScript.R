load("Computer.RData")
library(flexclust)
knnclass = function(x, trainx, trainy, k) {
  new = apply(dist2(x,trainx), 1, function(g) 
    which.max(tabulate(trainy[match(sort(g, partial=1:k)[1:k, drop=FALSE],g), drop=FALSE]+1)))
  return(new)
}
knntests = list() 
nfolds = 10
digitsubset = kFolds(cbind(scalable_digs[,1], pr.out), nfolds)
for(i in 1:nfolds){
  knntests[[i]] = knnclass(pr.out$x[digitsubset==i,1:150],
                           pr.out$x[digitsubset!=i,1:150], scalable_digs[digitsubset!=i,1],
                           k=5)
  print("One more done!")
}
print("Done")
save.image("~/knn.RData")