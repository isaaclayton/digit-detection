load(".RData")
if(!require(flexclust)) install.packages('flexclust', repos = "http://cran.us.r-project.org")
library(flexclust)

knnclass = function(x, trainx, trainy, k) {
  y_set = levels(as.factor(trainy))
  dists = apply(dist2(x,trainx), 1, order)
  #new = apply(dist2(x,trainx), 1, function(g) 
  #y_set[which.max(tabulate(as.factor(trainy[match(sort(g, partial=1:k)[1:k],g)])))])
  #new = apply(dists, 1, function(g) 
  # y_set[which.max(table(trainy[order(g)[1:k]]))])
  predictions = list()
  for (i in k) {
    predictions[[i]] = apply(dists, 1, function(g)
      y_set[which.max(tabulate(trainy[dists[1:i]]+1))])
  }
  return(predictions)
}

knntests = knnclass(validation[,2:226],training[,2:226], training[,1], k=1:15)
print("Done")
save.image("knn.RData")