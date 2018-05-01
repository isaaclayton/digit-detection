load(".RData")
if(!require(flexclust)) install.packages('flexclust', repos = "http://cran.us.r-project.org")
library(flexclust)

numFolds = 10
folds = kFolds(training, k=numFolds)
knnclass = function(x, trainx, trainy, k) {
  y = as.factor(trainy)
  y_set = levels(y)
  dists = apply(dist2(x,trainx), 1, order)
  predictions = list()
  for (i in k) {
    predictions[[i]] = apply(dists, 2, function(g)
      y_set[which.max(tabulate(match(y[g[1:i]], y_set)))])
  }
  return(predictions)
}
knntests = list()
for (i in 1:numFolds) {
  knntests[[i]] = knnclass(training[folds==i,2:226],training[folds!=i,2:226], training[folds!=i,1], k=1:25)
  print("Done")
}
save.image("knn.RData")