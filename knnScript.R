load(".RData")
if(!require(flexclust)) install.packages('flexclust', repos = "http://cran.us.r-project.org")
library(flexclust)

numFolds = 10
folds = kFolds(training, k=numFolds)
knntests = list()
for (i in 1:numFolds) {
  knntests[[i]] = knnclass(training[folds==i,2:226],training[folds!=i,2:226], training[folds!=i,1], k=1:25)
  print("Done")
}
save.image("knn.RData")