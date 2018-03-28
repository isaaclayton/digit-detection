load(".RData")
if(!require(randomForest)) install.packages('randomForest', repos = "http://cran.us.r-project.org")
if(!require(doMC)) install.packages('doMC', repos = "http://cran.us.r-project.org")
library(randomForest)
library(doMC)
registerDoMC()

#This script will return the random forest implementation
#It's too time-consuming to run on my computer, so I'll run it on an EC2 instance and save the results as an .RData file
nfolds = 10
subsets = kFolds(df[,1], nfolds)
rf.digits = list()
rf.predicts = list()
df$V1 = as.factor(df$V1)
for(i in 1:nfolds) {
  print(paste("Starting Fold", i))
  rf.digits[[i]] = foreach(ntree=501, .combine=combine, .multicombine=TRUE, .packages='randomForest') %dopar% {
    randomForest(df[subsets!=i, 2:151],df[subsets!=i,1], mtry=20, importance=T, keep.forest = TRUE, ntree=ntree)
  }
  rf.predicts[[i]] = predict(rf.digits[[i]], df[subsets==i,2:151])
  print(paste("Done with Fold", i))
}
warns = warnings()
#hi isaac (:
print("Done")
save.image("Forest.RData")