load(".RData")
if(!require(randomForest)) install.packages('randomForest', repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages('data.table', repos = "http://cran.us.r-project.org")
library(randomForest)
library(data.table)

#This script will return the random forest implementation
#It's too time-consuming to run on my computer, so I'll run it on an EC2 instance and save the results as an .RData file
nfolds = 5
subsets = kFolds(df[,1], nfolds)
rf.digits = list()
rf.predicts = list()
rf_df = data.table(df)
rf_df$V1 = as.factor(rf_df$V1)
for(i in 1:nfolds) {
  print(paste("Starting Fold", i))
  rf.digs[[i]] = randomForest(rf_df[subsets!=i, 2:151],rf_df[subsets!=i,1][[1]], mtry=20, importance=T, keep.forest = TRUE)
  rf.predicts[[i]] = predict(rf.digits[[i]], rf_df[subsets==i,2:151])
  print(paste("Done with Fold", i))
}
warns = warnings()
#hi isaac (:
print("Done")
save.image("Forest.RData")