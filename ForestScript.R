load(".RData")
if(!require(randomForest)) install.packages('randomForest', repos = "http://cran.us.r-project.org")
library(randomForest)

#This script will return the random forest implementation
#It's too time-consuming to run on my computer, so I'll run it on an EC2 instance and save the results as an .RData file
#nfolds = 10
#subsets = kFolds(df[,1], nfolds)
rf.digits = list()
rf.predicts = list()
df$V1 = as.factor(df$V1)
for(i in 1:10) {
  print(paste("Starting ", i))
  rf.digits[[i]] = randomForest(training[, 2:151],training[,1], mtry=i*5, importance=T, keep.forest = TRUE)
  rf.predicts[[i]] = predict(rf.digits[[i]], validation[,2:151])
  print(paste("Done with ", i*5))
}
warns = warnings()
#hi isaac (:
print("Done")
save.image("Forest.RData")