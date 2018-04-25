load(".RData")
if(!require(randomForest)) install.packages('randomForest', repos = "http://cran.us.r-project.org")
library(randomForest)

#This script will return the random forest implementation
#It's too time-consuming to run on my computer, so I'll run it on an EC2 instance and save the results as an .RData file
#nfolds = 10
#subsets = kFolds(df[,1], nfolds)
rf.digits = list()
rf.predicts = list()
#df$V1 = as.factor(df$V1)
rf.digit = randomForest(training[, 2:226],training[,1], mtry=15, importance=T, keep.forest = TRUE)
rf.predict = predict(rf.digit, validation[,2:226])
warns = warnings()
#hi isaac (:
print("Done")
save.image("Forest.RData")