load(".RData")
if(!require(flexclust)) install.packages('flexclust', repos = "http://cran.us.r-project.org")
library(flexclust)

knntests[[i]] = knnclass(test[,2:226],training[,2:226], training[,1], k=6)
print("Done")

save.image("test.RData")