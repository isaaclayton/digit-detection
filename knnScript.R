load(".RData")
if(!require(flexclust)) install.packages('flexclust', repos = "http://cran.us.r-project.org")
library(flexclust)

knntests = list() 
for(i in 1:15){
  knntests[[i]] = knnclass(validation[,2:226],
                           training[,2:226], training[,1],
                           k=i)
  print("One more done!")
}
print("Done")
save.image("~/knn.RData")