load(".RData")
knntests = list() 
for(i in 1:15){
  knntests[[i]] = knnclass(validation[,2:151],
                           training[,2:151], training[,1],
                           k=i)
  print("One more done!")
}
print("Done")
save.image("~/knn.RData")