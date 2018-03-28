#--------------------------------------------------------------------------------
#Function to subset the data into sections of size k. 
#For each row in dataset, returns a number 1 through k.
kFolds = function(dataset, k) {
  return(sample(1:k, NROW(dataset), replace=TRUE))
}
#--------------------------------------------------------------------------------
#Get unique colors for plotting purposes
Cols=function(vec) {
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
#--------------------------------------------------------------------------------
#See if x is full row rank
is.uniq = function(x) {
  if(length(unique(x))==1) {
    return(T)
  }
  else {
    return(F)
  }
}
#--------------------------------------------------------------------------------
pixpos = function(n) {
  y = 27-floor(n/28)
  x = n%%28
  return(c(x,y))
}
#--------------------------------------------------------------------------------
num2hex = function(n) {
  n = 255-n
  first = floor(n/16)
  second = n%%16
  if (first>9) {
    first = rawToChar(as.raw(first+55))
  }
  if (second>9) {
    second = rawToChar(as.raw(second+55))
  }
  both = paste(c(toString(first), toString(second)), collapse="")
  return(paste(c("#",both,both,both),collapse=""))
}
#--------------------------------------------------------------------------------
digitmap = function(col_list) {
  plot(c(0,28), c(0,28), type="n")
  abline(v=(seq(0,28,1)), col="lightgray")
  abline(h=(seq(0,28,1)), col="lightgray")
  for (i in 2:785) {
    b = pixpos(i-2)
    if (col_list[i]>0) {
      rect(b[1], b[2], b[1]+1, b[2]+2, col=num2hex(col_list[i]),
           border=num2hex(col_list[i]))
    }
  }
}
#--------------------------------------------------------------------------------
#
knnclass = function(x, trainx, trainy, k) {
  new = apply(dist2(x,trainx), 1, function(g) 
    levels(as.factor(trainy))
    [which.max(tabulate(as.factor(trainy[match(sort(g, partial=1:k)[1:k],g)])))])
  return(new)
}
#--------------------------------------------------------------------------------

dir = "~/Desktop/post-grad-learning/digit_detection"

setwd(dir)
load(paste(dir, '/.RData', sep=""))
save.image(paste(dir, '/.RData', sep=""))

library(MASS)
library(Matrix)
library(randomForest)
install.packages('ISLR')
install.packages('glmnet')
install.packages('gam')
install.packages('tree')


digits = read.csv("trainingdigits.csv", header=T)

#Only keeps pixel locations that have a variance of > 0
scalable_digs = digits[,which(apply(digits, 2, var, na.rm=T) != 0)]

#Output principal component variables
pr.out = prcomp(scalable_digs[,-1], scale=T)
df = data.frame(cbind(scalable_digs[,1], pr.out$x[,1:150]))

#Create nfold separate subsets of the data to later test the accuracy of each method
nfolds = 10
subsets = kFolds(df[,1], nfolds)


#Applying QDA to the dataset
qda.fit = list()
qda.class = list()
for(i in 1:nfolds) {
  qda.fit[[i]] = qda(df[subsets!=i, 2:150],df[subsets!=i,1])
  qda.class[[i]] = predict(qda.fit[[i]], df[subsets==i, 2:150])$class
}
table(qda.class[[1]], scalable_digs[subsets==1, 1])

#
kfoldpercentages = vector()
for (i in 1:10){
  kfoldpercentages = c(kfoldpercentages, sum(qda.class[[i]]==scalable_digs[subsets==i,1])/length(qda.class[[i]]))
}
mean(kfoldpercentages)
load('~/Dropbox/Layton-SeniorProject/Forest.RData')
rf.digitsubset = digitsubset
table(rf.predicts[[1]], scalable_digs[rf.digitsubset==1, 1], 
      dnn=c("KNN Predicted", "Actual"))
rf.percentages = vector()
for (i in 1:10){
  rf.percentages = c(rf.percentages, sum(rf.predicts[[i]]==scalable_digs[rf.digitsubset==i,1])/length(rf.predicts[[i]]))
}
mean(rf.percentages)
save.image('~/Dropbox/Layton-SeniorProject/knn.RData')
knn.digitsubset = digitsubset
table(knntests[[1]],scalable_digs[knn.digitsubset==1,1])
knnpercentages = vector()
for (i in 1:10){
  knntests[[i]] = knntests[[i]]-1
  knnpercentages = c(knnpercentages, sum(knntests[[i]]==scalable_digs[digitsubset==i,1])/length(knntests[[i]]))
}
mean(knnpercentages)

knntests[[1]][1:10]