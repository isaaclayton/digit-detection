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
#Subset the data into two sections
subsetFunc = function(data, proportion) {
  return(sample(1:NROW(data), NROW(data)*proportion))
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
#-------------------------------------------------------------------------------
#
knnclass = function(x, trainx, trainy, k) {
  new = apply(dist2(x,trainx), 1, function(g) 
    levels(as.factor(trainy))
    [which.max(tabulate(as.factor(trainy[match(sort(g, partial=1:k)[1:k],g)])))])
  return(new)
}
#-------------------------------------------------------------------------------
#QDA Function
QDAFunc = function(testx, trainx, trainy) {
  y_set = sort(unique(trainy))
  cov_k = by(trainx, trainy, cov)
  cov_inv = lapply(cov_k, solve)
  prop_k = by(trainy, trainy, FUN=function(x) length(x)/length(trainy))
  mean_k = apply(trainx, 2, FUN= function(x) by(unlist(x), trainy, mean))
  subtract = sapply(c(1:length(y_set)), FUN=function(i) (-0.5*(t(mean_k[i,])%*%cov_inv[[i]]%*%mean_k[i,]) 
                                              - (0.5*unlist(determinant(cov_k[[i]]))[1])+ log(prop_k[i])))
  QDA_vals = apply(testx, 1, FUN=function(x) y_set[which.max(lapply(c(1:length(y_set)), FUN= function(i) (
    -0.5*(t(x)%*%cov_inv[[i]]%*%as.matrix(x)) + (t(x)%*%cov_inv[[i]]%*%mean_k[i,]) + subtract[i])))])
  return(QDA_vals)
}
#-------------------------------------------------------------------------------
#Random forest function?
rfFunc = function(testx, trainx, trainy) {
  
}
#-------------------------------------------------------------------------------
dir = "~/Desktop/post-grad-learning/digit_detection"

setwd(dir)
load(paste(dir, '/.RData', sep=""))
save.image(paste(dir, '/.RData', sep=""))

#install.packages('ISLR')
#install.packages('glmnet')
#install.packages('gam')
#install.packages('tree')
#install.packages('flexclust')
library(MASS)
library(Matrix)
library(randomForest)
library(flexclust)

digits = read.csv("trainingdigits.csv", header=T)
sample_digit = digits[21,]

#Only keeps pixel locations that have a variance of > 0
scalable_digs = digits[,which(apply(digits, 2, var, na.rm=T) != 0)]

#Output principal component variables
pr.out = prcomp(scalable_digs[,-1], scale=T)
pr.var=pr.out$sdev ^2
pve=pr.var/sum(pr.var)
#plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Number of principal component factors", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type='b')
abline(h=cumsum(pve)[225])
cumsum(pve)[225]
df = data.frame(cbind(scalable_digs[,1], pr.out$x[,1:225]))

rm(scalable_digs)
rm(pr.out)
rm(digits)

test_divide = subsetFunc(df, 0.8)
test = df[-test_divide,]
training = df[test_divide,]
validation_divide = subsetFunc(training, 0.8)
validation = training[-validation_divide,]
training = training[validation_divide,]
rm(validation_divide)
rm(test_divide)

rm(df)

#Applying QDA to the dataset
qda_model = QDAFunc(validation[, 2:226], training[, 2:226], training[[1]])
table(qda_model, validation[,1])
qda_percentage = sum(qda_model==validation[,1])/length(qda_model)

load(paste(dir, '/Forest.RData', sep=""))
table(rf.predict, validation[, 1], 
      dnn=c("Predicted", "Actual"))
rf.percentage = sum(rf.predict==validation[,1])/length(rf.predict)
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
