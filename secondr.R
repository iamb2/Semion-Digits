#######################################################
# This code utilizes kNN for different values of k
#
# Rachael Hageman Blair
# Created: 8/1/2012
# Modified: 9/12/2019
#######################################################

rm(list = ls())
setwd("~/Dropbox/STA_545_Fall2019/Comp_Labs")

#install.packages("ggplot2")
library("ggplot2")

##################################
#  Load and visualize the data
#  
##################################
data <- read.delim("clipped_data.txt", sep = "\t", header= FALSE)
train <- data.frame(X1 = data[,1], X2 = data[,2], Y = data[,3])
dim(train)
head(train)
g <- ggplot(train, aes(X1,X2)) + geom_point(aes(colour = as.factor(Y))) + theme(legend.position = "none")
quartz()
plot(g)
ggsave(filename = "orig.png", plot = g, height = 5, width = 5)

######################################################
#  Create a test data set .... 
#  a grid of values spanning the ranges of X1 and X2
#  (long term goal: visualization)
######################################################
minX1 <- min(train$X1)
minX2 <- min(train$X2)
maxX1 <- max(train$X1)
maxX2 <- max(train$X2)

# ranges
X1.range <- seq(from = minX1, to = maxX1, length.out = 100)
X2.range <- seq(from = minX2, to = maxX2, length.out = 100)

# Create the test set
test <- data.frame(X1 = rep(X1.range, 100), X2 = rep(X2.range, each = 100))
g2 <- ggplot(test, aes(X1,X2)) + geom_point(size = 0.5)
quartz()
plot(g2)

################################
## Try different values of k
################################
require(class)
knnplot <- function(train, test, k){
  KNN <- knn(train[, c('X1', 'X2')], test, train$Y, k)
  test$predict <- KNN
  
  # change factor to numeric
  test$z <- c(0, 1)[sapply(test$predict, as.numeric)]
  
  title = paste("k=", as.character(k), sep ="")
  
  g <- ggplot(data = test, aes(X1,X2)) + geom_point(aes(colour = predict), size = 0.5) + geom_contour(aes(z=z), colour = 'black', size = 0.1) + theme(legend.position = "none") + labs(title = title)
  
  #add the training points in
  g <- g + geom_point(data = train, aes(X1,X2,colour = as.factor(Y), shape = 'x'))
  
  return(g)
  
}

###############################################
## Try differnt values of k, and save
###############################################
filer <- paste("k", c(1:10), ".png", sep="")
for (i in 1:10){
  p <- knnplot(train, test, i)
  ggsave(filename = filer[i], plot = p, height = 5, width = 5)
}





### Psuedo code

# done
my_train
my_test

# create a loop to look at kNN for different values of k
k_vals <- c(1, 3, 5, 7, 9, 11)
store_error <- c()
for (i in 1:6){
  
  # select "k"
  kk <- k_vals[i]
  
  # apply the algorithm
  fit <- knn(my_train, my_test, my_train[,1], k = kk)
  
  # check the answer
  fit
  
  # make a call --- you may have 2.1 2.7 ----> "round"
  # ?round
  
  # calculate the error
  which(fit != my_test[,1])
  
}

####################################
q3 winedata set
train <- sample(1:nrow(cervical_cancer), .66*nrow(cervical_cancer))
splitthisdata<-winedatafile[,2:14]#
cc_train <- cervical_cancer[train,]
cc_test <- cervical_cancer[-train,]
y_true_train <- cc_train$ca_cervix
y_true_test<-cc_test$ca_cervix


#################################


library(caret)

preproc1 <- preProcess(knndataset[1:169,2:14], method=c("center", "scale"))

norm1 <- predict(preproc1, knndataset[1:169,2:14])
ggplot(norm1) + geom_histogram(aes(Malic.acid),bins = 50)

summary(norm1)

norm1$cultivar<-cultivardata

ktrain <- sample(1:169, .66*nrow(norm1))
kc_train <- norm1[train,]
kc_test <- norm1[-train,]

k_true_train <- kc_train$cultivar
k_true_test<-kc_test$cultivar

require(class)

  KNN <- knn(kc_train[,1:13], kc_test[,1:13], k_true_train, k=5)
  
  
  
  ktest_err <- sum(abs(as.numeric(KNN)-as.numeric(k_true_test)))/length(k_true_test)
  ktest_err 

  0.03448276 
  
  

plot(KNN,k_true_test)

KNN <- knn(kc_train[,1:13], winedatafiletest[,1:13], k_true_train, k=5)
 wtest_err <- sum(abs(KNN- k_true_test))/length(k_true_test)
 
 
 preproc2 <- preProcess(winedatafiletest[1:9,2:13], method=c("center", "scale"))
 
 norm2 <- predict(preproc1, winedatafiletest[1:9,1:13])
 ggplot(norm1) + geom_histogram(aes(Malic.acid),bins = 50)

library(e1071)
data(iris)
 nB_model <- naiveBayes(knndataset[,:], iris[,5]) 
 
 nn<-predict(nB_model, iris[,-5])
 
 
 ##############################
 install.packages("randomForest")
 library(randomForest)
 set.seed(111)
 ind<- sample(2,nrow(norm1),replace=TRUE,prob=c(0.8,0.2))
 rnd.rf<-randomForest(cultivar~., data=norm1[ind==1,])
 rnd.pred <- predict(rnd.rf, norm1[ind == 2,])
 true_test=norm1[ind == 2,]  // rnd.pred 
 true_test_c=true_test$cultivar
 r=round(rnd.pred)
 ptest_err <- sum(abs(true_test_c- r))/length(true)
 rnd.rf<-randomForest(cultivar~., data=norm1[ind==1,])
 rnd2.pred <- predict(rnd.rf, norm2)
 round(rnd2.pred)
 ###############

 ###############
 r=round(rnd.pred)
 table(observed = iris[ind==2, "Species"], predicted = iris.pred)
 
 ## Look at variable importance: (by default, Gini index)
 round(importance(iris.rf), 2)
 
 plot(iris.rf)
 # red = setosa error rate, green=versicolor error, blue=virginica error, black = OOB error
 
 
 # prediction for all trees
 predict(iris.rf, knndataset[ind == 2,], predict.all=TRUE)
 predict(iris.rf, knndataset[ind == 2,], proximity=TRUE)
 
 ##################################################
 kwinepca <- prcomp(knndataset, scale.= TRUE,center = TRUE )
 pca3d(kwinepca, group=knndataset[,1])
 
 makeMoviePCA()
 
 
 ######################################

 
 nbtrain <- sample(1:nrow(norm1), .66*nrow(norm1))
 nbc_train <- norm1[nbtrain,]
 nbc_test <- norm1[-nbtrain,]
 nby_true_train <- nbc_train$cultivar
 nby_true_test<-nbc_test$cultivar
 
 nB_model <- naiveBayes(nbc_train[,1:13], nbc_train$cultivar) 
 
 nn<-predict(nB_model, nbc_train[,1:13])
 
 
 nbtrain_err <- sum((as.numeric(nn) - as.numeric(nby_true_train)))/length(nby_true_train)

 
 nbtest_err

 
 nn
 















conf <- confusionMatrix(as.factor(nnt), as.factor(nby_true_test))
conf


###########################random forests nyps
set.seed(12345)
modfit<-randomForest(norm1$cultivar~., data=norm1)


rf.pred.training=predict(modfit,df_training,type="class")
confussion.training<-confusionMatrix(rf.pred.training, cultivar)
confussion.training


rf.cv <- rfcv(norm1[,1:13], norm1$cultivar, cv.fold=10)
with(rf.cv, plot(n.var, error.cv, type="b", col="red"))



a11=norm1[ind == 2,]  // rnd.pred 
xxx=a11$cultivar
rrr=round(rnd.pred)
ptest_err <- sum(abs(xxx- rrr))/length(xxx)


rnd.rf<-randomForest(cultivar~., data=norm1[ind==1,])
rnd2.pred <- predict(rnd.rf, norm2)
round(rnd2.pred)








