train <- sample(1:nrow(cervical_cancer), .66*nrow(cervical_cancer))
 cc_train <- cervical_cancer[train,]
 cc_test <- cervical_cancer[-train,]
 y_true_train <- cc_train$ca_cervix
 y_true_test<-cc_test$ca_cervix
 glm.fit <- glm(ca_cervix~., data = cc_train, family = "binomial")
 summary(glm.fit)
 glm.probs.train <- predict(glm.fit, newdata = cc_train, type = "response")
 y_hat_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = cc_test, type = "response")
y_hat_test <- round(glm.probs.test)
train_err <- sum(abs(y_hat_train- y_true_train))/length(y_true_train)
test_err <- sum(abs(y_hat_test- y_true_test))/length(y_true_test)
install.packages("e1071")
library(caret)
  conf <- confusionMatrix(as.factor(y_hat_test), as.factor(y_true_test))
pca<-prcomp(cervical_cancer,center=T,scale=T)
pca$x
pcadf<-cervical_cancer[,c(12,13,14,15,16,17,18,19,20)]
View(pcadf)
train <- sample(1:nrow(pcadf), .66*nrow(pcadf))
pca<-prcomp(cervical_cancer,center=T,scale=T)
pc_train <- pcadf[train,]
pc_test <- pcadf[-train,]
p_true_train <- pc_train$ca_cervix
p_true_test <- pc_test$ca_cervix


pglm.probs.train <- predict(pglm.fit, newdata = pc_train, type = "response")
py_hat_train <- round(pglm.probs.train)
pglm.probs.test <- predict(pglm.fit, newdata = pc_test, type = "response")
py_hat_test <- round(pglm.probs.test)
train_err <- sum(abs(y_hat_train- y_true_train))/length(y_true_train)
ptrain_err <- sum(abs(py_hat_train- p_true_train))/length(p_true_train)
ptest_err <- sum(abs(py_hat_test- p_true_test))/length(p_true_test)

lda.fit<-lda(ca_cervix~.,data=cc_train)
library(klaR)  
library(MASS)

plda.fit<-lda(ca_cervix~.,data=pc_train)
plda.pred.train <- predict(plda.fit, newdata = pc_train)
ply_hat_train <- as.numeric(plda.pred.train$class)-1
plda.pred.test <- predict(plda.fit, newdata =pc_test )
ply_hat_test <- as.numeric(plda.pred.test$class)-1
plda_train_error <- sum(abs(p_true_train - py_hat_train))/length(p_true_train) 
plda_test_error <- sum(abs(p_true_test - py_hat_test))/length(p_true_test)
