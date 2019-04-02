#Statistical Learning_Homework6
#107064522

##### 1.(a) #####
# install.packages("tree")
library(tree)
library(MASS)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)  # takes a sample of the specified size from Boston
tree.boston = tree(medv~., Boston, subset = train)
summary(tree.boston)
cor(Boston[train,], Boston$medv[train])
plot(tree.boston)
text(tree.boston)
tr = Boston[train,]

##### 1.(b) #####
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
tree = rpart(tree.boston)
summary(tree)
rpart.plot(tree)

a =(tr$lstat<9.715) & (tr$rm>=7.437)
summary(tr$medv[a])

##### 1.(c) #####
# 5-fold Cross Validation
set.seed(1)
cv_tree_5 = cv.tree(tree.boston, K = 5)  # K-folds CV
cv_tree_5  
plot(cv_tree_5$size, cv_tree_5$dev, type="b", main = "5-fold Cross Validation")
# 10-fold Cross Validation
set.seed(1)
cv_tree_10 = cv.tree(tree.boston, K = 10)  # K-folds CV
cv_tree_10  
plot(cv_tree_10$size, cv_tree_10$dev, type="b", main = "10-fold Cross Validation")
#cv_tree$k[which.min(cv_tree$dev)]
prune_tree = prune.tree(tree.boston, best = 7)
plot(prune_tree)
text(prune_tree)

##### 1.(d) #####
test = Boston[-train,]
boston_test = Boston[-train, "medv"]

pred = predict(prune_tree, newdata = test)  #利用修剪完的tree對test data做預測
MSE = mean((pred - test$medv)^2)  # 計算MSE
RMSE = sqrt(MSE)

plot(pred, boston_test, xlab = "fitted value", ylab = "real value")
abline(0, 1)

##### 2.(a) #####
# Bagging
#install.packages("magrittr")
library(magrittr)
#install.packages("randomForest")
library(randomForest)
set.seed(1)
bag = randomForest(medv~., Boston, subset = train, mtry = 13)

pred_bag = predict(bag, newdata = test)
MSE = mean(( pred_bag - boston_test)^2) 
RMSE = MSE %>% sqrt

plot(pred_bag, boston_test, xlab = "fitted value", ylab = "real value")
abline(0, 1)

##### 2.(b) #####
set.seed(1)
bag2 = randomForest(medv~., Boston, subset = train, mtry = 13, ntree = 25)
pred_bag2 = predict(bag2, newdata = test)
MSE2 = mean(( pred_bag2 - boston_test)^2) 
RMSE2 = MSE2 %>% sqrt

plot(pred_bag2, boston_test, xlab = "fitted value", ylab = "real value")
abline(0, 1)

##### 2.(c) #####
set.seed(1)
bag3 = randomForest(medv~., Boston, subset = train, mtry = 5)
pred_bag3 = predict(bag3, newdata = Boston[-train,])
MSE3 = mean(( pred_bag3 - boston_test)^2) 
RMSE3 = MSE3 %>% sqrt

plot(pred_bag3, boston_test, xlab = "fitted value", ylab = "real value")
abline(0, 1)

plot(pred_bag, boston_test, xlab = "fitted value", ylab = "real value", pch = 20)
points(pred_bag2, boston_test, pch = 20, col = 2)
points(pred_bag3, boston_test, pch = 20, col = 3)
abline(0, 1, lwd = 2)
legend("topleft", pch = c(20,20,20), col = c(1,2,3), legend = c("2a","2b","2c"))
