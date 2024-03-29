library(tree)
library(ISLR)
attach(Carseats)

#---------------------classification tree----------------------
High = ifelse(Sales <= 8, 'No', 'Yes')
#Use teh data.frame() function to merge High with the rest of Carseats data
Carseats = data.frame(Carseats, High)
dim(Carseats) #400rows, 12cols

tree.carseats =tree(High~.-Sales,Carseats )
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats)

#R displays the split criterion (e.g. Price<92.5), 
#the number of observations in that branch, the deviance, 
#the overall prediction for the branch (Yes or No),
#and the fraction of observations in that branch that take on values of Yes and No
tree.carseats

set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]

tree.carseats = tree(High~. -Sales, Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type='class')
table(tree.pred, High.test)
mean((tree.pred==High.test)) #0.715


set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats #dev corresponds to the cross-validation error rate 
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type='b')
plot(cv.carseats$k, cv.carseats$dev, type='b')

#prune the tree to obtain the nine-node tree
prune.carseats = prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred = predict(prune.carseats, Carseats.test, type='class')
table(tree.pred, High.test)
mean((tree.pred==High.test)) #0.77

#prune the tree to obtain the fifteen-node tree
prune.carseats = prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred = predict(prune.carseats, Carseats.test, type='class')
table(tree.pred, High.test)
mean((tree.pred==High.test)) #0.74

#--------------Fitting Regression Trees----------------------
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.Boston = tree(medv~., Boston, subset = train)
summary(tree.Boston)

plot(tree.Boston)
text(tree.Boston, pretty=0)

cv.boston = cv.tree(tree.Boston)
plot(cv.boston$size, cv.boston$dev, type='b')

prune.boston = prune.tree(tree.Boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

yhat = predict(tree.Boston, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

#---------Bagging and Random Forests----------
library(randomForest)

#bagging is simply a special case of a random forest with m=p
set.seed(1)
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston

yhat.bag = predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2)

bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

set.seed(1)
rf.boston = randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf = predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf - boston.test)^2)
#Using the importance() function, we can view the importance of each variable.
importance(rf.boston)
varImpPlot(rf.boston)

#---------------Bagging-----------------
library(gbm)
set.seed(1)

boost.boston = gbm(medv~., data=Boston[train, ], distribution='gaussian',
                   n.trees=5000, interaction.depth = 4)
#We see that lstat and rm are by far the most important variables.
summary(boost.boston)

par(mfrow=c(1, 2))
plot(boost.boston, i='rm')
plot(boost.boston, i='lstat')

#default lambda: 0.001
boost.boston = gbm(medv~., data=Boston[train, ], distribution='gaussian',
                   n.trees=5000, interaction.depth = 4, shrinkage = 0.2, verbose=F)
yhat.boost = predict(boost.boston, newdata=Boston[-train,], n.trees = 5000)
# In this case, using lambda = 0,2 leads to a slightly lower test MSE than lambda = 0.001
mean((yhat.boost-boston.test)^2)














