library(ISLR)
names(Smarket)
dim(Smarket) # 1250 rows and 9 columns
summary(Smarket)

cor(Smarket) #Error message: Direction variable is qualitative
cor(Smarket[, -9])

attach(Smarket)
plot(Volume)

#-----Logistic Regression-------------------------
#Use glm() function
# 'family=binomial' --> logistic regression
glm.fit = glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5
              + Volume, data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit) #access the coefficients for this fitted models
summary(glm.fit)$coef
summary(glm.fit)$coef[, 4]
#if no data set is supplied to the predict() function,
#default by training data
glm.probs = predict(glm.fit, type='response') 
glm.probs[1:10]
#contrasts() function indicates that R has created a dummy variable with a 1 for Up
contrasts(Direction)
glm.pred = rep('Down', 1250) # a vector of character
glm.pred[glm.probs > 0.5] = 'Up'
#confusion matrix
table(glm.pred, Direction)
mean(glm.pred == Direction) #train error but not test error

train = (Year < 2005)
Smarket.2005 = Smarket[!train, ] #a submatrix selection
dim(Smarket.2005) #only contain data in 2015
Direction.2005 = Direction[!train]
#use the trained model to predict test data
glm.fit <- glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type='response')

glm.pred <- rep("Down", 252)
glm.pred[glm.probs>0.5] = 'Up'
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) #0.48
# After all, using predictors that have no relationship with the response tends
# to cause a deterioration in the test error rate（since such predictors cause an 
# increase in variance without a corresponding decrease in bias）

# Use less variables
glm.fit <- glm(Direction ~ Lag1 + Lag2, data=Smarket, family=binomial,
               subset=train)
glm.probs <- predict(glm.fit, Smarket.2005, type='response')
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = 'Up'
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

#Suppose that we want to predict the returns associated with particular values 
#of Lag1 and Lag2. In particular, we want to predict Direction on a day
#when Lag1 and Lag2 equal 1.2 and 1.1, respectively,
#and on a day when they equal 1.5 and −0.8. We do this using the predict() function.
predict(glm.fit, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)),
        type='response')



#----------Linear Discriminant Analysis--------------------
# use lda() function which is part of the MASS library
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[, 1] >=0.5)
sum(lda.pred$posterior[, 1] < 0.5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]


#-----------K-Nearest Neighbors-------------
#knn() requires four inputs
# 1. A matrix containing the predictors associated with the training data, labeled train.X below.
# 2. A matrix containing the predictors associated with the data for which we wish to make predictions, labeled test.X below.
# 3. A vector containing the class labels for the training observations, labeled train.Direction below.
# 4. A value for K, the number of nearest neighbors to be used by the classiﬁer.
library(class)
train.X = cbind(Lag1, Lag2)[train, ]
test.X = cbind(Lag1, Lag2)[!train, ]
train.Direction = Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
#k changes to 3
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

#------An implication to Caravan Insurance Data------
dim(Caravan)
attach(Caravan)
summary(Purchase)

#knn needs to scale
#A good way to handle this problem is to standardize the data so that 
#all variables are given a mean of zero and a standard deviation of one.
standardized.X = scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
#now every column of `standardized.X` has a standard deviation of one and a mean of zero.
var(standardized.X[, 1])
var(standardized.X[, 2])

test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No" )
table(knn.pred, test.Y)
#k=1
table(knn.pred, test.Y)[2, 2]/ sum(table(knn.pred, test.Y)[2, ])

#k=3
knn.pred <- knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)[2, 2]/ sum(table(knn.pred, test.Y)[2, ])

#k=5
knn.pred <- knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)[2, 2]/ sum(table(knn.pred, test.Y)[2, ])

#use logistic regression model to fit
glm.fit <- glm(Purchase~., data=Caravan, family=binomial,
               subset= -test)
glm.probs = predict(glm.fit, Caravan[test,], type='response')
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.5] = 'Yes'
table(glm.pred, test.Y)
#use p = 0.25 as the threshold-
glm.pred=rep ("No" ,1000)
glm.pred[glm.probs >.25]="Yes"








