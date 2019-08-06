library(ISLR)
names(Hitters)
dim(Hitters) # 322 rows and 20 columns
sum(is.na(Hitters$Salary)) #59: Salary is missing for 59 player

#na.omit() function removes all of the rows that have missing values in any variable
Hitters=na.omit(Hitters) 
dim(Hitters)
sum(is.na(Hitters))


#----Best Selection--------------------

#The regsubsets() function (part of the leaps library)
#performs best subset selection by identifying the best model 
#that contains a given number of predictors, where best is quantiﬁed using RSS.
library(leaps)
regit.full <- regsubsets(Salary~., Hitters)
summary(regit.full)

#by default, regsubsets() onlyl reports results up to the 
#best eight-variable model
#use nvmax to fit
regfit.full <- regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary <- summary(regfit.full)

names(reg.summary)
reg.summary$rsq
par(mfrow=c(2, 2))
plot(reg.summary$rss, xlab='Number of Variables', ylab='RSS',
     type='l')
plot(reg.summary$adjr2, xlab='Number of Variables', ylab=
       'Adjusted Rsq', type='l')

#The which.max() function can be used to identify the location of the maximum point of a vector
#The points() command works like the plot() command, except that it puts points on a plot that has already been created, instead of creating a new plot.
#For R^2 and adjusted R^2 , the value should be larger
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11],col='red', cex=2, pch=20)

plot(reg.summary$cp, xlab="Number of Variables", ylab='Cp', type='l')
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col='red', cex=2, pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab='Number of Variables', ylab='BIC', type='l')
points(6, reg.summary$bic[6], col='red', cex=2, pch=20)

coef(regfit.full, 6)


#--------------Forward and Backward Stepwise Selection----------
#forward stepwise selection
regfit.fwd = regsubsets(Salary~., data=Hitters, nvmax=19, method='forward')
summary(regfit.fwd)
#backward stepwise selection
regfit.bwd = regsubsets(Salary~., data=Hitters, nvmax=19, method='backward')
summary(regfit.bwd)

#use train & validation & cross validation
set.seed(1)
train=sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test=(!train)

regfit.best = regsubsets(Salary~., data=Hitters[train,], nvmax=19)
#The model.matrix() function is used in many regression packages for building an “X” matrix from data.
test.mat = model.matrix(Salary~., data=Hitters[test,])
val.errors=rep(NA, 19)
for (i in 1:19){
  coefi =  coef(regfit.best, id=i)
  pred = test.mat[, names(coefi)]%*%coefi #use the column value * coefficients
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best, 10)


predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}

#perform best subset selection on the full data set and select the best ten-variable model
regfit.best = regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best, 10)

#create a vector that allocates each observation to one of k = 10 folds, and we create a matrix in which we will store the results.
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors = matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

for (j in 1:k){
  best.fit = regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
  for (i in 1:19){
    pred= predict.regsubsets(best.fit, Hitters[folds ==j, ], id=i)
    cv.errors[j, i] = mean((Hitters$Salary[folds==j] - pred)^2)
  }
} 

mean.cv.errors = apply(cv.errors, 2, mean) #sekect an 11-variable model
mean.cv.errors  
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')

reg.best = regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best,11)

#----------------Ridge Regression-----------------
x = model.matrix(Salary~., Hitters)[, -1] #-1 deletes the intercept
y = Hitters$Salary

#glmnet() can only take numerical, quantitative inputs
#The glmnet() function has an alpha argument that determines what type of model is ﬁt. 
#If alpha=0 then a ridge regression model is ﬁt, and if alpha=1 then a lasso model is ﬁt.
library(glmnet)
grid = 10 ^seq(10, -2, length=100) #implement the function over a grid of values ranging from lambda=10^10 t0 lambda 10^-2
ridge.mod = glmnet(x, y, alpha=0, lambda=grid)
#Note that by default, the glmnet() function standardizes the variables so that they are on the same scale. To turn oﬀ this default setting, use the argument standardize=FALSE.

dim(coef(ridge.mod)) # 20 rows(one for each predictor, plus an intercept); 100 columns (one for each value of lambda) 
ridge.mod$lambda[50] #11498
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1,50]^2)) #6.36

ridge.mod$lambda[60] #705
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))  #57.11
#Note the much large lambda2 norm of the coefficients associated with this smaller lamda

predict(ridge.mod, s=50, type='coefficients')[1:20,]

set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred = predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred - y.test)^2) #101037

#if just use the intercept
mean((mean(y[train])-y[test])^2) #193253

ridge.preed = predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2) #101037

#least square
ridge.pred=predict(ridge.mod, s=0, newx=x[test,], exact=T) #跑不出来不知道维护
mean((ridge.pred - y.test)^2)


#using built-in cross-validation function, cv.glmnet(), 10fold
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam #find the best lambda, 212

ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2) #96015.51

#As expected, none of the coeﬃcients are zero—ridge regression does not perform variable selection!
out = glmnet(x, y, alpha=0)
predict(out, type='coefficients', s=bestlam)[1:20,]


#------------------Lasso regression-------------------
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
#plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min #select the best lambda
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred - y.test)^2) #100743.3

out=glmnet(x, y, alpha=1, lambda = grid)
lasso.coef = predict(out, type='coefficients', s=bestlam)[1:20,]
lasso.coef










