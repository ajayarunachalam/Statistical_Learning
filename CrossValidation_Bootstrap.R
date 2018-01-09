# The validation Set Approach

library(ISLR)
set.seed(1)

# split the set of observations into two halves
train = sample(392, 196)

# fit a linear regression
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)

# estimate the response and calculate MSE in validation dataset
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

# Quadratic and cubic regression
lm.fit2 = lm(mpg~poly(horsepower, 2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower, 3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# If we choose different training set, errors will be different
set.seed(2)
train = sample(392, 196)
lm.fit = lm(mpg~horsepower, subset = train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower, 2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower, 3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# Leave one out Cross Validation
glm.fit = glm(mpg~horsepower, data = Auto) #linear regression
coef(glm.fit)

lm.fit = lm(mpg~horsepower, data=Auto)
coef(lm.fit)

# glm() can be used for cross validation
library(boot)
glm.fit = glm(mpg~horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

# complex fits LOOCV
cv.error = rep(0,5)
for(i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

# K-Fold Cross validation

set.seed(17)
cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

# Bootstrap

# Estimating the accuracy of a Statistic of Interest
alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100,100, replace=T))

# boot() function automates this approach
boot(Portfolio, alpha.fn, R=1000)

# Estimating the accuracy of a linear Regression model
boot.fn = function(data, index){
  return (coef(lm(mpg~horsepower, data = data, subset = index)))
}
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace = T))
boot.fn(Auto,sample(392,392,replace = T))

boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower, data = Auto))$coef

boot.fn = function(data, index){
  coefficients(lm(mpg~horsepower+I(horsepower^2), data = data, subset = index))
}
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef
