library(MASS)
library(ISLR)

# Simple Linear Regression
fix(Boston)
names(Boston)

# lm() function will be used to fit a simple linear regression model
# medv as the response and lstat as the predictor
# medv is median house value
# lstat is percent of households with low socioeconomic status

lm.fit = lm(medv~lstat, data=Boston)
attach(Boston) #attach the Boston dataset, so that R recognizes its variables
lm.fit = lm(medv~lstat)

# some basic information about the model is output using
lm.fit

#For getting residuals, coefficients (estimates, standard error, t-statistic, p-value)
# R^2 value, F-statistic
summary(lm.fit)

# what other information is stored in lm.fit?
names(lm.fit)

# coefficients
coef(lm.fit)

# confidence interval
confint(lm.fit)

# predict() function can be used to produce confidence intervals
# and prediction intervals for the prediction of medv for a given value of lstat

predict(lm.fit, data.frame(lstat=c(5,10,15)),
        interval = "confidence")

predict(lm.fit, data.frame(lstat=c(5,10,15)),
        interval = "prediction")

# plot medv and lstat along wit least squares regression line

plot(lstat, medv)
abline(lm.fit)

# some experiments with plot and abline
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)

# make subplots
par(mfrow = c(2,2))
plot(lm.fit)

# residuals can be computed using residuals() function
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# leverage statistics can be computed for any number of predictors
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) #index of the largest element of the vector

# Multiple Linear Regression

lm.fit <- lm(medv~lstat+age, data=Boston)
summary(lm.fit)

# Regression using all predictors
lm.fit <- lm(medv~., data = Boston)
summary(lm.fit)

# Accessing single elements
summary(lm.fit)$r.sq #R square value
summary(lm.fit)$sigma #RSE value

# Computation of Variance Inflation Factors
library(car)
vif(lm.fit)

# Regression using all variables but one
lm.fit1 <- lm(medv~.-age, data = Boston)
summary(lm.fit1)

# alternatively, update() can be used
lm.fit1 <- update(lm.fit, ~.-age)

# Interaction terms

# x1:x2 is interaction term between x1 and x2 in R
# x1*x2 simultaneously includes x1, x2 and x1*x2

summary(lm(medv~lstat*age, data = Boston))

# Non-linear transformations of the predictors

# lm() function also accomodate non-linear transformations
# we use I(X^2) function

lm.fit2 <- lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

# anova() function quantify the extent  to which the quadratic function
# is superior to the linear fit

lm.fit<- lm(medv~lstat)
anova(lm.fit, lm.fit2)

par(mfrow = c(2,2))
plot(lm.fit2)

# polynomial fitting can be done directly using
lm.fit5 <- lm(medv~poly(lstat, 5))
summary(lm.fit5)

# log transformation
summary(lm(medv~log(rm), data = Boston))

# Quanlitative Predictors
fix(Carseats)
names(Carseats)

# this has qualitative predictor such as Shelveloc
# R generates dummy variables automatically
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)

# contrast() returns the coding that R uses for the dummy variable
attach(Carseats)
contrasts(ShelveLoc)

