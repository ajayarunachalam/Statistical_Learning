# Best subset selection method

library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# omit the rows with missing values
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# regsubset() is used for implementing best subset selection based on RSS values
library(leaps)
regfit.full = regsubsets(Salary~., Hitters)
summary(regfit.full)

# regsubsets() only reports results up to best eight variable model
# so use nvmax

regfit.full = regsubsets(Salary~., data = Hitters, nvmax = 19)
reg.summary = summary(regfit.full)
names(reg.summary)

reg.summary$ rsq

# Plotting RSS, adjusted Rsquare, Cp and BIC to decide which model to choose
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type = "l")

# plot red dot to identify model with largest adjusted Rsq
which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)],
       col = "red", cex=2, pch=20)

plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type = 'l')
which.min(reg.summary$cp)
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)],
       col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = 'l')
which.min(reg.summary$bic)
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)],
       col = "red", cex = 2, pch = 20)

# regsubsets() has built-in plot 
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

# we can use coef() to see the coefficient etimates associated with a model
coef(regfit.full,6)

# Forward and Backward Stepwise Selection

# regsubsets() with argument method
regfit.fwd = regsubsets(Salary~., data = Hitters, nvmax = 19,
                        method = "forward")
summary(regfit.fwd)

regfit.bwd = regsubsets(Salary~., data = Hitters, nvmax = 19,
                        method = "backward")
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# Choosing among models using the validation set approach and cross-validation
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test = (!train)

regfit.best = regsubsets(Salary~., data = Hitters[train,], nvmax = 19)

# compute the validation set error for the best model of each model size
test.mat = model.matrix(Salary~., data = Hitters[test,])

val.errors = rep(NA, 19)
for (i in 1:19){
  coefi = coef(regfit.best, id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
  }
val.errors

which.min(val.errors)
coef(regfit.best, 10)

# Let's make a function for prediction
predict.regsubsets = function(object, newdata, id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best = regsubsets(Salary~., data=Hitters, nvmax = 19)
coef(regfit.best, 10)

# k-fold cross validation
k=10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k){
  best.fit = regsubsets(Salary~., data = Hitters[folds!=j,],
                        nvmax = 19)
  for (i in 1:19){
    pred = predict(best.fit, Hitters[folds==j, ], id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors, type = 'b')

reg.best = regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best, 11)
