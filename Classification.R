# Classification using Logistic Regression, LDA, QDA and KNN

library(ISLR)
# We will use Smarket data

names(Smarket)
dim(Smarket)

summary(Smarket)

# Correlation between the predictors
cor(Smarket[,-9]) #Because direction variable is qualitative

# Correlation between lag variables and today's return is almost zero
# ONly Year and volume have substantial correlation

attach(Smarket)
plot(Volume)

# Logistic Regression
# Predict direction using Lag1 to Lag5 and volume
# glm() fits genralized linear models that includes Logistic regression
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Smarket, family = binomial)
summary(glm.fits)

# coefficients of fitted model
coef(glm.fits)
summary(glm.fits)$coef

# predict() can be used to predict the probability that te market will go up, 
# given the value of predictors
glm.probs = predict(glm.fits, type = "response")

# first ten probabilities of market going up
glm.probs[1:10]

# Check the dummy variable
contrasts(Direction)

# Make the class labels
glm.pred = rep("Down", 1250) #Create vector of 1250 "down" elements
glm.pred[glm.probs > 0.5] = "Up" #convert to "up" for prob > 0.5

# Confusion matrix of classification
table(glm.pred, Direction)
(507+145)/1250
mean(glm.pred == Direction)

# Diagonal elements of confusion matrix are correct predictions
# Off-diagonal elements are incorrect predictions
# Above error is training error, because it shows error on data that was used to train the model

# lets use train and test data separately now
train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]

dim(Smarket.2005)
Direction.2005 = Direction[!train]

# Now fit the Logistic Regression model on this data
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")

# Compute the predictions for 2005
# and compare with actual movement of the market
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005) # test set error rate

# Not a very good test error rate
# So let's remove the predictors that are not significant

glm.fits = glm(Direction~Lag1+Lag2, data=Smarket,
               family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

# accuracy rate when model predicts and increase in the market
106/(106+76)

# Predict the return associated with a particular value
predict(glm.fits, newdata = data.frame(Lag1=c(1.2, 1.5), 
                                       Lag2=c(1.1,-0.8)), type = "response")

# Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)

# predict() function return three elements
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

# Applying a 50% threshold to the posterior probabilities
# allow us to recreate the predictions contained in lda.pred$class

sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] < 0.5)

# posterior prob output corresponds to the prob that the market will decrease
lda.pred$posterior[1:20, 1]
lda.class[1:20]

# we can change the threshold
sum(lda.pred$posterior[,1] > 0.9)

# Quadratic Discriminant Analysis
qda.fit = qda(Direction~Lag1+Lag2, data = Smarket, subset = train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

# K-Nearest Neighbors
# We bind Lag1 and Lag2 to form train and test
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)

(83+43)/252

knn.pred = knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

# An application to Caravan Insurance data
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822

standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

# Split the data into train and test
test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred, test.Y)

knn.pred = knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)

knn.pred = knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)

glm.fits = glm(Purchase~.,data=Caravan, family = binomial, subset = -test)
glm.probs = predict(glm.fits, Caravan[test,], type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs>0.5]="yes"
table(glm.pred, test.Y)
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table(glm.pred, test.Y)
