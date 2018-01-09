library(ISLR)
attach(Wage)

# Polynomial Regression and Step functions

fit = lm(wage~poly(age,4), data=Wage)
coef(summary(fit))

fit2 = lm(wage~poly(age,4,raw=T), data=Wage)
coef(summary(fit2))

fit2a = lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
coef(fit2a)

fit2b = lm(wage~cbind(age, age^2, age^3, age^4), data=Wage)
coef(fit2b)

# make a grid of values of age, where we want to predict
agelims  = range(age)
age.grid = seq(from = agelims[1], to=agelims[2])

preds = predict(fit, newdata = list(age=age.grid), se=TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit,
                 preds$fit - 2*preds$se.fit)

# plot the data and add the fit from the degree-4 polynomial

par(mfrow = c(1,2), mar = c(4.5, 4.5,1,1), oma=c(0,0,4,0))
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Degree-4 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty = 3)

preds2 = predict(fit2, newdata = list(age=age.grid), se=TRUE)
max(abs(preds$fit - preds2$fit))

# Use hypothesis test to find which model to use
# anova() performs analysis of variance in order to test null hypothesis

fit.1 = lm(wage~age, data=Wage)
fit.2 = lm(wage~poly(age,2), data=Wage)
fit.3 = lm(wage~poly(age,3), data=Wage)
fit.4 = lm(wage~poly(age,4), data=Wage)
fit.5 = lm(wage~poly(age,5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# quadratic or cubic model is significant
coef(summary(fit.5))

# if we have another terms in model as well
fit.1 = lm(wage~education+age, data=Wage)
fit.2 = lm(wage~education+poly(age,2), data=Wage)
fit.3 = lm(wage~education+poly(age,3), data = Wage)
anova(fit.1, fit.2, fit.3)

# Predict whether an individual earns more than 250000 dollars per year

fit = glm(I(wage>250)~poly(age,4), data = Wage, family = binomial)
preds = predict(fit, newdata = list(age=age.grid), se=T)

pfit = exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2*preds$se.fit,
                       preds$fit - 2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))

preds = predict(fit, newdata = list(age=age.grid), type="response", se=T)

#rug plot
plot(age, I(wage>250), xlim=agelims, type="n", ylim=c(0,0.2))
points(jitter(age), I((wage>250)/5), cex=0.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

# fit a step function
table(cut(age,4))
fit = lm(wage~cut(age,4), data=Wage)
coef(summary(fit))

# Splines
library(splines)
fit = lm(wage~bs(age, knots = c(25,40,60)), data = Wage)
pred = predict(fit, newdata = list(age = age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit+2*pred$se, lty="dashed")
lines(age.grid, pred$fit-2*pred$se, lty="dashed")

# splines with knots at uniform quantiles of data
dim(bs(age, knots = c(25, 40,60)))
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")

# fitting a natural spline
# 4 degrees of freedom

fit2 = lm(wage~ns(age, df=4), data = Wage)
pred2 = predict(fit2, newdata = list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col="red", lwd=2)

# smoothing spline
plot(age, wage, xlim = agelims, cex = 0.5, col="darkgrey")
title("Smoothing Splines")
fit = smooth.spline(age, wage, df=16)
fit2 = smooth.spline(age, wage, cv=TRUE)
fit2$df.residual

lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"), col=c("red", "blue"),lty=1, lwd=2, cex=0.8)

# local regression
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Local regression")
fit = loess(wage~age, span=0.2, data=Wage)
fit2 = loess(wage~age, span=0.5, data=Wage)
lines(age.grid, predict(fit, data.frame(age=age.grid)), col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)), col="blue", lwd=2)
legend("topright", legend = c("Span=0.2", "Span=0.5"), col=c("red", "blue"),
       lty=1, lwd=2, cex=0.8)

# GAM
gam1 = lm(wage~ns(year, 4)+ns(age,5)+education, data=Wage)

library(gam)
gam.m3 = gam(wage~s(year,4)+s(age,5)+education, data = Wage)

par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")

plot.gam(gam1, se=TRUE, col="red")

gam.m1 = gam(wage~s(age,5)+education, data=Wage)
gam.m2 = gam(wage~year+s(age,5)+education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test="F")

summary(gam.m3)

preds=predict(gam.m2, newdata = Wage)
# local regression
gam.lo = gam(wage~s(year, df=4)+lo(age, span=0.7)+education, data=Wage)
plot.gam(gam.lo, se=TRUE, col="green")

gam.lo.i = gam(wage~lo(year, age, span=0.5)+education,
               data=Wage)

library(akima)
plot(gam.lo.i)

gam.lr = gam(I(wage>250)~year+s(age, df=5)+education, family=binomial, data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green")

table(education, I(wage>250))

gam.lr.s = gam(I(wage>250)~year+s(age, df=5)+education, family = binomial,
               data=Wage, subset = (education!="1. < HS Grad"))
plot(gam.lr.s, se=T, col="green")
