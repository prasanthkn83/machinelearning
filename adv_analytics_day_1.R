##Day 1
x <- 20

cars
class(cars)
?cars
plot(cars)
m1 <- lm(dist ~ speed, data=cars)
abline(m1, col='red')
with(cars, cor(speed,dist)) # find the correlation of variables
#Linear Model - Y, the dependent is continous(Real value: not discrete or categorical)
#X: may be continous or discrete

## Data Simulation, Random variables
df <- data.frame(y = rnorm(n=30, mean=10, sd=2), 
                 x = rnorm(n=30, mean=10, sd=2))
plot( y ~ x, df, col='blue', pch=21)
with(df, cor(x,y))

## Data Simulation, Random variables scenario 2
tmp <- rnorm(n=30, mean=10, sd=2)
df <- data.frame(y = tmp + rnorm(n=30, mean=0, sd=0.2), 
                 x = tmp + rnorm(n=30, mean=0, sd=0.3))
plot( y ~ x, df, col='blue', pch=21)
with(df, cor(x,y))

#Different methods of invoking data frame values
cor(cars$dist, cars$speed) ## to be precise, this $ is a list operator
cor(cars[,1],cars[,2]) ## equivalent
cor(cars[,'dist'],cars[,'speed']) ## equivalent
cor(cars[[1]],cars[[2]]) ## equivalent
cor(cars) # corealtion matrix

m1
summary(m1)
# Residuals are the distance from the actual value
# Analyze the residuals summary value; mean will be close zero, normally distrbuted in ideal scenarios
# Coefficients values : Estimate 
# Std. Error - slope could be between Estimate + or - Std.Error
# t value - how far estimate is away from zero, t distrbution value
# P value - the probability of t value
# We can conclude that this is a significant coefficient based on  P value
# Residual standard error - distance between residuals
# R-squared is cor between Y & Y-hat, higher the R-squared better the model
# Adjusted R-squared - Rsquared can be improved by adding variables. Adjusted R-squared will penalize insignifact variables.coed
# F-statistic - ratio of variances (a) our model (b) NULL model(mean)/ANOVA
# P value - probabilty of F-statistic value

names(m1)
m1$coefficients
m1$residuals
m1$fitted.values
m1$coefficients[2]*cars$speed+m1$coefficients[1] #this will return Y-hat/fitted values

# Residual Analysis
plot(m1)
# Residual vs Fitted - not uniformly distrbuted around zeroes
# Normal Q-Q - this should follow a straight line. slight variations to the tail is fine.
# Scale-Location - 
# Residuals vs Leverage - identify the leverage(outlier) points that influence the regression negatively

# Predictions

# Data frame with sample speed values for prediction
speednew <- data.frame(speed=c(18, 25, 40))
predict(object=m1, newdata=speednew)
# Comparing predicted values vs model values
m1$coefficients[2]*speednew$speed+m1$coefficients[1]

#second example
mtcars
#returns the list of data sets available in R
data()

?mtcars
names(mtcars)
str(mtcars)
summary(mtcars)
# is the data of correct format/type
# within will intialize all the values in a data frame using a single call
mtcars <- within(mtcars, {
  cyl <- factor(cyl)
  vs <- factor(vs, labels=c('P','V'), levels=c(0,1))
  am <- factor(am, labels=c('A','M'), levels=c(0,1))
  gear <- factor(gear)
  carb <- factor(carb)
  })
str(mtcars)

# Explore
summary(mtcars)
# table(mtcars$cyl) will provide the summary of a single variable

# check one vairable at a time
hist(mtcars$mpg)
# plot area using par function
par(mfrow=c(2,2))
hist(mtcars$disp)
hist(mtcars$hp)
hist(mtcars$drat)
hist(mtcars$wt)

# reset plot area
par(mfrow=c(1,1))
plot(mpg ~ wt, mtcars)
model1 <- lm(mpg ~ wt, data=mtcars)
model1
abline(model1)
summary(model1)
plot(model1)

## univariate discrete
table(mtcars$cyl)
table(mtcars$carb)


## bi-variate exploration relatiohsip of all the variables with mpg, the variable of interest
## continous var
## correlation
cor(mtcars[,c('mpg', 'disp', 'wt', 'drat', 'hp', 'qsec')])
# sapply applies a function to all specified elements
sapply(mtcars,is.numeric)
# get the names of numeric columns in mtcars
names(mtcars)[sapply(mtcars,is.numeric)]

# apply the sapply logic to select columns
cor(mtcars[,names(mtcars)[sapply(mtcars,is.numeric)]])
## scatterplot
plot(mtcars[,names(mtcars)[sapply(mtcars,is.numeric)]])
plot(mpg ~ disp, mtcars)

m2 <- lm(mpg ~ disp, mtcars)
m2
summary(m2)
plot(mpg ~ disp, mtcars)
abline(m2, col = "red")
# data not normally distrbuted against the linear model, this needs to addressed

### discrete / factor variables
names(mtcars)[sapply(mtcars,is.factor)]
# use box plot to understand relation bt continous & factor
boxplot(mtcars$mpg)
boxplot(mpg ~ cyl, mtcars)
plot(mpg ~ disp, mtcars, col=mtcars$cyl, pch=16)
abline(m2, col="red")

m3 <- lm(mpg ~ disp + cyl, mtcars)
m3
summary(m3)
# discrete variable added as seperate coefficients, mpgs reduced if 6 & 8 cyclinder cars
# abline cannot be done with this model since there are more coefficient values
m3out <- data.frame(mpg=m3$fitted, disp=mtcars$disp)
m3out <- m3out[order(m3out$disp),]
lines(mpg ~ disp, m3out, col='purple')

#seperate intercept & slope for each of the cylinders
m4 <- lm(mpg ~ cyl/disp - 1, mtcars)
m4
summary(m4)
m4out <- data.frame(mpg=m4$fitted, disp=mtcars$disp)
m4out <- m4out[order(m4out$disp),]
lines(mpg ~ disp, m4out, col='darkgreen')

## scenario where we had identified the curvature in model m2 with displacement alone
m5 <- lm(mpg ~ disp+ I(disp^2), mtcars)
m5
summary(m5)
m5out <- data.frame(mpg=m5$fitted, disp=mtcars$disp)
m5out <- m5out[order(m5out$disp),]
lines(mpg ~ disp, m5out, col='darkgrey')
# curved model

m6 <- lm(mpg ~ disp+ I(disp^2) + I(disp^3), mtcars)
m6
summary(m6)
m6out <- data.frame(mpg=m6$fitted, disp=mtcars$disp)
m6out <- m6out[order(m6out$disp),]
lines(mpg ~ disp, m6out, col='brown')
# cubic model
# dont make the model unnecessarily complicated. Deriving a cylinder based modeling was more appropriate

# two factor interaction - example will be discussed later

## Why should we not add correlated variables in a model
## Std. Error(varinace)  will increase when correlated variables are used in model
## vif(m1) can be used to find the variance
mm1 <- lm(mpg ~ disp, mtcars)
mm2 <- lm(mpg ~ disp + wt, mtcars)
mm3 <- lm(mpg ~ wt, mtcars)

summary(mm1)
summary(mm2)
summary(mm3)

library(car)
vif(mm1)
vif(mm2)
## If we want to add , use principal components

