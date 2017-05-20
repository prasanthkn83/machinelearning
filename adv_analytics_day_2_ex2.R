auto <- read.csv("Cars_Retail_Price.csv")
dim(auto)
names(auto)
summary(auto)
str(auto)

auto <- within(auto, {
  Cylinder <- factor(Cylinder)
  Liter <- factor(Liter)
  Doors <- factor(Doors)
  Cruise <- factor(Cruise)
  Sound <- factor(Sound)
  Leather <- factor(Leather)
})

a1 <- lm(Price ~ Mileage, data=auto)
a1
summary(a1)
#bad r2 value, plots

library(lattice)

xyplot(Price ~ Mileage, data = auto)
xyplot(Price ~ Mileage | Model, data = auto)

a2 <- lm(Price ~ Model + Mileage, data=auto) # Mileage across models
a2
summary(a2)
#one slope(Mileage), different intercepts
# Intercept in summary is the intercept for the first value

a3 <- lm(Price ~ Model/Mileage - 1, data=auto) # Mileage within each model
a3
summary(a3)
#different slope, different intercepts for each combination: Check the mileage in summary

anova(a2,a3)
#p value is small. significant improevment over eariler model

# John Fox: Applie Regression Analysis
# ISLR

library(car)
residualPlots(a3)

avPlots(a3, id.n=2)

qqPlot(a3, id.n=3)

outlierTest(a3)

influenceIndexPlot(a3, id.n=3)

ncvTest(a3)
vif(a3)

newauto <- auto[200:300,]
predPrice <- predict(a3, newdata=newauto)
predPrice
newauto$Price

mean(abs(predPrice - newauto$Price)/newauto$Price) # MAPE