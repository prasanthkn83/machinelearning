# library(XLConnect) #to load Excel data

##OTC Case Study

#Load data
otc <- read.csv("otcdata.csv")
dim(otc)
names(otc)
summary(otc)
str(otc)
#data correction for New Vol Sales
otc$NewVolSales <- as.numeric(gsub(",","",otc$NewVolSales))
# library(plyr)
# otc <- rename(otc,c("StockOut."="StockOut", "X..of.stores.with.InStore.Prom"="InStore.Prom"))
# names(otc) <-c()
paste(names(otc), collapse="','") #copy the values to the below vector
#Correct the names of headers
names(otc) <-c('NewVolSales','Price','Radio','StockOut','InStore','Discount','Facebook','Twitter','WebCamp','Inserts','Online','Website','Newspaper','TV')

otc.old <- otc
summary(otc.old)

#Explore
str(otc)
summary(otc)
#Observed zero values in discount & radio
table(otc$Discount) # 61 Zero values
table(otc$Radio) # 8 Zero values
# change value Inserts/Online value to factors
otc <- within(otc, {
  Inserts <- factor(Inserts, labels=c('N','Y'), levels=c(0,1))
  Online <- factor(Online, labels=c('N','Y'), levels=c(0,1))
})
str(otc)
summary(otc)
# Multiply discounts with 100
otc$Discount <- otc$Discount * 100
# Remove the reduntant information columns
otc <- otc[,c('NewVolSales', 'Price', 'Radio', 'StockOut', 'InStore', 'Discount', 'Inserts','Online','TV')]

# Univariate - Continous
hist(otc$NewVolSales)
hist(otc$Price)
hist(otc$Radio)
hist(otc$StockOut)
hist(otc$InStore)
hist(otc$Discount)
hist(otc$TV)

# Univariate - Discrete

table(otc$Online)
table(otc$Inserts)
#Majority No values observed for both of the above

# Bivariate Analysis

cor(otc[,c('NewVolSales', 'Price', 'Radio', 'StockOut', 'InStore', 'Discount', 'TV')])
# 0.7 price; 0.4 for instore & discount; 0.2 for radio, tv; Instore & discount are highly correalted
plot(otc[,c('NewVolSales', 'Price', 'InStore', 'Discount', 'TV', 'Radio')])
# NewVolSales vs Price - Few outliers in the start of chart
# New Vol Sales vs Instore - No consistent trends
# New Vol Sales vs Discount - Discount Zero for many values; Strong Correlation between Discount, Instore
# Discount can be converted to categorical values for better model fitment
# Stockout high sales volume dropped


boxplot(NewVolSales ~ Online, otc)
# Sales reduced while online promotion happened, budget impact due to additional cost
# Interaction events
boxplot(TV ~ Online, otc)
# Comparing TV GRPS with Online to see impact, TV GRP budget was not reduced
boxplot(InStore ~ Online, otc)
# Comparing InStore with Online to see impact, InStore budget was not reduced
boxplot(Radio ~ Online, otc)
# Comparing Radio with Online to see impact, Radio budget was reduced
# We need to check with customer regarding this interaction

# 70% TV Value computation
otc$TV70 <- otc$TV
otc$TV70[2:104] <- otc$TV70[2:104] * 0.7 + otc$TV70[1:103] * 0.3
plot(NewVolSales ~ TV70, otc)
# No major variation observed compared to TV

#################Model Creation#############################

otcm1 <- lm(NewVolSales ~ Price + InStore + Discount + Online + TV70 +  Radio + StockOut, otc)
otcm1
summary(otcm1)
# Insignificant negative values observed - Online(can be removed), Radio(need to investigate)

#Remove online
otcm2 <- lm(NewVolSales ~ Price + StockOut + InStore + Discount + TV70 +  Radio , otc)
otcm2
summary(otcm2)
# Good impact on Discount, Radio still has a negative coefficient

#Discount classifier
otc$Disc <- cut(otc$Discount, breaks=c(-1,1,7,100))
table(otc$Disc)

boxplot(NewVolSales ~ Disc, otc)
# Notice the sales volume impact on discount type

otcm3 <- lm(NewVolSales ~ Price + StockOut + InStore + Disc + TV70 +  Radio , otc)
otcm3
summary(otcm3)
#Instore value became more significant, Discount no signifcant impact

otcm4 <- lm(NewVolSales ~ Price + StockOut + InStore + TV70 +  Radio , otc)
otcm4
summary(otcm4)
#Radio not a signifcant contributor

plot(otcm4)
#need to explore 21, 79, 57, 91 in details
otc[c(21, 79, 57, 91),]

otc.new <- otc[c(-21, -79, -57, -91),]
otcm5 <- update(otcm4, subset = !rownames(otc) %in% c('15','21', '79', '57', '91'))
otcm5
summary(otcm5)

#####################Model Diagnostics#################################
library(car)
residualPlots(otcm5)
# No patterns, No problems
# All P should be non-significant
# Model ok if residual have mean = 0 and vraince = 1
# Tukey test null hypothesis : model is additive

avPlots(otcm5, id.n=2) ##added variables plot
## id.n = 2 indetifies the 2 most influential observations

qqPlot(otcm5, id.n=3)

outlierTest(otcm5)

influenceIndexPlot(otcm5, id.n=3)

ncvTest(otcm5)
vif(otcm5)

newotc <- otc[13:21,]
predVol <- predict(otcm5, newdata=newotc)
predVol
newotc$NewVolSales

## How good is the fit ? MAPE : Mean absolute percent error

##error actual - pred
##mean(abs(error/actual))

mean(abs(predVol - newotc$NewVolSales)/newotc$NewVolSales) # MAPE

