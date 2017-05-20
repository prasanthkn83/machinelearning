## Time Series

LakeHuron
class(LakeHuron)

plot(LakeHuron)

## EWA Exponentially weighted moving average: beta=FALSE & gamma=FALSE
m1 <- HoltWinters(LakeHuron, beta=FALSE, gamma=FALSE)
m1
predict(m1, n.ahead=2) ##forecast same for year beyond 1. EWA is a good predictor only for next year.

y <- rnorm(36, mean=10, sd=2)
plot(y)
y <- ts(data=y, start=2010, frequency=12) ##timeseries object wtih monthly observations & annual seasonality
plot(y)

ts(data=y, start=1, freq=7) ##Weekly seasonal series, with daily observations
plot(ts(data=y, start=1, freq=7))
## For mdeling, we should be using data upto last season(leave the last season out); 
## build the model and predict for theleft out season
## that gives us data to compare the performance of our model
## for y, Build model on 2010-2011 data & predict & compare 2012 data
## How to extract a part of the timeseries?
ytr <- y[1:24] #coont use
ytr <- window(y, start=c(2010,1), end=c(2011,12))
ytr
m2 <- HoltWinters(ytr, beta=FALSE, gamma=FALSE)
m2
p2 <- predict(m2, n.ahead=12)
##Error in prediction MAPE
actual <- window(y, start=c(2012, 1))
MAPE <- mean(abs((actual-p2)/actual))*100
plot(y)
lines(p2, col='red')

library(forecast)
plot(gas)
str(gas)
plot(decompose(gas)) ##additive -- not good for gas since the seasonality is not of same magnitude for different levels of the series
plot(decompose(gas, type='multiplicative'))


beer.df <- read.csv('beer.csv')
beer <- ts(beer.df, start=c(1956,1), frequency=12)
plot(beer)
plot(decompose(beer))
plot(decompose(beer, type='multiplicative'))
plot(stl(beer, s.window='periodic'))

##HoltWinters with trend and seasonality
gw <- window(gas, end=c(1992,12))
m3 <- HoltWinters(gw, seasonal='multiplicative')
m3
p3 <- forecast(m3, h=32)
plot(p3)
lines(gas)

actual <- window(gas, start=c(1993,1))
MAPE <- mean(abs((actual - p3$mean)/actual)) * 100
MAPE
accuracy(f=p3, x=actual)

##HoltWinters beer
bw <- window(beer, end=c(1992,12))
m4 <- HoltWinters(bw, seasonal='additive')
m4
p4 <- forecast(m4, h=32)
plot(p4)
lines(beer)

actual <- window(beer, start=c(1993,1))
MAPE1 <- mean(abs((actual - p4$mean)/actual)) * 100
MAPE1
accuracy(f=p4, x=actual)

m5 <- HoltWinters(bw, seasonal='multiplicative')
m5
p5 <- forecast(m5, h=32)
plot(p5)
lines(beer)

actual <- window(beer, start=c(1993,1))
MAPE2 <- mean(abs((actual - p5$mean)/actual)) * 100
MAPE2
accuracy(f=p5, x=actual)

#Trying with a smaller dataset
beer1 <- window(beer, start=c(1988,1))
plot(decompose(beer1))
#rerun the above model code

bw<- window(beer1, end=c(1992,12))
et1 <- ets(bw, model='AAA', damped=TRUE)
ef1 <- forecast(et1, h=32)
plot(ef1)
lines(beer1)

et1 <- ets(bw, model='MMM', damped=TRUE)
ef1 <- forecast(et1, h=32)
plot(ef1)
lines(beer1)

et1 <- ets(bw, model='ZZZ', damped=TRUE)
ef1 <- forecast(et1, h=32)
plot(ef1)
lines(beer1)

plot(gas)
gas1 <- window(gas, start=c(1988,1))
plot(decompose(gas1))
gw <- window(gas1, end=c(1992,12))
#rerun the above model code



## stationary time series
library(MASS)
boxcox(lm(beer ~ I(1:length(beer))), lambda=seq(-2,2,by=1/10))
par(mfrow=c(2,1))
plot(beer)
plot(sqrt(beer))

plot(diff(beer, lag=12))
db <- diff(beer, lag=12) + 100
boxcox(lm(db ~ I(1:length(db))), lambda=seq(-2,5,by=1/10))
par(mfrow=c(1,1))
plot(sqrt(diff(beer, lag=12) + 100))

## Box-Jenkins (ARIMA) Models
## read this : https://www.otexts.org/fpp/8/9
par(mfrow=c(2,1))
Acf(beer) ## seasonlatiy effect .. do not use this
Pacf(beer)
Acf(diff(beer, lag=1)) ## signifcant at lag 1 & 2, 13
Pacf(diff(beer, lag=1)) ## signifcant at lag 1 & 2
Acf(diff(beer, lag=12)) ## signifcant at lag 1, 2 & 3 .. not really because 1 & 2 are barely significant
Pacf(diff(beer, lag=12)) ## signifcant at lag 1, 2 & 3 .. not really because 1 & 2 are barely significant

Acf(diff(sqrt(bw), lag=1)) ## signifcant at lag 1 & 2, 13
Pacf(diff(sqrt(bw), lag=1)) ## signifcant at lag 1 & 2


a1 <- Arima(bw, order=c(2, 1, 2), seasonal=c(0, 0, 1), lambda=0.5)
af1 <- forecast(a1, h=32)
par(mfrow=c(1,1))
plot(af1)
lines(beer1)

a1 <- Arima(bw, order=c(0, 0, 0), seasonal=c(0, 1, 1))
af1 <- forecast(a1, h=32)
par(mfrow=c(1,1))
plot(af1)
lines(beer1)
accuracy(af1, x=window(beer1, start=c(1993,1)))
## fitting ARIMA model is harder
## ETS a little eariler and more popular thse days

#ARIMA for gas
par(mfrow=c(2,1))
gas1 <- window(gas, start=c(1988,1))
gw <- window(gas1, end=c(1992,12))
Acf(gas1) ## seasonal effect, exponential component
Pacf(gas1)



Acf(diff(gw, 12))
Pacf(diff(gw, 12))
am2 <- Arima(gw, order=c(1, 0, 0), seasonal=c(0, 1, 0))
am2 <- Arima(gw, order=c(2, 0, 0), seasonal=c(0, 1, 0))
am2 <- Arima(gw, order=c(1, 1, 0), seasonal=c(0, 0, 1)) ##failes because we have not captured seasonality

af2 <- forecast(am2, h=32)
par(mfrow=c(1,1))
plot(af2)
lines(gas1)
accuracy(af2, x=window(gas1, start=c(1993,1)))

#LakeHuron
plot(LakeHuron)
boxcox(lm(LakeHuron ~ I(1:length(LakeHuron))), lambda=seq(-2, 10, by=0.1))
tseries::adf.test(diff(LakeHuron,1))
par(mfrow=c(2,1))
Acf(diff(LakeHuron,1))
Pacf(diff(LakeHuron,1))

am3 <- Arima(LakeHuron, order=c(0,1,0))
accuracy(am3)
am4 <- Arima(LakeHuron, order=c(1,1,0))
accuracy(am4)
am5 <- Arima(LakeHuron, order=c(0,1,1))
accuracy(am5)

#find the best arima model
aa1 <- auto.arima(bw, trace=TRUE)

