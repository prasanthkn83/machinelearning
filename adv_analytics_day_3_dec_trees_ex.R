### rpart model for regession tree - automobile insurance claims

#Load Data
ins <- read.csv("Claims_Data_Sample_Share.csv", as.is=TRUE)

#Data Exploration
dim(ins)
names(ins)
summary(ins)
str(ins)

ins <- within(ins, {
  Geography <- factor(Geography)
  Mfr.Model <- factor(Mfr.Model)
  Cubic.Capacity <- factor(Cubic.Capacity)
})

ins$Year <- factor(ins$Year)


summary(ins)

# Numerical vs Numerical
names(ins)[sapply(ins, is.numeric)]

plot(Claim ~ IDV, ins)
plot(Claim ~ Premium, ins)
paste(names(ins)[sapply(ins, is.numeric)], collapse="','")
cor(ins[,c('IDV','Claim','Premium')])

# Numerical vs Categorical
names(ins)[sapply(ins, is.factor)]

boxplot(Claim ~ Year, ins)
boxplot(Claim ~ Geography, ins) ##not significant
boxplot(Claim ~ Cubic.Capacity, ins)
boxplot(Claim ~ Mfr.Model, ins)

# Model
library(rpart)

sam <- sample.int(n=nrow(ins), size =0.7*nrow(ins))

##Model
##Premium ~ IDV + Mfr.Model+ Year + Cubic.Capacity
r8 <- rpart(Claim ~ IDV, data=ins, subset=sam)
r8
prp(r8)
r8 <- rpart(Claim ~ IDV , data=ins, subset=sam, 
            control=rpart.control(cp=0.0001))
r8
prp(r8)
printcp(r8)
plotcp(r8)

merr <- which.min(r8$cptable[,'xerror'])
mCP <- r8$cptable[merr, 'CP']
r9 <- prune(r8, cp=mCP)
r9
prp(r9) 

#Train
trainPremium <- predict(r9, newdata=ins[sam,])

summary(data.frame(trainPremium, ins[sam,'Premium'], 100 * abs(trainPremium - ins[sam,'Premium'])/ins[sam,'Premium']))

mean(abs(trainPremium - ins[sam,'Premium'])/ins[sam,'Premium']) # MAPE


#Test
predPremium <- predict(r9, newdata=ins[-sam,])

summary(data.frame(predPremium, ins[-sam,'Premium'], 100 * abs(predPremium - ins[-sam,'Premium'])/ins[-sam,'Premium']))

mean(abs(predPremium - ins[-sam,'Premium'])/ins[-sam,'Premium']) # MAPE

##Linear model comparison
m1 <- lm(Premium ~ IDV, data=ins, subset=sam)
m1
summary(m1)

trainlmPremium <- predict(m1, newdata=ins[sam,])

summary(data.frame(trainlmPremium, ins[sam,'Premium'], 100 * abs(trainlmPremium - ins[sam,'Premium'])/ins[sam,'Premium']))

mean(abs(trainlmPremium - ins[sam,'Premium'])/ins[sam,'Premium']) # MAPE

predlmPremium <- predict(r9, newdata=ins[-sam,])

summary(data.frame(predlmPremium, ins[-sam,'Premium'], 100 * abs(predlmPremium - ins[-sam,'Premium'])/ins[-sam,'Premium']))

mean(abs(predlmPremium - ins[-sam,'Premium'])/ins[-sam,'Premium']) # MAPE
