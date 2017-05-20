### rpart model for regession tree - automobile insurance claims

#Load Data
claims <- read.csv("Claims_Data_Sample_Share.csv", as.is=TRUE)

str(claims)
table(claims$Cubic.Capacity)

claims <- within(claims, {
  Geography <- factor(Geography)
  Mfr.Model <- factor(Mfr.Model)
  Cubic.Capacity <- cut(Cubic.Capacity, breaks=c(999, 1251, 2001, 2401))
})

summary(claims)
table(claims$Mfr.Model) ##to check low frequency car models, looks ok
sam <- sample.int(nrow(claims), size=0.7 * nrow(claims))

r10 <- rpart(Claim ~ ., data=claims, subset=sam)
r10
printcp(r10)
##cross validation erro dropping, grow the tree further
r10 <- rpart(Claim ~ ., data=claims, subset=sam,
             control=rpart.control(cp=0.0001))
r10
printcp(r10)
plotcp(r10)

merr <- which.min(r10$cptable[,'xerror'])
mCP <- r10$cptable[merr, 'CP']
r11 <- prune(r10, cp=mCP)
r11
prp(r11) 

###Random Forest

iris
plot(iris[,-5],col=iris$Species)
library(randomForest)
rf1 <- randomForest(Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,data=iris, ntree=10000)
rf1
##OOB - Out of Bag, for each tree go with a part of data, whatever the hold out data can be used
## to test the tree.
summary(rf1)
varImpPlot(rf1)
# vairable importance identification
plot(rf1)

#prediction can be applied to the above data

?fgl
fgl <- MASS::fgl
head(fgl)
rf2 <- randomForest(type ~ .,data=fgl, do.trace=TRUE, ntree=1000)
#class.error to identify he fitmet
rf2
summary(rf2)
varImpPlot(rf2)
plot(rf2)

## skar
rf3 <- randomForest(status ~ Plan.Type + NEW_CELL_IND + use1 + prom, data=skar)
rf3
summary(rf3)
varImpPlot(rf3)
plot(rf3)
skar <- within(skar, {
  minuse4[is.na(minuse4)] <- 0 
  minuse3[is.na(minuse3)] <- 0
  minuse2[is.na(minuse2)] <- 0
  prom4[is.na(prom4)] <- 0
  prom3[is.na(prom3)] <- 0
  prom2[is.na(prom2)] <- 0
})
rf3 <- randomForest(status ~ Plan.Type + NEW_CELL_IND + minuse4 + minuse3 + minuse2 + prom4 + prom3 + prom2, data=skar)
rf3
plot(rf3)

#claims
rf4 <- randomForest(Claim ~., data=claims)
rf4
pClaim <- predict(rf4)
plot(x=claims$Claim, y=pClaim)
cor(x=claims$Claim, y=pClaim)^2

lm4 <- lm(Claim ~., data=claims)
summary(lm4)

    fgl.res <- tuneRF(fgl[,10], fgl[,10], stepFactor=1.5)

rf2 <- randomForest(type ~ .,data=fgl, mtry=4, ntree=500)
rf2

?varUsed