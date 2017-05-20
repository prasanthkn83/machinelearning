### Day 4 - Decision Trees

iris
paste(names(iris), collapse="','")
plot(iris[,c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')], col=iris$Species)

##Loading rpart library
library(rpart)

# running decision tree with all independant vairables
r1 <- rpart(Species ~ ., iris)
r1
plot(r1)
text(r1)
library('rpart.plot')
prp(r1)

summary(r1)
## CP Table
## Vairable importance
## Node number 1 : Petal.Length & Petal.Width are good splits
## Gini coefficient
## Use the plsit with maximal impurity reduction ie improvement


plot(iris[,c('Petal.Length','Petal.Width')], col=iris$Species)
abline(v=2.45)
abline(h=0.8)
abline(h=1.75)
abline(v=4.75, lty=3)

### Good Bad and run rpart

gb <- read.csv("GOODBAD.csv")

str(gb)
summary(gb)
table(gb$Good.Bad)

gb$y <- factor(gb$Good.Bad, levels=c(1,2), labels=c('Good','Bad'))
str(gb)

sam <- sample.int(nrow(gb), size=0.7 * nrow(gb))
r2 <- rpart(y ~ ., data=gb[,-which(names(gb)=='Good.Bad')], subset=sam)
r2
plot(r2)
text(r2)
prp(r2)
printcp(r2)
plotcp(r2)
## CP Table
## grow tree further by relaxing CP (in some other cases we may need to relax minsplit or minbucket)
r3 <- rpart(y ~ ., data=gb[,-which(names(gb)=='Good.Bad')], subset=sam,
            control=rpart.control(cp=0.001))
printcp(r3)
plotcp(r3)
## Get the CP value which has min xerror (cross-validation error)
merr <- which.min(r3$cptable[,'xerror'])

##CP nsplit rel error  xerror     xstd
##1 0.0440252      0   1.00000 1.00000 0.057345
##2 0.0424528      4   0.81132 1.04245 0.058007
##3 0.0188679      5   0.76887 0.93868 0.056294 //this row
##4 0.0157233      6   0.75000 0.94340 0.056379
##5 0.0141509     10   0.68396 0.98113 0.057033
##6 0.0094340     13   0.64151 1.02358 0.057719
##7 0.0078616     16   0.61321 1.01415 0.057571
##8 0.0047170     21   0.56604 1.02358 0.057719
##9 0.0010000     22   0.56132 1.02830 0.057792

mCP <- r3$cptable[merr, 'CP']

## prune the tree to remove nodes having CP < mCP
r3 <- prune(r3, cp=mCP)
r3
plot(r3)
text(r3)
prp(r3)  
head(predict(r3))

# predictions for the training set
tail(predict(r3, type='class'))
names(r3)

# training confusion matrix
cm <- table(actual=gb[sam, 'y'], pred=predict(r3, type='class'))
cm
prop.table(cm,1)

## test predictions & confusion matrix
cm <- table(actual=gb[-sam, 'y'], pred=predict(r3, newdata=gb[-sam,], type='class'))
cm
prop.table(cm,1)
prp(r3)

levels(gb$Check_Account_Status)
table(gb[,c('y','Check_Account_Status')])

table(gb[gb$Duration < 17 & gb$Check_Account_Status %in% c('A13', 'A14'),c('y','CreditHistory')])

table(gb[gb$Duration >= 17 & gb$Check_Account_Status %in% c('A13', 'A14'),c('y','SavingsAcc')])

## altered losss matrx
loss <- matrix(c(0,1,2,0), ncol=2)
r4 <- rpart(y ~ ., data=gb[,-which(names(gb)=='Good.Bad')], subset=sam,
            control=rpart.control(cp=0.001, minsplit=15), parm= list(prior=c(0.8, 0.2), loss=loss))

#..rest is same code as above. This reduces the false positive rate since we sa that the loss due to FP is twice as much as the loss due to FN
prp(r4)
printcp(r4)
merr <- which.min(r4$cptable[-1,'xerror'])
mCP <- r4$cptable[merr, 'CP']
r4 <- prune(r4, cp=mCP)
r4
prp(r4)  
head(predict(r4))

## rpart model for skar

skar <- read.csv("skar_sample_class.csv", as.is=TRUE)

str(skar)

#plan type - categorical , so should be changed to factor..

#promo.. monthly data.. opted for promotion offer in a month

# service start date... character to be converted to date

#.. CALC AGE..  at start of the service

skar <- within (skar, {
  Plan.Type <- factor(Plan.Type)
  svc_start_dt <- as.Date(svc_start_dt, format ='%Y-%m-%d')
  svc_end_dt <- as.Date(svc_end_dt, format = '%Y-%m-%d')
  BIRTH_DT <- as.Date(as.character(BIRTH_DT), format ='%Y%m%d')
  NEW_CELL_IND <- factor(NEW_CELL_IND)
  Age <- as.numeric((svc_start_dt - BIRTH_DT)/365)
  status <- factor(is.na(svc_end_dt), levels =c(TRUE,FALSE), labels = c('active','churn'))
})

str(skar)

summary(skar)

##check -- persons who have NA in  PROM 3 also have NA in minue3

table(is.na(skar$prom3), is.na(skar$minuse3))

table(is.na(skar$prom4), is.na(skar$minuse4))

table(is.na(skar$prom2), is.na(skar$minuse2))

table(is.na(skar$prom1), is.na(skar$minuse1))

#are all prom4 na case, chruned
summary(skar$svc_end_dt[is.na(skar$prom4)]) # no missing svc_end_dt.,so all have churned

summary(skar$svc_end_dt[is.na(skar$prom3)]) # no missing svc_end_dt.,so all have churned

# For all non-churn case minues4 is the lastest usage data...
# For 117 - 69 , case who have chruend in 4th month , minues3 is the last usage data
#For 69 who churned in 3rd month, minues2 is the latest usage data.
# so, we need to arrange the data

skar <- skar[!is.na(skar$minuse1),]

skar <- within (skar, {
  use1 <- minuse4
  use1[is.na(use1)] <- minuse3[is.na(use1)] #so, case which have missing value in 4th month get values from month 3
  use1[is.na(use1)] <- minuse2[is.na(use1)] #so, case which have missing value in 4th month get values from month 2
  prom <- prom4
  prom[is.na(prom)] <- prom3[is.na(prom)] #so, case which have missing value in 4th month get values from month 3
  prom[is.na(prom)] <- prom2[is.na(prom)] #so, case which have missing value in 4th month get values from month 2
  Age[is.na(Age)] <- 41
  Age[Age < 18] <- 18
})

summary(skar)
#Draw sample
sam <- sample.int(n=nrow(skar), size =0.7*nrow(skar))

##need to investigate
##r5 <- rpart(status ~ ., data=skar, subset=sam, control=rpart.control(cp=0.01))
##r5
##prp(r5)
##printcp(r5)
##plotcp(r5)
##table(skar[sam,]$status, skar[sam,]$minuse2 < 243.5)

##Model
r6 <- rpart(status ~ Plan.Type + NEW_CELL_IND + use1 + prom, data=skar, subset=sam)
r6
prp(r6)
r6 <- rpart(status ~ Plan.Type + NEW_CELL_IND + use1 + prom, data=skar, subset=sam, 
            control=rpart.control(cp=0.0001))
r6
prp(r6)
printcp(r6)
plotcp(r6)

merr <- which.min(r6$cptable[,'xerror'])
mCP <- r6$cptable[merr, 'CP']
r7 <- prune(r6, cp=mCP)
r7
prp(r7)  

# training confusion matrix
cm <- table(actual=skar[sam, 'status'], pred=predict(r7, type='class'))
cm
prop.table(cm,1)

## test predictions & confusion matrix
cm <- table(actual=skar[-sam, 'status'], pred=predict(r7, newdata=skar[-sam,], type='class'))
cm
prop.table(cm,1)


