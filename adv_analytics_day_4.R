## Logistic regression representation
y <- rep(0:1, each=30)
x <- rnorm(60, 5, 0.5)

x[31:60] <- x[31:60] + 1.5

plot(y ~ x)
##Logisitic Regression computation
# 1 / (1 + exp(-z))
yhat <- 1/ (1 + exp(-1 * (x - mean(x))))

points(x=x, y=yhat, pch='+', col='red')

yhat <- 1/ (1 + exp(-10 * (x - mean(x))))

points(x=x, y=yhat, pch='+', col='red')

#y=1 when x=4 & y=0 when x=7
abline(a=3, b=-3/7)
#anything below line failure, anything above line success


###Linear SVM

df <- iris[iris$Species != 'setosa', c('Petal.Width','Petal.Length', 'Species')]
str(df)
df$Species <- droplevels(df$Species)
plot(df[,-3], col=df$Species)
library(e1071)

s1 <- svm(Species ~ ., data=df, kernel='linear')
s1
plot(s1, df)
names(s1)
cm <- table(actual=df$Species, pred=s1$fitted)
cm

s1 <- svm(Species ~ ., data=df, kernel='linear', cost=0.01)
s1
plot(s1, df)
names(s1)
cm <- table(actual=df$Species, pred=s1$fitted)
cm

st1 <- tune(svm, Species ~., data=df, kernel='linear', 
            ranges=list(cost=c(0.01,0.1,1,5,10,50,100),
                        gamma=c(0.1, 0.3, 0.5, 0.7, 1, 5)))
st1
names(st1)
st1$best.model
st1$performances

st1 <- tune(svm, Species ~., data=df, kernel='linear', 
            ranges=list(cost=seq(0.1, 0.5, by=0.01)))
st1$performances
st1$best.model

#Polynomial Kernel
s2 <- svm(Species ~., data=df, kernel='polynomial', degree=2, gamma=1, cost=1)
s2
plot(s2, df)
cm <- table(actual=df$Species, pred=s2$fitted)
cm
st2 <- tune(svm, Species ~., data=df, kernel='polynomial', 
            ranges=list(degree=c(2, 5), 
                        gamma=c(0.1, 0.5, 1, 1.5, 2, 5), 
                        cost=c(0.1, 0.5, 1, 10)
                        ))
st2$performances
st2$best.performance
st2b <- st2$best.model
plot(st2b, df)

cm <- table(actual=df$Species, pred=st2b$fitted)
cm
#handwritten digit recognition svm fits best
#logistic regression fits most of the scenarios where an explanation is required

#Radial Kernel
s3 <- svm(Species ~., data=df, kernel='radial', gamma=1, cost=1)
s3
plot(s3, df)
cm <- table(actual=df$Species, pred=s3$fitted)
cm
st3 <- tune(svm, Species ~., data=df, kernel='radial', 
            ranges=list(gamma=c(0.1, 0.5, 1, 5, 10, 100), 
                        cost=c(0.1, 0.5, 1, 10, 100)
            ))
st3$performances
st3$best.performance
st3b <- st3$best.model
st3b
plot(st3b, df)

cm <- table(actual=df$Species, pred=st3b$fitted)
cm

##goodbad
gb <- read.csv("GOODBAD.csv")

gb$y <- factor(gb$Good.Bad, levels=c(1,2), labels=c('Good','Bad'))
str(gb)

sam <- sample.int(nrow(gb), size =0.7 * nrow(gb))
svm1 <- svm(y ~ ., gb[sam, -which(names(gb)=='Good.Bad')])
svm1
plot(svm1, gb, Duration ~ Amount)


##training
cm <- table(actual=gb$y[sam], pred=svm1$fitted)
cm
#test
cm <- table(actual=gb$y[-sam], pred=predict(svm1, newdata=gb[-sam,]))
cm

?tune

svm1 <- tune(svm, y ~ ., df, 
             kernel='linear',
             ranges=list(cost=c(0.1,1)),
             tunecontrol=list(sampling="fix"))
# error

## skar

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

##Model
svmsk <- svm(status ~ Plan.Type + NEW_CELL_IND + use1 + prom, data=skar[sam,])
svmsk
table(actual=skar[sam, 'status'], pred=svmsk$fitted)

svmsk <- tune(svm, status ~Plan.Type + NEW_CELL_IND + use1 + prom, data=skar[sam,],
              ranges=list(cost=c(0.1,1)))
svmsk$performances
svmsk2 <- svmsk$best.model
table(actual=skar[sam, 'status'], pred=svmsk2$fitted)

## ANN
df <- iris[iris$Species != 'setosa', ] #c('Petal.Width','Petal.Length', 'Species')
str(df)
df$Species <- droplevels(df$Species)
plot(df[,-5], col=df$Species)
library(nnet)
nn1 <- nnet(Species ~ ., df, size=3)
summary(nn1)
predict(nn1, type='class')
table(actual=df$Species, pred=predict(nn1, type='class'))

##Whole of iris data
nn2 <-nnet(Species ~ ., iris, size=3)
summary(nn2)
table(actual=iris$Species, pred=predict(nn2, type='class'))

nn2 <-nnet(Species ~ ., iris, size=10, skip=TRUE) #ski might provide better fitmet
summary(nn2)
table(actual=iris$Species, pred=predict(nn2, type='class'))

library(neuralnet)
iris$Species <-as.integer(iris$Species)
?neuralnet
nn3 <-neuralnet(formula=Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris, 
                hidden=c(4,2), algorithm = 'rprop+')
nn3
summary(nn3)
plot(nn3)
nn3$net.result[[1]]
table(actual=iris$Species, pred=cut(nn3$net.result[[1]], breaks=c(0, 1.5, 2.5, 3.5)))

##skar
sknn <- nnet(status ~ Plan.Type + NEW_CELL_IND + use1 + prom, data=skar[sam,], size=3)
table(actual=skar[sam,'status'], pred=predict(sknn, type='class'))

##converting skar to numeric matrix for neural net processing
sk <- model.matrix(~ status + Plan.Type + NEW_CELL_IND + use1 + prom - 1, data=skar[sam,])
dim(sk)
paste(colnames(sk), collapse=" + ")
##sknn2 <- neuralnet(statuschurn ~ 'Plan.TypeCoast to Coast' + 'Plan.TypeNights and Weekends' + NEW_CELL_INDU + NEW_CELL_INDY + use1 + prom,
##                   sk, hidden = c(4, 3))
##table(actual=sk[,'statuschurn'], pred=cut(nn2$net.result[[1]]),)

##Association Rule Mining

library(arules)

#Epub access data -- already stored as transactions sparse matrix, dataset in arules packages
data('Epub')
Epub
summary(Epub)
str(Epub)
inspect(head(Epub))

head(transactionInfo(Epub))
head(itemInfo(Epub))
head(itemsetInfo(Epub))
class(Epub)

summary(transactionInfo(Epub))
xx <- transactionInfo(Epub)
str(xx)

year <- strftime(xx[,'TimeStamp'], '%Y')
year <- strftime(transactionInfo(Epub)[,'TimeStamp'], '%Y')
table(year)

E2003 <- Epub[year=='2003']
str(E2003)
image(E2003)
image(Epub[year=='2006'])
inspect(head(Epub))

inspect(E2003[size(E2003)>20])
Etid <- as(Epub, 'tidLists')

as(Etid[900:905], 'list')
someDocs <- head(itemInfo(Epub))

##In which sessions were these someDocs docIds downloaded?

xx <- as(Etid, 'list')
names(xx)
xx[unlist(someDocs)]

itemFrequencyPlot(Epub, support=0.01, cex.names=0.6)

#Another example -- dataframe source
data('AdultUCI')
dim(AdultUCI)
head(AdultUCI)
str(AdultUCI)

toRemove <- which(names(AdultUCI) %in% c("education-num","fnlwgt"))
AdultUCI <- AdultUCI[,-toRemove]

names(AdultUCI) <- gsub(pattern='-', replacement ='.', x=names(AdultUCI), fixed=TRUE)

summary(AdultUCI)

AdultUCI <- within(AdultUCI,{
  age <- cut(age, breaks=c(-Inf, 25, 35, 45, 55, 65, Inf))
  hours.per.week <- cut(hours.per.week, c(0, unique(quantile(AdultUCI$hours.per.week))))
  capital.gain <- cut(capital.gain, c(unique(quantile(AdultUCI$capital.gain))))
  capital.loss <- cut(capital.loss, c(unique(quantile(AdultUCI$capital.loss))))
})

str(AdultUCI)

Adult <- as(AdultUCI, 'transactions')
Adult
str(Adult)

transactionInfo(Adult) <- data.frame(...)

data('Adult')
Adult
summary(Adult)

itemFrequencyPlot(Adult, cex=0.8, support=0.1)

rules <- apriori(Adult, parameter=list(support=0.01, confidence=0.6))
rules
summary(rules)

## What results in small or large income?
rulesIncomeSmall <- subset(rules, subset= rhs %in% 'income=small' & lift > 1.2)
inspect(head(sort(rulesIncomeSmall, by='confidence')))

rulesIncomeLarge <- subset(rules, subset= rhs %in% 'income=large' & lift > 1.2)
inspect(head(sort(rulesIncomeLarge, by='confidence')))

write(rulesIncomeSmall, file="rulesIncomeSmall.csv", sep=',', col.names=NA)
library(pmml)
write.PMML(head(rulesIncomeSmall), file="rulesIncomeSmall.xml")

## Sampling transactions database
## for an itemset X with support t = supp(X), for an acceptable relative error of support e.
## (i.e accuracy of 1-e) at a given confidence level. 1-c, the needed sample size is given by
## Chernov bounds on binomial distrbution

data('Adult')
Adult
summary(Adult)

t <- 0.05
e <- 0.1
c <- 0.1

n <- -2*log(c)/(t*e^2)
n

set.seed(1)
aSam <- sample(Adult, size=n, replace=TRUE)
#compare the sample with the population
itemFrequencyPlot(aSam, population=Adult, support=t, cex.names=0.7,
                  popCol='blue', popLwd=2)
itemFrequencyPlot(aSam, population=Adult, support=t, cex.names=0.7, lift=TRUE)
                  
## ECLAT(Equivalence Class trasnformation) is a depth-first serach alogrith using set transactiob
# to compare run times.. extract frequent itemsets
fisSam <- eclat(aSam, parameter=list(support=t),
                control=list(verbose=FALSE))
inspect(head(sort(fisAdult, by="support")))

##run time

##Retail purchase transaction dataset
retail <- read.transactions("retail.dat.gz", sep=' ')
retail
str(retail)
inspect(head(retail)) ##inspect is required to see the actual transactions, whats are the items

head(transactionInfo(retail))
head(itemInfo(retail)) ##just the ids
head(itemsetInfo(retail))
class(retail)

##LIST List Representaion for Objects Basedon on "itemMatrix"

retail.list <- LIST(head(retail))

## summary of data
retSumr <- summary(retail)
retSumr

str(retSumr)

barplot(retSumr@lengths, xlab='Transaction Size', ylab='Number of Transactions')
itemFrequencyPlot(retail, support=0.01)

ret39.48 <- subset(retail, subset = retail %in% c('39','48'))
retail1 <- subset(retail, subset = !(retail %in% c('39','48')))

retail.rows.10.15 <- retail[10:15,]
inspect(retail.rows.10.15)
str(retail.rows.10.15)

retR1 <- apriori(retail, parameter=list(support=0.001, confidence=0.4))
retR1
inspect(sort(retR1, by='confidence'))

retR1 <- subset(retR1, subset=!(lhs %in% c('32','38','39','41','48')))
retR1
inspect(sort(retR1, by='confidence'))

retR2 <- apriori(retail, parameter=list(support=0.001, confidence=0.5))
retR2
inspect(sort(retR2, by='confidence'))

retR1 <- apriori(retail, parameter=list(support=0.001, confidence=0.4),
                 appearance=list(none=c('32','38','39','41','48')))
retR1
inspect(sort(retR1, by='confidence'))

##closure of an itemset, closed itemset
## maximal itemset
## http://www.hypertextbookshop.com/dataminingbook/public_version/contents/chapters/chapter002/section004/blue/page002.html

data(Groceries)
g1 <- head(Groceries, 4)
inspect(g1)

aff.g1 <- affinity(Groceries)
aff.g1[c('whole milk', 'other vegetables'), c('meat spreads', 'yogurt')]

#aggregate  Aggregate Items into Item Groups
head(itemInfo(Groceries))
aggregate(Groceries, "level2")
