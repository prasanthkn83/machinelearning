# Day 2; Logistic regression

## GOODBAD.CSV - credit score
## skar - Telephone churm example

#####################Load & Explore Data#####################
gb <- read.csv("GOODBAD.csv")
#####################Univariate Exploration#####################
str(gb)
summary(gb)
table(gb$Good.Bad)
## 
gb$y <- factor(gb$Good.Bad, levels=c(1,2), labels=c('Good','Bad'))
str(gb)
contrasts(gb$y)
# to identify the reference levels for factor
# use relevel() to correct the contrasts if required

#####################Bivariate Exploration#####################
## 1. Response variable is categorical
## 1. Categorical Vs Continous; e.g. y vs Duration
## Spearman's rank correlation -- Rank sum test
boxplot(Duration ~ y, gb) # may be interesting
names(gb)[sapply(gb, is.numeric)]
boxplot(Amount ~ y, gb)
boxplot(Rate ~ y, gb) # not ideal for box plot, 4 values
boxplot(CurrResidTenure ~ y, gb) # not ideal for box plot, 4 values
boxplot(Age ~ y, gb) # may be interesting
boxplot(ExCredit ~ y, gb)# not ideal for box plot,
boxplot(NumLiab ~ y, gb)# not ideal for box plot,

# How to check significance of a categorical vairable in classification of another categorical vairable
names(gb)[sapply(gb, is.factor)]


library(vcd)
for(colName in names(gb)[sapply(gb, is.factor)]){
  print(colName)
  tbl <- table(gb[,c(colName, 'y')])
  print(tbl)
  print(assocstats(tbl))
}
# to identfy association values of these variables, Cramer's V of 0.15 is a good indicator
# association is high, interesting -> Check_Account_Status, CreditHistory, Purpose, SavingsAcc

##sam <- sample.int(n=nrow(gb), size=0.7 * nrow(gb))
## generalized linear model
g1 <- glm(y ~ Check_Account_Status + CreditHistory + Purpose 
          + SavingsAcc + Duration + Age, gb, family='binomial') ##, subset=sam)
## binomial - binary classification "logistic regression" problem
## poisson - counts data (number of persons waiting in a queue)
## expected life time - exponential or weibul etc.

g1
summary(g1)
library(car)
          ## Analysis of Variance is a comparison of more than two means
          mtcars$cyl <- factor(mtcars$cyl)
          boxplot(mpg ~ cyl, mtcars)
          aggregate(mpg ~ cyl, mtcars, FUN=mean)
          # H0 : All means are same
          ## H1 : Atleast one mean is different
          anova(aov(mpg ~ cyl, mtcars))
anova(g1)
Anova(g1, type='III')
##Check thie signficance using anova results, insignficant variables needs to be treated
names(g1)

summary(g1$fitted)
hist(g1$fitted)
gb$Pred <- g1$fitted
gb$PredDef <- factor(g1$fitted>0.5, level=c(FALSE, TRUE), labels=c('Good', 'Bad'))
cm <- table(gb[,c('y','PredDef')])

## Confusion Matrix
# PredDef
# y      Good/NonDef            Bad/Default (Target Value)
# NonDef 630  (True Negative)   70 (False Positive)
# Default   167 (False Negative)  133 (True Positive)

pcm <- prop.table(cm)

##correct classification rate: - acuracy
sum(diag(pcm))
##Misclassification rate: - error rate
1- sum(diag(pcm))
##True positive rate - 
##False postive rate - 0.44
prop.table(cm,1)

#PredDef
#y           Good       Bad
#Good 0.9000000 (TNR) 0.1000000 (FPR)
#Bad  0.5566667 (FPR) 0.4433333 (TPR)

## Concordance & Discordance
# y predicted probabilty
## 
## Good 0.05 Concordance
## Bad  0.67 Concordance
## Good 0.57 Discordance
## Bad  0.54 Discordance
head(gb[,c('y','Pred')])

quantile(gb$Pred, seq(0,1, by=0.1)) # split at different percentages
gb$PredBin <- cut(gb$Pred, breaks=quantile(gb$Pred, seq(0,1, by=0.1)))
table(gb$PredBin)
cm1 <- table(gb[,c('y','PredBin')])
# Confusion matrix with different bins, doesnt look good from a Bad value perspective

plot(prop.table(cm1,2)[2,],type='l', main='Lift Chart')# This is the LIFT chart
# There shouldnt be any dips in the chart

##Concordence/Discordence check
library(Hmisc)
somers2(x = gb$Pred, y = gb$y=='Bad')
# C is area under ROC curve
# C & Dxy is pretty good in the scenario, should be greater than 0.5

#AUC plotting
library(ROCR)
?performance
pp <- prediction(predictions = g1$fitted, labels = gb$y)
plot(performance(pp, measure='fpr', x.measure='tpr'))
plot(performance(pp, measure='sens', x.measure='spec'))
plot(performance(pp, measure='prec', x.measure='rec'))


## lift curves

## Sampling (i.e. model building & validation)

### Explore interaaction effect



#removing Age from model using Anova analysis results
g2 <- glm(y ~ Check_Account_Status + CreditHistory + Purpose 
          + SavingsAcc + Duration, gb, family='binomial')
summary(g2)

#compare two models
anova(g2, g1, test='Chisq')
#test to check whether additional/removal vairable results in improvmenet of results

summary(g1$fitted)
hist(g1$fitted)
p1 <- predict(g1, type='response')
head(p1)
head(g1$fitted)

## Automate the variable search when you have large number of variables
names(gb)
gb <- gb[, 1:22]
add1(g1, scope= ~ . + Debtors + Tel + Job + Rate, test='Chisq')
# Debtors are variables that can be considered

g1 <- update(g1, formula = ~ . + Debtors)
summary(g1)
Anova(g1, type='III')

add1(g1, scope= ~ . + Tel + Job + Rate, test='Chisq')
# Debtors are variables that can be considered

g1 <- update(g1, formula = ~ . + Rate)
summary(g1)
Anova(g1, type='III')

#automated method
steps(g1, scope= ~ . + Debtors )