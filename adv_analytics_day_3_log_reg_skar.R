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

mean(skar$Age)

skar <- within (skar, {
  use1 <- minuse4
  use1[is.na(use1)] <- minuse3[is.na(use1)] #so, case which have missing value in 4th month get values from month 3
  use1[is.na(use1)] <- minuse2[is.na(use1)] #so, case which have missing value in 4th month get values from month 2
  prom <- prom4
  prom[is.na(prom)] <- prom3[is.na(prom)] #so, case which have missing value in 4th month get values from month 3
  prom[is.na(prom)] <- prom2[is.na(prom)] #so, case which have missing value in 4th month get values from month 2
  Age[is.na(Age)] <- 41 #assign average value for missing data
  Age[Age < 18] <- 18
})

summary(skar)

names(skar)[sapply(skar, is.numeric)]

boxplot(use1 ~ status, skar)
boxplot(Age ~ status, skar)
prop.table(table(skar[, c('prom','status')]),2)

#... difference bt promos is insignificant..

names(skar)[sapply(skar, is.factor)]
library(vcd)

tbl <- table(skar[,c('status', 'Plan.Type')])

tbl

assocstats(tbl)

tbl <- table(skar[,c('status', 'NEW_CELL_IND')])

tbl

assocstats(tbl)

#build the model
#variabels..


# draw sample
sam <- sample.int(n=nrow(skar), size =0.7*nrow(skar))

prop.table(table(skar$status[sam]))
prop.table(table(skar$status[-sam]))
# samples are equally proportional


##model

g1 <-  glm(status ~ Plan.Type + NEW_CELL_IND + use1 + prom, skar,
           family='binomial', subset=sam)

summary(g1)

library(car)
Anova(g1, type='III')

#model diagnostics

tProb <- predict(g1, type ='response')
summary(tProb)
vProb <- predict(g1, newdata = skar[-sam,], type ='response')

tClass <- factor(tProb > 0.5, levels = c(FALSE, TRUE), labels = c('active','churn'))
tClass <- factor(tProb < 0.5, levels = c(FALSE, TRUE), labels = c('active','churn'))

cmFunc <- function(actual, pred){ # actual is skar$status and pred ae the predicted probabilities
  predClass <- factor(pred > 0.5, levels = c(FALSE, TRUE), labels = c('active','churn'))
  tbl <- table(actual = actual,pred=predClass)
  cat("confusion matrix:\n")
  print(tbl)
  cat("\nTPR,FPR, TNR, FNR:\n")
  print(prop.table(tbl,margin=1))
  #Lift deciles
  predBin <- cut(pred, breaks=quantile(pred, probs = seq(0,1, by=0.1)))
  tbl <- table(actual = actual,pred=predBin)
  cat('Life table\n')
  print(tbl)
  tbl <- prop.table(tbl, 2)[2,]
  cat('Lift vector:\n')
  print(tbl)
  plot(tbl, type='o', main ='Lift chart')
  ## Somer's
  library(Hmisc)
  somers2(x=pred, y= actual =="churn")
  
}

cmFunc(skar$status[sam],pred=tProb)
cmFunc(skar$status[-sam],pred=vProb)

#-- updated model
g2 <-  glm(status ~ Plan.Type + NEW_CELL_IND + use1 + prom +Age, skar,
           family='binomial', subset=sam)

summary(g2)

library(car)
Anova(g2, type='III')

#model diagnostics

tProb <- predict(g2, type ='response')
summary(tProb)
vProb <- predict(g2, newdata = skar[-sam,], type ='response')

tClass <- factor(tProb > 0.5, levels = c(FALSE, TRUE), labels = c('active','churn'))
tClass <- factor(tProb < 0.5, levels = c(FALSE, TRUE), labels = c('active','churn'))

cmFunc <- function(actual, pred){ # actual is skar$status and pred ae the predicted probabilities
  predClass <- factor(pred > 0.5, levels = c(FALSE, TRUE), labels = c('active','churn'))
  tbl <- table(actual = actual,pred=predClass)
  cat("confusion matrix:\n")
  print(tbl)
  cat("\nTPR,FPR, TNR, FNR:\n")
  print(prop.table(tbl,margin=1))
  #Lift deciles
  predBin <- cut(pred, breaks=quantile(pred, probs = seq(0,1, by=0.1)))
  tbl <- table(actual = actual,pred=predBin)
  cat('Life table\n')
  print(tbl)
  tbl <- prop.table(tbl, 2)[2,]
  cat('Lift vector:\n')
  print(tbl)
  plot(tbl, type='o', main ='Lift chart')
  
}

cmFunc(skar$status[sam],pred=tProb)
cmFunc(skar$status[-sam],pred=vProb)

vif(g2)

# another model.. lrm(model), package rms; model output