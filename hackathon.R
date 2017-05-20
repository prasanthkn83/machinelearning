setwd("F:/Hackathon")

###################Data Load###################
bureau <- read.csv("POC_BUREAU.csv", strip.white = TRUE)
demog <- read.csv("POC_DEMOG.csv", strip.white = TRUE)
prod <- read.csv("POC_PROD.csv", strip.white = TRUE)
city <- read.csv("City_Ranking.csv", strip.white = TRUE)
bank <- read.csv("Bank_Types.csv", strip.white = TRUE)

library(plyr)
library(stringr)
library(dplyr)
###################Exploratory Data Analysis###################

###################Bureau###################
head(bureau)
str(bureau)
# balance, sanctionamount to be converted to real
# dateclosed & dateopened for feature convert

# Data type corrections
bureau <- within(bureau, {
  balance <- as.numeric(as.character(balance))
  sanctionamount <- as.numeric(as.character(sanctionamount))
  dateclosed <- as.Date(dateclosed, format = '%Y-%m-%d')
  dateopened <- as.Date(dateopened, format = '%Y-%m-%d')
})

# Data cleansing - Product Description
table(bureau$description)
# Consolidate all hyphen characters
bureau$description <-
  str_replace_all(bureau$description,"\\p{Pd}","-")
# Merge duplicates
bureau$description[bureau$description == 'Busines Loan - Priority Sector - Small Business'] = 'Business Loan - Priority Sector - Small Business'
bureau$description[bureau$description == 'Business Loan General'] = 'Business Loan - General'
bureau$description[bureau$description == 'Commercial_Vehicle'] = 'Commercial Vehicle Loan'
bureau$description[bureau$description == 'Fleet Cards'] = 'Fleet Card'
bureau$description[bureau$description == 'Housing Loan'] = 'Home Loan'
bureau$description[bureau$description == 'Loan againt bank deposits'] = 'Loan Against Bank Deposits'
bureau$description[bureau$description == 'Loan_against_securities'] = 'Loan Against Shares/Securities'
bureau$description[bureau$description == 'Microfinance - Other'] = 'Microfinance - Others'
bureau$description[bureau$description == 'Non Funded Credit Facility'] = 'Non-Funded Credit Facility'
bureau$description[bureau$description == 'Not Categorized'] = 'Others'
bureau$description[bureau$description == ''] = 'Others'
bureau$description[bureau$description == 'Two-wheeler Loan'] = 'Two-Wheeler Loan'
# Level factor
bureau$description <- droplevels(as.factor(bureau$description))

head(arrange(plyr::count(bureau,'description'),desc(freq)),50)

##########Bureau - Identifying Defaulters##########
bureau$dpd <- as.character(bureau$dpd)

bureau$dpd <- str_trim(str_replace_all(bureau$dpd,"NO DATA",""))

bureau$dpdlength <- str_length(bureau$dpd)
bureau$dpdSTDlen <- str_count(bureau$dpd, "STD")
bureau$dpdSUBlen <- str_count(bureau$dpd, "SUB")
bureau$dpdSMAlen <- str_count(bureau$dpd, "SMA")
bureau$dpdDBTlen <- str_count(bureau$dpd, "DBT")
bureau$dpdLSSlen <- str_count(bureau$dpd, "LSS")
bureau$dpdzerolen <- str_count(bureau$dpd, "000")
bureau$dpdXXXlen <- str_count(bureau$dpd, "XXX")
bureau$dpdnonzerolen <-
  str_count(bureau$dpd, "\\d{3}") - bureau$dpdzero

# All accounts set to Zero
bureau$defaulted <- 0

bureau$defaulted[bureau$dpdSMAlen > 1] <-
  1 # Special Mention Accounts
bureau$defaulted[bureau$dpdnonzerolen > 1] <- 2 # Delayed Payment
bureau$defaulted[bureau$dpdSUBlen > 1] <- 3 # Sub Standard Account
bureau$defaulted[bureau$dpdDBTlen > 1] <- 4 # Doubtful Account
bureau$defaulted[bureau$dpdLSSlen > 1] <- 5 # Loss Account

bureau$defaulted <- as.factor(bureau$defaulted)
table(bureau$defaulted)

##########Bureau - Complete Records##########
summary(bureau$dateopened)

# Merging NULL values as NAs
bureau$balance[bureau$balance == "" | bureau$balance == "NULL"] <-
  NA
summary(bureau$balance)
bureau$sanctionamount[bureau$sanctionamount == "" |
                        bureau$sanctionamount == "NULL"] <- NA
summary(bureau$sanctionamount)
str(bureau)

# Identify complete cases where description, ID, data opened, sanction amount are present
bureaucomplete <-
  bureau[complete.cases(bureau[,c('description','dateopened','sanctionamount','PoC_CustID')]),
         c(
           'PoC_CustID','score','description','sanctionamount','balance'
           ,'dateopened','dateclosed','defaulted'
         )]

# Adding credit rating
bureaucomplete$score <-
  as.numeric(as.character(bureaucomplete$score))
bureaucomplete$rating <-
  as.factor(cut(
    bureaucomplete$score, breaks = c(-1, 300, 550, 650, 750, 900), include.lowest =
      TRUE,
    labels = c('UA','BAD','FAIR','GOOD','EXCELLENT')
  ))

head(arrange(plyr::count(
  bureaucomplete,c('PoC_CustID','dateopened','sanctionamount','description')
),desc(freq)),200)

bureaucomplete$defaulted <-
  as.numeric(as.character(bureaucomplete$defaulted))

# Mergin (partial) duplicate records in bureau complete
bureaucomplete <- bureaucomplete %>%
  group_by(PoC_CustID,dateopened,description,score,rating,sanctionamount) %>%
  summarise(
    defaulted = max(defaulted),
    dateclosed = max(dateclosed,na.rm = TRUE),
    balance = min(balance,na.rm = TRUE)
  )

head(arrange(plyr::count(
  bureaucomplete,c('PoC_CustID','dateopened','sanctionamount','description')
),desc(freq)),200)
summary(bureaucomplete)
str(bureaucomplete)

# Aging calculation

today <- Sys.Date()
str(today)

tempdate <-
  ifelse(is.na(as.character(bureaucomplete$dateclosed)),today,bureaucomplete$dateclosed)
class(tempdate) <- "Date"
bureaucomplete$ageopened <-
  difftime(tempdate, bureaucomplete$dateopened, units = "days")
bureaucomplete$ageopened <- as.numeric(bureaucomplete$ageopened)

# Attrition calculation
bureaucomplete$attrition <- 'Y'
bureaucomplete[bureaucomplete$balance > 0 &
                 is.na(as.character(bureaucomplete$dateclosed))  &
                 bureaucomplete$defaulted == 0 &
                 bureaucomplete$ageopened < 6040 ,c('attrition')] <- 'N'
bureaucomplete$attrition <- as.factor(bureaucomplete$attrition)

# No of years since account closed calculation
bureaucomplete$ageclosedyears <- 0
bureaucomplete$ageclosedyears[!is.na(as.character(bureaucomplete$dateclosed))] <-
  round(difftime(today, bureaucomplete$dateclosed[!is.na(as.character(bureaucomplete$dateclosed))], units = "days") /
          365,0)

str(bureaucomplete)

# Preparing a bureau complete summary with frequency, total sanction amount & age closed years
bureausummary <- bureaucomplete %>%
  group_by(PoC_CustID,description,rating) %>%
  summarise(
    frequency = n(),
    totalsanctionamount = sum(sanctionamount),
    attrition = max(as.character(attrition)),
    ageclosedyears = min(ageclosedyears),
    defaulted = max(defaulted)
  )

summary(bureausummary)
str(bureausummary)

bureausummary$rating[is.na(bureausummary$rating)] <- "UA"

# Realigning data types
bureausummary <- within(bureausummary, {
  defaulted <- as.factor(as.character(defaulted))
  frequency <- as.factor(as.character(frequency))
  ageclosedyears <- as.factor(as.character(ageclosedyears))
  attrition <- as.factor(attrition)
})

###################Demographic###################

str(demog)
## comp_name, citydesc, custbankname has >100 values. possbile candidates for feature convertion

summary(demog)
## bin age into age groups
## duplication in martial_status, needs to investigate
## company names also has some data issues, require cleansing, binning
## same comment applicable for city
## custbankname also requires cleansing
## cpccode can be clubbed, grouped
## lot of NULL values, how to treat them ?

# Marital status duplicate data issue correction
table(demog$marital_status)
demog$marital_status[demog$marital_status == 'M'] = 'MARRIED'
demog$marital_status[demog$marital_status == 'S'] = 'SINGLE'
demog$marital_status <- droplevels(demog$marital_status)

# Group age into brackets
summary(as.numeric(demog$age))
demog$agegroup <-
  cut(
    as.numeric(demog$age), breaks = c(1, 18, 25, 30, 35, 40, 45, 50, 55, 60, 70, 80, 90), include.lowest =
      TRUE
  )
demog <- demog[!is.na(demog$agegroup),]

#####Company Name Grouping#####
head(arrange(plyr::count(demog,c('comp_name')),desc(freq)),200)
demog$comp_name[demog$comp_name == "." |
                  demog$comp_name == "NULL"] <- "OTHERS"

compgroup <- arrange(plyr::count(demog,c('comp_name')),desc(freq))
compgroup$comp_group <- "OTHERS"
compgroup$comp_group[2:6] <- "TOP5"
compgroup$comp_group[7:11] <- "TOP6-10"
compgroup$comp_group[12:21] <- "TOP11-20"
compgroup$comp_group[22:51] <- "TOP21-50"
compgroup$comp_group[52:101] <- "51-100"
compgroup$comp_group[102:201] <- "101-200"
compgroup$comp_group[202:501] <- "201-500"
compgroup$comp_group[502:1001] <- "501-1000"
compgroup$comp_group[1002:5001] <- "1001-5000"
compgroup$comp_group[5002:10001] <- "5001-10000"
compgroup$comp_group[10002:20001] <- "10001-20000"
compgroup$comp_group[20002:30001] <- "20001-30000"
compgroup$comp_group[30002:40001] <- "30001-40000"
compgroup$comp_group[40002:50001] <- "40001-50000"

aggregate(freq ~ comp_group, FUN = sum, data = compgroup)
compgroup$freq <- NULL
# Merging company group data with demog
demog <- demog %>% left_join(compgroup, by = "comp_name")
demog$comp_group <- as.factor(demog$comp_group)
demog$comp_group[is.na(demog$comp_group)] <- "OTHERS"

#####City Band Grouping#####
str(city)
names(city) <- c("cityrating","citydesc")
city$citydesc <- as.character(city$citydesc)
demog$citydesc <- as.character(demog$citydesc)
demog <- demog %>% left_join(city, by = "citydesc")
demog$cityrating[is.na(demog$cityrating)] <- 3

#####CPC Code Grouping#####
cpcgroup <- arrange(plyr::count(demog,c('cpcode')),desc(freq))
cpcgroup$cpcgroup <- "OTHERS"
cpcgroup$cpcgroup[1:2] <- as.character(cpcgroup$cpcode[1:2])
cpcgroup$cpcgroup[4:19] <- as.character(cpcgroup$cpcode[4:19])

aggregate(freq ~ cpcgroup, FUN = sum, data = cpcgroup)
cpcgroup$freq <- NULL

demog <- demog %>% left_join(cpcgroup, by = "cpcode")
demog$cpcgroup <- as.factor(demog$cpcgroup)
demog$cpcgroup[is.na(demog$cpcgroup)] <- "OTHERS"

#####Bank Type Grouping#####
names(bank) <- c('banktype','custbankname')

summary(demog$custbankname)
demog$custbankname <-
  str_trim(str_replace_all(str_replace_all(
    str_replace_all(str_to_upper(demog$custbankname),"LIMITED",""),"LTD",""
  ),"\\.",""))

bank$custbankname <-
  str_trim(str_replace_all(str_to_upper(bank$custbankname),"\\.",""))

demog <- demog %>% left_join(bank, by = "custbankname")
demog$banktype <- as.character(demog$banktype)
demog$banktype[is.na(demog$banktype)] = "OTHERS"
demog$banktype <- as.factor(demog$banktype)

# Demog Summary computation
names(demog)
demogsummary <-
  demog[,c(
    "PoC_CustID", "agegroup", "marital_status", "residencetype",
    "cityrating", "comp_group", "cpcgroup", "banktype"
  )]
summary(demogsummary)
str(demogsummary)

# Realigning data types
demogsummary <- within(demogsummary, {
  cityrating <- as.factor(as.character(cityrating))
  banktype <- as.factor(as.character(banktype))
})

#####Sampling#######
sam <-
  sample.int(n = nrow(demogsummary), size = 0.7 * nrow(demogsummary))

demogsummarytrans <- demogsummary[sam,]
nontranscustomers <-
  setdiff(demogsummarytrans$PoC_CustID, bureausummary$PoC_CustID)
demogsummarytrans <-
  demogsummarytrans[!demogsummarytrans$PoC_CustID %in% nontranscustomers,]
demogbureausummary <-
  merge(demogsummarytrans, bureausummary, by = c('PoC_CustID'))

#################Product Recommendation###################
names(demogbureausummary)
productrec <-
  demogbureausummary[,c(
    "PoC_CustID","agegroup","marital_status","residencetype","cityrating",
    "comp_group","cpcgroup","banktype","rating","description","defaulted",
    "totalsanctionamount"
  )]

productrec$defaulted <- as.character(productrec$defaulted)
productrec$defaulted[productrec$defaulted %in% c('1','2','3','4','5')] <-
  'Y'
productrec$defaulted[productrec$defaulted == '0'] <- 'N'
productrec$defaulted <- as.factor(productrec$defaulted)

library(e1071)
library(randomForest)
# library(gbm)
# library(rpart)
#library(svm)
# library(caret)

summary(productrec)
str(productrec)

# m1 <- rpart(defaulted ~ agegroup + marital_status + residencetype + cityrating +
#             comp_group + cpcgroup + banktype + rating + description, data=productrec, method="class")

# gbmmodel<-train(defaulted ~ agegroup + marital_status + residencetype + cityrating +
#                   comp_group + cpcgroup + banktype + rating + description, data=productrec, method="gbm", distribution="bernoulli")

# m2 <- gbm(defaulted ~ agegroup + marital_status + residencetype + cityrating +
#              comp_group + cpcgroup + banktype + rating + description, data=productrec, n.trees=100)

m3 <-
  randomForest(
    defaulted ~ agegroup + marital_status + residencetype + cityrating +
      comp_group + cpcgroup + banktype + rating + description, data =
      productrec, ntree = 100
  )

table(m3$predicted, productrec$defaulted)
m4$fitted

