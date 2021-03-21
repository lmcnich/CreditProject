#Things to do?
#Create a new column variable for total number of creditlines/loans
#The Y variable 'SeriosDlqi2yrs' why is not in data set?
#Run summary diagnostics to check p-value or test statistics for signfigance and drop variables


rm(list=ls())
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(pROC)
library(xgboost)
library(Amelia)
library(psych)

training <- read_csv("https://raw.githubusercontent.com/mattmcd71/fnce5352_spring2021/main/Assignments/ConsumerCredit/ConsumerCred-train.csv")
test <- read_csv("https://raw.githubusercontent.com/mattmcd71/fnce5352_spring2021/main/Assignments/ConsumerCredit/ConsumerCred-test.csv")

#Clean up the data by rearranging rows to make it easier to view && training data set is now traininga variable
training <- training %>% relocate(`NumberOfTime60-89DaysPastDueNotWorse`, .after = `NumberOfTime30-59DaysPastDueNotWorse`)
training <- training %>% relocate(`NumberRealEstateLoansOrLines`, .before = `NumberOfOpenCreditLinesAndLoans`)
training <- training %>% relocate(`age`, .after = `NumberOfDependents`)
training <- training %>% relocate(`NumberOfTimes90DaysLate`, .after = `NumberOfTime60-89DaysPastDueNotWorse`)
training <- training %>% relocate(`DebtRatio`, .after = `RevolvingUtilizationOfUnsecuredLines`)
training <- training %>% relocate(`MonthlyIncome`, .after = `DebtRatio`)


#test
test <- test %>% relocate(`NumberOfTime60-89DaysPastDueNotWorse`, .after = `NumberOfTime30-59DaysPastDueNotWorse`)
test <- test %>% relocate(`NumberRealEstateLoansOrLines`, .before = `NumberOfOpenCreditLinesAndLoans`)
test <- test %>% relocate(`age`, .after = `NumberOfDependents`)
test <- test %>% relocate(`NumberOfTimes90DaysLate`, .after = `NumberOfTime60-89DaysPastDueNotWorse`)
test <- test %>% relocate(`DebtRatio`, .after = `RevolvingUtilizationOfUnsecuredLines`)
test <- test %>% relocate(`MonthlyIncome`, .after = `DebtRatio`)


View(training) #view the csv file

####CLEAN THE DATA

myData <- training

#WHERE DATA IS MISSING
sapply(myData,function(x) sum(is.na(x)))
#MISSMAP
missmap(myData, legend = TRUE, col = c("white","firebrick2"), main= "Missing values vs observed from Training set",yaxt='n')


#NumberOfDependents cleaner
#Missing data
sum(is.na(myData$NumberOfDependents))
summary(myData$NumberOfDependents)
#Median
myData$NumberOfDependents[is.na(myData$NumberOfDependents)] <- median(myData$NumberOfDependents,na.rm=T)
myData$NumberOfDependents<-ifelse(myData$NumberOfDependents>10,median(myData$NumberOfDependents),myData$NumberOfDependents)
#Histogram
par(lend="butt")
rf <- sum(myData$SeriousDlqin2yrs == 0, na.rm = TRUE) #Person no experienced 90 days past due delinquency or worse 
rt <- sum(myData$SeriousDlqin2yrs == 1, na.rm = TRUE) #Person experienced 90 days past due delinquency or worse
n <- rf + rt
dst <- density(myData$NumberOfDependents, na.rm = TRUE)
dstg <- density(myData$NumberOfDependents[myData$SeriousDlqin2yrs == 0], na.rm = TRUE)
dstf <- density(myData$NumberOfDependents[myData$SeriousDlqin2yrs == 1], na.rm = TRUE)
hist(myData$NumberOfDependents, col = grey(0.9), border = grey(0.8),
     main = paste("Number of dependents"),
     xlab = "Number",
     proba = TRUE, ylim = c(0, max(dst$y)+0.002))
lines(dstg$x, rf/n*dstg$y, lwd = 3, col = "blue")
lines(dstf$x, rf/n*dstf$y, lwd = 3, col = "red")
lines(dst$x, dst$y)
legend("topright", inset = 0.01, legend = c("Delinquency", "No delinquency","Total"),
       col = c("red","blue","black"),
       lty = c(1, 1,1), lwd = 2, pt.cex = 2)


#NumberRealEstateLoansOrLines cleaner
summary(myData$NumberRealEstateLoansOrLines)
sum(is.na(myData$NumberRealEstateLoansOrLines))
table(myData$NumberRealEstateLoansOrLines)
#Median
myData$NumberRealEstateLoansOrLines<-ifelse(myData$NumberRealEstateLoansOrLines==54,median(myData$NumberOfOpenCreditLinesAndLoans),myData$NumberRealEstateLoansOrLines)

#Histogram
par(lend="butt")
rf <- sum(myData$SeriousDlqin2yrs == 0, na.rm = TRUE) #Person no experienced 90 days past due delinquency or worse 
rt <- sum(myData$SeriousDlqin2yrs == 1, na.rm = TRUE) #Person experienced 90 days past due delinquency or worse
n <- rf + rt
dst <- density(myData$NumberRealEstateLoansOrLines, na.rm = TRUE)

hist(myData$NumberRealEstateLoansOrLines, col = grey(0.5), border = grey(0.9),
     main = paste("Number of real estate loans or lines"),
     xlab = "Number",
     proba = TRUE, ylim = c(0, max(dst$y)+0.002))
lines(dst$x, dst$y,col="blue")


#NumberOfOpenCreditLinesAndLoans cleaner
sum(is.na(myData$NumberOfOpenCreditLinesAndLoans))
summary(myData$NumberOfOpenCreditLinesAndLoans)
#Replace by median (more than 15 is strange)
myData$NumberOfOpenCreditLinesAndLoans<-ifelse(myData$NumberOfOpenCreditLinesAndLoans>15,median(myData$NumberOfOpenCreditLinesAndLoans),myData$NumberOfOpenCreditLinesAndLoans)
#Boxplot
boxplot(myData$NumberOfOpenCreditLinesAndLoans,col = "blue")



#Monthly Income cleaner
sum(is.na(myData$MonthlyIncome))
summary(myData$MonthlyIncome)
#If salary > 100 000 (senseless)
myData$MonthlyIncome[is.na(myData$MonthlyIncome)]<-median(myData$MonthlyIncome,na.rm = TRUE)
myData$MonthlyIncome[(myData$MonthlyIncome)>100000]<-median(myData$MonthlyIncome,na.rm = TRUE)

#Histogram
par(lend="butt")
rf <- sum(myData$SeriousDlqin2yrs == 0, na.rm = TRUE) #Person no experienced 90 days past due delinquency or worse 
rt <- sum(myData$SeriousDlqin2yrs == 1, na.rm = TRUE) #Person experienced 90 days past due delinquency or worse
n <- rf + rt
dst <- density(myData$MonthlyIncome, na.rm = TRUE)
dstg <- density(myData$MonthlyIncome[myData$SeriousDlqin2yrs == 0], na.rm = TRUE)
dstf <- density(myData$MonthlyIncome[myData$SeriousDlqin2yrs == 1], na.rm = TRUE)
hist(myData$MonthlyIncome, col = grey(0.9), border = grey(0.8),
     main = paste("Monthly income"),
     xlab = "Income",
     proba = TRUE, ylim = c(0, max(dst$y)))
lines(dstg$x, rf/n*dstg$y, lwd = 3, col = "blue")
lines(dstf$x, rf/n*dstf$y, lwd = 3, col = "red")
lines(dst$x, dst$y)
legend("topright", inset = 0.01, legend = c("Delinquency", "No delinquency","Total"),
       col = c("red","blue","black"),
       lty = c(1, 1,1), lwd = 2, pt.cex = 2)


#Debt Ratio cleaner
summary(myData$DebtRatio)
sum(is.na(myData$DebtRatio))
range(myData$DebtRatio)
#Replace by median
myData$DebtRatio<-ifelse(myData$DebtRatio<0,median(myData$DebtRatio),myData$DebtRatio)
myData$DebtRatio<-ifelse(myData$DebtRatio>1,median(myData$DebtRatio),myData$DebtRatio)

#Boxplot
boxplot(myData$DebtRatio, notch=TRUE, 
        col="gold",
        main="Dept ratio")


#Age cleaner
sum(is.na(myData$age))
summary(myData$age)
#Histograme of age
par(lend="butt")
rf <- sum(myData$SeriousDlqin2yrs == 0, na.rm = TRUE) #Person no experienced 90 days past due delinquency or worse 
rt <- sum(myData$SeriousDlqin2yrs == 1, na.rm = TRUE) #Person experienced 90 days past due delinquency or worse
n <- rf + rt
dst <- density(myData$age, na.rm = TRUE)
dstg <- density(myData$age[myData$SeriousDlqin2yrs == 0], na.rm = TRUE)
dstf <- density(myData$age[myData$SeriousDlqin2yrs == 1], na.rm = TRUE)
hist(myData$age, col = grey(0.9), border = grey(0.8),
     main = paste("Age of ", nrow(myData), " clients"),
     xlab = "Age [years]",
     proba = TRUE, ylim = c(0, max(dst$y)+0.002))
lines(dstg$x, rf/n*dstg$y, lwd = 3, col = "blue")
lines(dstf$x, rf/n*dstf$y, lwd = 3, col = "red")
lines(dst$x, dst$y)
legend("topright", inset = 0.01, legend = c("Delinquency", "No delinquency","Total"),
       col = c("red","blue","black"),
       lty = c(1, 1,1), lwd = 2, pt.cex = 2)


#RevolvingUtilizationOfUnsecuredLines cleaner
summary(myData$RevolvingUtilizationOfUnsecuredLines)

# It should be between 0 and 1. (%)
sum(is.na(myData$RevolvingUtilizationOfUnsecuredLines))
sum(myData$RevolvingUtilizationOfUnsecuredLines>1)

# Median (?)
myData$RevolvingUtilizationOfUnsecuredLines[myData$RevolvingUtilizationOfUnsecuredLines>1]=median(myData$RevolvingUtilizationOfUnsecuredLines)

#If <0 -> 0 | If >1 -> 1 
myData$RevolvingUtilizationOfUnsecuredLines<-ifelse(myData$RevolvingUtilizationOfUnsecuredLines<0,0,myData$RevolvingUtilizationOfUnsecuredLines)
myData$RevolvingUtilizationOfUnsecuredLines<-ifelse(myData$RevolvingUtilizationOfUnsecuredLines>1,1,myData$RevolvingUtilizationOfUnsecuredLines)

#Boxplot
boxplot(myData$RevolvingUtilizationOfUnsecuredLines,col="green")



#NumberOfTime30.59DaysPastDueNotWorse cleaner
sum(is.na(myData$NumberOfTime30.59DaysPastDueNotWorse))
table(myData$NumberOfTime30.59DaysPastDueNotWorse)

#Absurd data (>50 for example)
myData$NumberOfTime30.59DaysPastDueNotWorse[myData$NumberOfTime30.59DaysPastDueNotWorse>=50]<-0
#Histogram
hist(myData$NumberOfTime30.59DaysPastDueNotWorse,col="blue")


#NumberOfTime60.89DaysPastDueNotWorse cleaner
sum(is.na(myData$NumberOfTime60.89DaysPastDueNotWorse))
table(myData$NumberOfTime30.59DaysPastDueNotWorse)
#Absurd data (>50 for example)
myData$NumberOfTime60.89DaysPastDueNotWorse[myData$NumberOfTime60.89DaysPastDueNotWorse>50]<-0
#Histogram
hist(myData$NumberOfTime60.89DaysPastDueNotWorse,col="grey")


#NumberOfTimes90DaysLate cleaner
sum(is.na(myData$NumberOfTimes90DaysLate))
summary(myData$NumberOfTimes90DaysLate)
table(myData$NumberOfTimes90DaysLate)
#More than 20 is non sense 
myData$NumberOfTimes90DaysLate<-ifelse(myData$NumberOfTimes90DaysLate>50,0,myData$NumberOfTimes90DaysLate)
#Histogram
hist(myData$NumberOfTimes90DaysLate,col="green")




#Information after data distributions & imputation :
  describeBy(myData)


#Correlation
#The correlation plot is usefull to check the correlated variable
  
names(myData) <- c("Result", "DefaultLine","Age","DelayBetween3059","DebtRatio","MonthlyIncome","NbLoanCredit","DelaySup90","NbRealEstateLoansLines","DelayBetween6089","NumberOfDependents")
head(myData)
cor(myData)
M <- cor(myData)
  corrplot(M, method = "square")

  names(myData) <-c("SeriousDlqin2yrs","RevolvingUtilizationOfUnsecuredLines","age",
                    "NumberOfTime30.59DaysPastDueNotWorse","DebtRatio","MonthlyIncome",
                    "NumberOfOpenCreditLinesAndLoans","NumberOfTimes90DaysLate",
                    "NumberRealEstateLoansOrLines","NumberOfTime60.89DaysPastDueNotWorse")
  
#xgboost deals with NA treats as data
y <- as.numeric(myData$SeriousDlqin2yrs)
X <- myData[, !(colnames(myData) == 'SeriousDlqin2yrs')]

params <- list(eval_metrics = 'auc', objective = 'binary:logistic')

model <- xgboost(data=as.matrix(X), label=y, params=params, nrounds=20, verbose=1)

#xgb.plot.shap(data=as.matrix(X), model=model, top_n=3)

ptrain <- predict(model, as.matrix(X))
p <- predict(model, as.matrix(test))

id <- as.numeric(test$id)

res <- data.frame(id, p)

#area under curve
roc(y, ptrain, plot=TRUE)





#Summary statistics for Age
age_summary <- traininga %>% 
  summarize(
    min = min(age, na.rm = TRUE),
    q1 = quantile(age, 0.25, na.rm = TRUE),
    median = quantile(age, 0.5, na.rm = TRUE),
    q3 = quantile(age, 0.75, na.rm = TRUE),
    max = max(age, na.rm = TRUE),
    mean = mean(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE),
    missing = sum(is.na(age))
  )
age_summary

#Histogram for age
ggplot(data = traininga, mapping = aes(x = age)) +
  geom_histogram(color = "white", bins = 20)
