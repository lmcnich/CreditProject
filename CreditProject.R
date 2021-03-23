#Things to do?
library(xgboost)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(pROC)

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


####CLEAN THE DATA

myData <- training

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
auc <- roc(y, ptrain, plot=TRUE)
print(auc)
