#Things to do?
#Create a new column variable for total number of creditlines/loans
#The Y variable 'SeriosDlqi2yrs' why is not in data set?
#Run summary diagnostics to check p-value or test statistics for signfigance and drop variables



library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

training <- read_csv("https://raw.githubusercontent.com/mattmcd71/fnce5352_spring2021/main/Assignments/ConsumerCredit/ConsumerCred-test.csv")

#Clean up the data by rearranging rows to make it easier to view && training data set is now traininga variable
traininga <- training %>% relocate(`NumberOfTime60-89DaysPastDueNotWorse`, .after = `NumberOfTime30-59DaysPastDueNotWorse`)
traininga <- traininga %>% relocate(`NumberRealEstateLoansOrLines`, .before = `NumberOfOpenCreditLinesAndLoans`)
traininga <- traininga %>% relocate(`age`, .after = `NumberOfDependents`)
traininga <- traininga %>% relocate(`NumberOfTimes90DaysLate`, .after = `NumberOfTime60-89DaysPastDueNotWorse`)
traininga <- traininga %>% relocate(`DebtRatio`, .after = `RevolvingUtilizationOfUnsecuredLines`)
traininga <- traininga %>% relocate(`MonthlyIncome`, .after = `DebtRatio`)


View(traininga) #view the csv file


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
