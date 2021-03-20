library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

training <- read_csv("https://raw.githubusercontent.com/mattmcd71/fnce5352_spring2021/main/Assignments/ConsumerCredit/ConsumerCred-test.csv")
View(training) #view the csv file

#Summary statistics for Age
age_summary <- training %>% 
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
ggplot(data = training, mapping = aes(x = age)) +
  geom_histogram(color = "white", bins = 20)
