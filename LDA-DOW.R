library(ISLR)
library(randomForest)
library(dplyr)
library(caret)
library(e1071)
library(rpart)
library(caretEnsemble)
library(tree)
library(MASS)
library(tm)


set.seed(1)
#data = read.csv("C:\\Users\\micha\\Dropbox\\MSDA\\Fall 2021\\Applications\\Dow Jones Case Study\\dow_jones_index.data", header = TRUE, sep = ",")

data = read.csv("/Users/michael/Dropbox/MSDA/Fall 2021/Applications/Dow Jones Case Study/dow_jones_index.data")

increase = ifelse(data$percent_change_price >= 0, 'Yes','No')
data = data.frame(data, increase)

data$open = as.numeric(gsub("\\$","",data$open))
data$high = as.numeric(gsub("\\$","",data$high))
data$low = as.numeric(gsub("\\$","",data$low))
data$close = as.numeric(gsub("\\$","",data$close))

data$next_weeks_open = as.numeric(gsub("\\$","",data$next_weeks_open ))
data$next_weeks_close = as.numeric(gsub("\\$","",data$next_weeks_close ))

data$date = as.Date(data$date, "%m/%d/%Y")
week = as.numeric(strftime(data$date, format = "%V"))
data = data.frame(data, week)
day = as.numeric(strftime(data$date, format = '%j'))
data = data.frame(data,day)

lag1 = lag(data$close, n =1)
data = data.frame(data,lag1)

train = filter(data, quarter == 1)
test = filter(data, quarter == 2)


train = na.omit(train)
test = na.omit(test)


## LDA
m2 = lda(factor(increase) ~ lag1 + volume + next_weeks_open + previous_weeks_volume + days_to_next_dividend, data = train)

pred.m2 = predict(m2, test)

confusionMatrix(pred.m2$class, as.factor(test$increase))





