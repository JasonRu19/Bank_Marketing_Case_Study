library(dplyr)
library(tree)
library(ISLR)
library(e1071)

data <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Case Studies/3_Dow_Jones_Case_Study/dow_jones_index.data")

str(data)

increase = ifelse(data$percent_change_price >= 0, 'Yes','No')
data$open = as.numeric(gsub("\\$","",data$open))
data$high = as.numeric(gsub("\\$","",data$high))
data$low = as.numeric(gsub("\\$","",data$low))
data$close = as.numeric(gsub("\\$","",data$close))
data$next_weeks_open = as.numeric(gsub("\\$","",data$next_weeks_open ))
data$next_weeks_close = as.numeric(gsub("\\$","",data$next_weeks_close ))
data$date = as.Date(data$date, "%m/%d/%Y")
week = as.numeric(strftime(data$date, format = "%V"))
day = as.numeric(strftime(data$date, format = '%j'))
lag1 = lag(data$close, n =1)
data = data.frame(data, increase, week, day, lag1)

data$increase = as.factor(data$increase)
data = na.omit(data)


train <- filter(data, quarter == 1 )
test <- filter(data, quarter == 2 )

svm1 = svm(increase ~.-quarter-week-day-date-stock, data = train, kernel = 'radial', gamma = 0.1, cost = 10)

summary(svm1)
pred = predict(svm1, test)
confusion_matrix = table(test$increase, pred)
confusion_matrix
(195+170)/(195+170+15+10)
#93.59


svm2 = svm(increase ~.-quarter-week-day-date, data = train, kernel = 'radial', gamma = 0.1, cost = 10)
summary(svm2)
pred2 = predict(svm2, test)
confusion_matrix2 = table(test$increase, pred2)
confusion_matrix2
(190+166)/(190+166+14+20)
#91.28