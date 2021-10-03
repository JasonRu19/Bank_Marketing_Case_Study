library(broom)
library(car)
library(e1071)
library(readxl)
library(ROCR)
library(tidyverse)

book <- read_excel("C:/Users/Rachel/Documents/UTSA/MSDA/2021.3/DA6813/case_studies/bookbinders/BBBC-Train.xlsx")
book_test <- read_excel("C:/Users/Rachel/Documents/UTSA/MSDA/2021.3/DA6813/case_studies/bookbinders/BBBC-Test.xlsx")

str(book)
head(book)
anyNA(book)

# removing observation, as it is not needed
book <- select(book, -Observation)
book_test <- select(book_test, -Observation)

set.seed(200)
book$Gender <- factor(book$Gender)
book_test$Gender <- factor(book_test$Gender)

form1 <- Choice ~ .
x <- seq(from = 0.01, to = 0.99, by = 0.01)

# checking normality
qplot(sample = Amount_purchased, data = book)
qplot(sample = Amount_purchased, data = book_test)
qplot(sample = Frequency, data = book)
qplot(sample = Frequency, data = book_test)
qplot(sample = Last_purchase, data = book)
qplot(sample = Last_purchase, data = book_test)
qplot(sample = First_purchase, data = book)
qplot(sample = First_purchase, data = book_test)
qplot(sample = P_Child, data = book)
qplot(sample = P_Child, data = book_test)
qplot(sample = P_Youth, data = book)
qplot(sample = P_Youth, data = book_test)
qplot(sample = P_Cook, data = book)
qplot(sample = P_Cook, data = book_test)
qplot(sample = P_DIY, data = book)
qplot(sample = P_DIY, data = book_test)
qplot(sample = P_Art, data = book)
qplot(sample = P_Art, data = book_test)

qplot(sample = log(Amount_purchased), data = book)
qplot(sample = log(Amount_purchased), data = book_test)
qplot(sample = log(Frequency), data = book)
qplot(sample = log(Frequency), data = book_test)
qplot(sample = log(Last_purchase), data = book)
qplot(sample = log(Last_purchase), data = book_test)
qplot(sample = log(First_purchase), data = book)
qplot(sample = log(First_purchase), data = book_test)
qplot(sample = log(P_Child), data = book)
qplot(sample = log(P_Child), data = book_test)
qplot(sample = log(P_Youth), data = book)
qplot(sample = log(P_Youth), data = book_test)
qplot(sample = log(P_Cook), data = book)
qplot(sample = log(P_Cook), data = book_test)
qplot(sample = log(P_DIY), data = book)
qplot(sample = log(P_DIY), data = book_test)
qplot(sample = log(P_Art), data = book)
qplot(sample = log(P_Art), data = book_test)


# Linear regression
m1 <- lm(formula = form1, data = book)

summary(m1)

vif(m1)

df2 <- select(book, -Last_purchase)
m1.1 <- lm(formula = form1, data = df2)
vif(m1.1) #First_purchase high vif so remove

summary(m1.1)

lmpreds <- predict.lm(m1.1, newdata = book_test, type = 'response')

# Minimizing difference between sensitivity and specificity
difference <- c()
for(i in x) {
  book.preds.lm <- ifelse(lmpreds >= i, 1, 0)
  confusionMatrix <- caret::confusionMatrix(as.factor(book.preds.lm), as.factor(book_test$Choice), positive = "1")
  diff <- abs(confusionMatrix$byClass['Sensitivity']-confusionMatrix$byClass['Specificity'])
  if (is.numeric(diff)) {
    difference <- append(difference, diff)
  }
}

min(difference)
which.min(difference)

book.preds.lm <- ifelse(lmpreds >= which.min(difference)/100, 1, 0)
caret::confusionMatrix(as.factor(book.preds.lm), as.factor(book_test$Choice), positive = "1")

# variable importance
caret::varImp(m1.1)

#Check for influential variables, only highlighting 2
plot(m1.1, which = 4, id.n = 2) # cook's distance

# Extract model results to compute std. residuals
m1.data <- augment(m1.1)
top_n(m1.data, 2, .cooksd)

# find data points with an absolute standardized residuals above 3: outliers 
filter(m1.data, abs(.std.resid) > 3) # none exists

#ROC Curve and AUC
pred <- prediction(predict.lm(m1.1, newdata = book_test, type = 'response'),book_test$Choice) #Predicted Probability and True Classification

# area under curve
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values),3)

# some important statistics
false.rates <-performance(pred, "fpr","fnr")
accuracy <-performance(pred, "acc","err")
perf <- performance(pred, "tpr","fpr")

#plotting the ROC curve and computing AUC
plot(perf,colorize = T, main = "Linear Regression ROC Curve")
text(0.5,0.5, paste("AUC:", auc))



# Logit
m2 <- glm(formula = form1, data = book, family = binomial)

summary(m2)

#check for multicollinearity
vif(m2) #Last_purchase high vif so remove

df2 = select(book, -Last_purchase)
m2.1 <- glm(Choice ~., data = df2, family = binomial)
vif(m2.1) #First_purchase high vif so remove

summary(m2.1)

logitpreds <- predict.glm(m2.1, newdata = book_test, type = 'response')

# Minimizing difference between sensitivity and specificity
difference <- c()
for(i in x) {
  book.preds.glm <- ifelse(logitpreds >= i, 1, 0)
  tried <- try(confusionMatrix <- caret::confusionMatrix(as.factor(book.preds.glm), as.factor(book_test$Choice), positive = "1"))
  if(class(tried) == "try-error") next;
  diff <- abs(confusionMatrix$byClass['Sensitivity']-confusionMatrix$byClass['Specificity'])
  if (is.numeric(diff)) {
    difference <- append(difference, diff)
  }
}

min(difference)
which.min(difference)

book.preds.glm <- ifelse(logitpreds >= which.min(difference)/100, 1, 0)
caret::confusionMatrix(as.factor(book.preds.glm), as.factor(book_test$Choice), positive = "1")

# variable importance
caret::varImp(m2.1)

#Check for influential variables, only highlighting 2
plot(m2.1, which = 4, id.n = 2) # cook's distance

# Extract model results to compute std. residuals
m2.data <- augment(m2.1)
top_n(m2.data, 2, .cooksd)

# find data points with an absolute standardized residuals above 3: outliers 
filter(m2.data, abs(.std.resid) > 3) # none exists

#ROC Curve and AUC
pred <- prediction(predict(m2.1, book_test, type = "response"),book_test$Choice) #Predicted Probability and True Classification

# area under curve - this is 1, so no need to plot
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values),3)

# some important statistics
false.rates <-performance(pred, "fpr","fnr")
accuracy <-performance(pred, "acc","err")
perf <- performance(pred, "tpr","fpr")

#plotting the ROC curve and computing AUC
plot(perf,colorize = T, main = "Logit ROC Curve")
text(0.5,0.5, paste("AUC:", auc))



# SVM
tuned <- tune.svm(form1, data = book, gamma = seq(0.07, 0.08, by = 0.01), cost = seq(0.9, 1.0, by = 0.1))
#tuned <- tune.svm(form1, data = book, gamma = seq(0.01, 0.1, by = 0.03), cost = seq(0.1, 1.0, by = 0.3))

tuned$best.parameters
tuned$performances

m3 <- svm(formula = form1, data = book, gamma = tuned$best.parameters$gamma, cost = tuned$best.parameters$cost)
summary(m3)

svmpred <- predict(m3, book_test, type = "response")

difference <- c()
for(i in x) {
  book.preds.svm <- ifelse(svmpred >= i, 1, 0)
  confusionMatrix <- caret::confusionMatrix(as.factor(book.preds.svm), as.factor(book_test$Choice), positive = "1")
  diff <- abs(confusionMatrix$byClass['Sensitivity']-confusionMatrix$byClass['Specificity'])
  if (is.numeric(diff)) {
    difference <- append(difference, diff)
  }
}

min(difference)
which.min(difference)

book.preds.svm <- ifelse(svmpred >= which.min(difference)/100, 1, 0)
caret::confusionMatrix(as.factor(book.preds.svm), as.factor(book_test$Choice), positive = "1")

#Check for influential variables, only highlighting 2
plot(m3, which = 4, id.n = 2) # cook's distance

#ROC Curve and AUC
pred <- prediction(predict(m3, book_test, type = "response"),book_test$Choice) #Predicted Probability and True Classification

# area under curve
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values),3)

# some important statistics
false.rates <-performance(pred, "fpr","fnr")
accuracy <-performance(pred, "acc","err")
perf <- performance(pred, "tpr","fpr")

#plotting the ROC curve and computing AUC
plot(perf,colorize = T, main = "SVM ROC Curve")
text(0.5,0.5, paste("AUC:", auc))

# Factor weights
weight <- t(m3$coefs) %*% m3$SV
weight <- apply(weight, 2, function(v){sqrt(sum(v^2))})
weight <- sort(weight, decreasing = TRUE)
print(weight)

# profit for a sold book is 31.95-15*1.45 (price of book for company plus price of overhead)
# multiply by historic probability of success (9.03%)
# subtract cost of mailers to all 50000 people
10.2*50000*0.0903-50000*0.65

# checking that rate of positive response in testing data makes sense
sum(book_test$Choice)/length(book_test$Choice)

# linreg
50000*(584+144)/2300
(31.95 -15*1.45)*15826*0.1987-15826*0.65
(31.95 -15*1.45)*15826*0.1987-15826*0.65-13553

# logit
50000*(605+144)/2300
(31.95 -15*1.45)*16283*0.1923-16283*0.65
(31.95 -15*1.45)*16283*0.1923-16283*0.65-13553

# SVM
50000*(605+139)/2300
(31.95 -15*1.45)*16174*0.1868-16174*0.65
(31.95 -15*1.45)*16174*0.1868-16174*0.65-13553
