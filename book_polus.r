library(broom)
library(e1071)
library(readxl)
library(ROCR)
library(tidyverse)

book <- read_excel("C:/Users/Rachel/Documents/UTSA/MSDA/2021.3/DA6813/case_studies/bookbinders/BBBC-Train.xlsx")
book_test <- read_excel("C:/Users/Rachel/Documents/UTSA/MSDA/2021.3/DA6813/case_studies/bookbinders/BBBC-Test.xlsx")

str(book)
head(book)
anyNA(book)

set.seed(200)
book$Gender <- factor(book$Gender)
book_test$Gender <- factor(book_test$Gender)

form1 <- Choice ~ .
x <- seq(from = 0.01, to = 0.99, by = 0.01)

# Linear regression
m1 <- lm(formula = form1, data = book)

summary(m1)

lmpreds <- predict.lm(m1, newdata = book_test, type = 'response')

# Minimizing difference between sensitivity and specificity
difference <- c()
for(i in x) {
  book.preds.lm <- ifelse(lmpreds >= i, 1, 0)
  confusionMatrix <- caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.lm), positive = "1")
  diff <- abs(confusionMatrix$byClass['Sensitivity']-confusionMatrix$byClass['Specificity'])
  if (is.numeric(diff)) {
    difference <- append(difference, diff)
  }
}

min(difference)
which.min(difference)

book.preds.lm <- ifelse(lmpreds >= which.min(difference)/100, 1, 0)
caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.lm))

#Check for influential variables, only highlighting 2
plot(m1, which = 4, id.n = 2) # cook's distance

# Extract model results to compute std. residuals
m1.data <- augment(m1)
top_n(m1.data, 2, .cooksd)

# find data points with an absolute standardized residuals above 3: outliers 
filter(m1.data, abs(.std.resid) > 3) # none exists

#ROC Curve and AUC
pred <- prediction(predict.lm(m1, newdata = book_test, type = 'response'),book_test$Choice) #Predicted Probability and True Classification

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

logitpreds <- predict.glm(m2, newdata = book_test, type = 'response')

book.preds.logit <- ifelse(logitpreds >= 0.5, 1, 0)
caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.logit))

#Check for influential variables, only highlighting 2
plot(m2, which = 4, id.n = 2) # cook's distance

# Extract model results to compute std. residuals
m2.data <- augment(m2)
top_n(m2.data, 2, .cooksd)

# find data points with an absolute standardized residuals above 3: outliers 
filter(m2.data, abs(.std.resid) > 3) # none exists

#ROC Curve and AUC
pred <- prediction(predict(m2, book_test, type = "response"),book_test$Choice) #Predicted Probability and True Classification

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
table(pred = svmpred, true = book_test$Choice)

difference <- c()
for(i in x) {
  book.preds.svm <- ifelse(svmpred >= i, 1, 0)
  confusionMatrix <- caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.svm), positive = "1")
  diff <- abs(confusionMatrix$byClass['Sensitivity']-confusionMatrix$byClass['Specificity'])
  if (is.numeric(diff)) {
    difference <- append(difference, diff)
  }
}

min(difference)
which.min(difference)

book.preds.svm <- ifelse(svmpred >= which.min(difference)/100, 1, 0)
caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.svm))

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


