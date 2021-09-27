library(e1071)
library(readxl)
library(MASS)
library(ggplot2)

book <- read_excel("C:/Users/Rachel/Documents/UTSA/MSDA/2021.3/DA6813/case_studies/bookbinders/BBBC-Train.xlsx")
book_test <- read_excel("C:/Users/Rachel/Documents/UTSA/MSDA/2021.3/DA6813/case_studies/bookbinders/BBBC-Test.xlsx")

str(book)

head(book)

anyNA(book)

set.seed(200)
book$Gender <- factor(book$Gender)
book_test$Gender <- factor(book_test$Gender)

form1 <- Choice ~ .

tuned <- tune.svm(form1, data = book, gamma = seq(0.07, 0.08, by = 0.01), cost = seq(0.9, 1.0, by = 0.1))

tuned$best.parameters

tuned$performances

mysvm <- svm(formula = form1, data = book, gamma = tuned$best.parameters$gamma, cost = tuned$best.parameters$cost)
summary(mysvm)

svmpred <- predict(mysvm, book_test, type = "response")
table(pred = svmpred, true = book_test$Choice)

preds <- ifelse(svmpred >= 0.5, 1, 0)

caret::confusionMatrix(as.factor(book_test$Choice), as.factor(preds))




m1 <- glm(formula = form1, data = book, family = binomial)

summary(m1)

book.preds <- predict.glm(m1, newdata = book_test, type = 'response')

book.preds.bin <- ifelse(book.preds >= 0.5, 1, 0)

caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.bin))



m2 <- lm(formula = form1, data = book)

summary(m2)

book.preds <- predict.lm(m2, newdata = book_test, type = 'response')

book.preds.bin <- ifelse(book.preds >= 0.5, 1, 0)

caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.bin))
