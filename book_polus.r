library(broom)
library(car)
library(DescTools)
library(e1071)
library(lmtest)
library(MASS)
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

maxAccuracy <- c()

for(i in x) {
  preds <- ifelse(lmpreds >= i, 1, 0)
  
  confusionMatrix <- caret::confusionMatrix(as.factor(book_test$Choice), as.factor(preds))
  maxAccuracy <- append(maxAccuracy, confusionMatrix$overall['Accuracy'])
}

max(maxAccuracy)
which.max(maxAccuracy)

book.preds.lm <- ifelse(lmpreds >= which.max(maxAccuracy)/100, 1, 0)

caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.lm))

mydata = dplyr::select_if(book_test, is.numeric) 
predictors <- colnames(mydata)

mydata = gather(mydata, key = "predictors", value = "predictor.value")

#Check for influential variables, only highlighting 2
plot(m1, which = 4, id.n = 2) # cook's distance

# Extract model results to compute std. residuals
m1.data <- augment(m1)
top_n(m1.data, 2, .cooksd)

# find data points with an absolute standardized residuals above 3: outliers 
filter(m1.data, abs(.std.resid) > 3) # none exists

#check for multicollinearity
vif(m1) #Last_purchase high vif so remove

df2 = select(book_test, -Last_purchase)
m1.1 <- lm(Choice ~., data = df2)
vif(m1.1)

#ROC Curve and AUC
pred <- prediction(predict.lm(m1.1, newdata = book_test, type = 'response'),book_test$Choice) #Predicted Probability and True Classification

# area under curve
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values),3)

# some important statistics
false.rates <-performance(pred, "fpr","fnr")
accuracy <-performance(pred, "acc","err")
perf <- performance(pred, "tpr","fpr")

#plotting the ROC curve and computing AUC
plot(perf,colorize = T, main = "Logit ROC Curve")
text(0.5,0.5, paste("AUC:", auc))

# computing threshold for cutoff to best trade off sensitivity and specificity
#first sensitivity
plot(unlist(performance(pred, "sens")@x.values), unlist(performance(pred, "sens")@y.values), 
     type="l", lwd=2, 
     ylab="Sensitivity", xlab="Cutoff", main = paste("Maximized Cutoff (Linear Regression)\n","AUC: ",auc))

par(new=TRUE) # plot another line in same plot

#second specificity
plot(unlist(performance(pred, "spec")@x.values), unlist(performance(pred, "spec")@y.values), 
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.2)) #specificity axis labels
mtext("Specificity",side=4, col='red')

#find where the lines intersect
min.diff <-which.min(abs(unlist(performance(pred, "sens")@y.values) - unlist(performance(pred, "spec")@y.values)))
min.x<-unlist(performance(pred, "sens")@x.values)[min.diff]
min.y<-unlist(performance(pred, "spec")@y.values)[min.diff]
optimal <-min.x #this is the optimal points to best trade off sensitivity and specificity

abline(h = min.y, lty = 3)
abline(v = min.x, lty = 3)
text(min.x,0,paste("optimal threshold=",round(optimal,2)), pos = 4)

# using cutoff from above
book.preds.lm <- ifelse(lmpreds >= 0.71, 1, 0)

caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.lm))



# Logit
m2 <- glm(formula = form1, data = book, family = binomial)

summary(m2)

logitpreds <- predict.glm(m2, newdata = book_test, type = 'response')

book.preds.logit <- ifelse(logitpreds >= 0.5, 1, 0)
caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.logit))

mydata = dplyr::select_if(book_test, is.numeric) 
predictors <- colnames(mydata)

# Bind the logit and tidying the data for plot
mydata = mutate(mydata, logit = log(logitpreds/(1-logitpreds))) 
mydata = gather(mydata, key = "predictors", value = "predictor.value", -logit)

#making the plot
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Check for influential variables, only highlighting 2
plot(m2, which = 4, id.n = 2) # cook's distance

# Extract model results to compute std. residuals
m2.data <- augment(m2)
top_n(m2.data, 2, .cooksd)

# find data points with an absolute standardized residuals above 3: outliers 
filter(m2.data, abs(.std.resid) > 3) # none exists

#check for multicollinearity
vif(m2) #Last_purchase high vif so remove

df2 = select(book_test, -Last_purchase)
m2.1 <- glm(Choice ~., data = df2, family = binomial)
vif(m2.1) #First_purchase high vif so remove

df3 = select(df2, -First_purchase)
m2.2 <- glm(Choice ~., data = df3, family = binomial)
vif(m2.2) # under 10; you could keep going to under 5 if you like, but I'll stop here.
summary(m2.2)

#ROC Curve and AUC
pred <- prediction(predict(m2.2, book_test, type = "response"),book_test$Choice) #Predicted Probability and True Classification

# area under curve - this is 1, so no need to plot
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values),3)



# SVM
tuned <- tune.svm(form1, data = book, gamma = seq(0.07, 0.08, by = 0.01), cost = seq(0.9, 1.0, by = 0.1))
#tuned <- tune.svm(form1, data = book, gamma = seq(0.01, 0.1, by = 0.03), cost = seq(0.1, 1.0, by = 0.3))

tuned$best.parameters
tuned$performances

m3 <- svm(formula = form1, data = book, gamma = tuned$best.parameters$gamma, cost = tuned$best.parameters$cost)
summary(m3)

svmpred <- predict(m3, book_test, type = "response")
table(pred = svmpred, true = book_test$Choice)

maxAccuracy <- c()

for(i in x) {
  preds <- ifelse(svmpred >= i, 1, 0)

  confusionMatrix <- caret::confusionMatrix(as.factor(book_test$Choice), as.factor(preds))
  maxAccuracy <- append(maxAccuracy, confusionMatrix$overall['Accuracy'])
}

max(maxAccuracy)
which.max(maxAccuracy)

book.preds.svm <- ifelse(svmpred >= which.max(maxAccuracy)/100, 1, 0)
caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.svm))

mydata = dplyr::select_if(book_test, is.numeric) 
predictors <- colnames(mydata)
 
mydata = gather(mydata, key = "predictors", value = "predictor.value")

#Check for influential variables, only highlighting 2
plot(m3, which = 4, id.n = 2) # cook's distance

#ROC Curve and AUC
pred <- prediction(predict(m3, book_test, type = "response"),book_test$Choice) #Predicted Probability and True Classification
pred. 

# area under curve
auc <- round(as.numeric(performance(pred, measure = "auc")@y.values),3)

# some important statistics
false.rates <-performance(pred, "fpr","fnr")
accuracy <-performance(pred, "acc","err")
perf <- performance(pred, "tpr","fpr")

#plotting the ROC curve and computing AUC
plot(perf,colorize = T, main = "Logit ROC Curve")
text(0.5,0.5, paste("AUC:", auc))

# computing threshold for cutoff to best trade off sensitivity and specificity
#first sensitivity
plot(unlist(performance(pred, "sens")@x.values), unlist(performance(pred, "sens")@y.values), 
     type="l", lwd=2, 
     ylab="Sensitivity", xlab="Cutoff", main = paste("Maximized Cutoff (SVM)\n","AUC: ",auc))

plot(scaled.x, unlist(performance(pred, "sens")@y.values), 
     type="l", lwd=2, 
     ylab="Sensitivity", xlab="Cutoff", main = paste("Maximized Cutoff (SVM)\n","AUC: ",auc))

par(new=TRUE) # plot another line in same plot

#second specificity
plot(unlist(performance(pred, "spec")@x.values), unlist(performance(pred, "spec")@y.values), 
     type="l", lwd=2, col='red', ylab="", xlab="")
axis(4, at=seq(0,1,0.2)) #specificity axis labels
mtext("Specificity",side=4, col='red')

#find where the lines intersect
min.diff <-which.min(abs(unlist(performance(pred, "sens")@y.values) - unlist(performance(pred, "spec")@y.values)))
min.x<-unlist(performance(pred, "sens")@x.values)[min.diff]
min.y<-unlist(performance(pred, "spec")@y.values)[min.diff]
optimal <-min.x #this is the optimal points to best trade off sensitivity and specificity

abline(h = min.y, lty = 3)
abline(v = min.x, lty = 3)
text(min.x,0,paste("optimal threshold=",round(optimal,2)), pos = 4)

book.preds.svm <- ifelse(svmpred >= 0.85, 1, 0)
caret::confusionMatrix(as.factor(book_test$Choice), as.factor(book.preds.svm))


x.vals <- performance(pred, "sens")@x.values[[1]]
x.vals <- x.vals[!is.infinite(x.vals)]

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
tail(x.vals)
scaled.x <- range01(x.vals)
tail(scaled.x)
