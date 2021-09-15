# Rachel Polus
# DA6813

bank <- read.csv("bank-additional.csv", sep = ";")

# Load necessary libraries
library(MASS)
library(ggplot2)

# See the dimensions of the dataset
dim(bank)

# See the composition of the data
str(bank)

# Descriptive Stats
summary(bank)

levels(as.factor(bank$job))
levels(as.factor(bank$marital))
levels(as.factor(bank$education))
levels(as.factor(bank$default))
levels(as.factor(bank$housing))
levels(as.factor(bank$loan))
levels(as.factor(bank$contact))
levels(as.factor(bank$month))
levels(as.factor(bank$day_of_week))
levels(as.factor(bank$poutcome))

bank$result <- ifelse(bank$y == "yes", 1, 0)

# Creating training and testing samples
set.seed(1)
row.number = sample(1:nrow(bank), 0.8*nrow(bank))
banktrain = bank[row.number,]
banktest = bank[-row.number,]

num <- data.frame(matrix(ncol=0, nrow=4119))
num$age <- bank$age
num$duration <- bank$duration
num$emp.var.rate <- bank$emp.var.rate
num$cons.price.idx <- bank$cons.price.idx
num$cons.conf.idx <- bank$cons.conf.idx
num$euribor3m <- bank$euribor3m
num$nr.employed <- bank$nr.employed

cor(num)

# LDA modeling. 
lda.model <- lda(factor(result) ~ age + factor(job) + factor(marital) + factor(education) 
                 + factor(default) + factor(housing) + factor(loan) 
                 + factor(contact) + factor(month) + factor(day_of_week) 
                 + duration + factor(previous) + factor(poutcome)
                 + cons.price.idx + nr.employed, data = banktrain)

# View the output
lda.model

# Predicting for the testing dataset we created
predictions.lda <- predict(lda.model, banktest)

# Make confusion matrix for the LDA predictions to compare accuracy 
caret::confusionMatrix(as.factor(predictions.lda$class), as.factor(banktest$result))

