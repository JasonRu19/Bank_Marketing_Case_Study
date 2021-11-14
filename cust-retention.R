library(SMCRM) # CRM data
library(caret)
library(dplyr)
library(ggplot2) # plotting
library(Metrics)
library(pdp)
library(randomForest)
library(randomForestSRC)
library(rpart)


data("acquisitionRetention")
acquisition <- acquisitionRetention

# split into train and test
set.seed(123)
train <- sample(1:nrow(acquisition), size = 0.7 * nrow(acquisition))
train.df <- acquisition[train,]
test.df <- acquisition[-train,]

# theme for nice plotting
theme_nice <- theme_classic()+
  theme(
    axis.line.y.left = element_line(colour = "black"),
    axis.line.y.right = element_line(colour = "black"),
    axis.line.x.bottom = element_line(colour = "black"),
    axis.line.x.top = element_line(colour = "black"),
    axis.text.y = element_text(colour = "black", size = 12),
    axis.text.x = element_text(color = "black", size = 12),
    axis.ticks = element_line(color = "black")) +
  theme(
    axis.ticks.length = unit(-0.25, "cm"), 
    axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
    axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))


### Duration

forest2 = rfsrc(duration ~ ., 
                data = train.df,                  
                importance = TRUE, 
                ntree = 1000)

data.frame(importance = forest2$importance) %>%
  tibble::rownames_to_column(var = "variable") %>%
  ggplot(aes(x = reorder(variable,importance), y = importance)) +
  geom_bar(stat = "identity", fill = "orange", color = "black")+
  coord_flip() +
  labs(x = "Variables", y = "Variable importance")+
  theme_nice 

forest2 = rfsrc(duration ~ 
                  crossbuy +
                  acquisition +
                  ret_exp +
                  freq_sq +
                  ret_exp_sq +
                  freq + 
                  sow + 
                  profit,
                data = train.df,
                importance = TRUE,
                ntree = 1000)

#Establish a list of possible values for hyper-parameters
mtry.values <- seq(4,8,1)
nodesize.values <- seq(4,8,2)
ntree.values <- seq(4e3,6e3,1e3)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry.values, nodesize = nodesize.values, ntree = ntree.values)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model = rfsrc(duration ~ 
                  crossbuy +
                  acquisition +
                  ret_exp +
                  freq_sq +
                  ret_exp_sq +
                  freq + 
                  sow + 
                  profit,
                data = train.df,
                mtry = hyper_grid$mtry[i],
                nodesize = hyper_grid$nodesize[i],
                ntree = hyper_grid$ntree[i])  
  
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[length(model$err.rate)]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])


forestDuration.hyper = rfsrc(duration ~ 
                               crossbuy +
                               acquisition +
                               ret_exp +
                               freq_sq +
                               ret_exp_sq +
                               freq + 
                               sow + 
                               profit,
                             data = train.df,
                             mtry = 4,
                             nodesize = 4,
                             ntree = 4000)

length(test.df$duration)

error.df = 
  data.frame(pred1 = predict.rfsrc(forest2,newdata = test.df)$predicted, 
             pred2 = predict.rfsrc(forestDuration.hyper, newdata = test.df)$predicted,
             actual = test.df$duration, 
             customer = test.df$customer) %>%
  mutate_at(.funs = funs(abs.error = abs(actual - .),
                         abs.percent.error = abs(actual - .)/abs(actual)),
            .vars = vars(pred1:pred2))

#mae
error.df %>%
  summarise_at(.funs = funs(mae = mean(.)), 
               .vars = vars(pred1_abs.error:pred2_abs.error))


sqrt(mean((error.df$actual - error.df$pred1)^2))

rmse(error.df$actual, error.df$pred1)


plot(x=error.df$pred1, y=error.df$actual,
     xlab = 'Predicted Values',
     ylab = 'Actual Values',
     main = 'Predicted vs Actual Values')
     
p = ggplot(error.df, aes(actual, pred1))
p + geom_point(colour = "blue")
p + geom_line(aes(y=actual,pred1))
     

### acquisition

# logit
logit.model <- glm(acquisition ~  employees + industry + revenue + acq_exp, 
                   data = train.df, family=binomial)
logit.preds <- ifelse(predict(logit.model, test.df)>0.5,1,0)
confusionMatrix(as.factor(logit.preds), as.factor(test.df$acquisition))


# decision tree for acquisition
dt.model <- rpart(acquisition ~ acq_exp +
                    industry +
                    revenue +
                    employees, 
                  data = train.df) # simple DT model

rattle::fancyRpartPlot(dt.model, sub = "") # vizualize the DT

dt.preds <- ifelse(predict(dt.model, test.df)>0.5,1,0)
confusionMatrix(as.factor(dt.preds), as.factor(test.df$acquisition))


# random forest for acquisition
set.seed(234)
forest <- rfsrc(acquisition ~ acq_exp + 
                   industry + 
                   revenue + 
                   employees, 
                 data = train.df, 
                 importance = TRUE, 
                 ntree = 1000)

# variable importance in random forest model
data.frame(importance = forest$importance) %>%
  tibble::rownames_to_column(var = "variable") %>%
  ggplot(aes(x = reorder(variable,importance), y = importance)) +
  geom_bar(stat = "identity", fill = "orange", color = "black")+
  coord_flip() +
  labs(x = "Variables", y = "Variable importance")

# plot the OOB error rate
data.frame(err.rate = forest$err.rate) %>%
  na.omit() %>%
  tibble::rownames_to_column(var = "trees") %>%
  mutate(trees = as.numeric(trees)) %>%
  ggplot(aes(x = trees, y = err.rate, group = 1))+
  geom_line()+
  scale_x_continuous(breaks = seq(0,1050,100))+
  labs(x = "Number of trees", y = "OOB Error rate")


# Establish a list of possible values for hyper-parameters
mtry.values <- seq(2,4,1)
nodesize.values <- seq(4,8,2)
ntree.values <- seq(4e3,6e3,1e3)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry.values, nodesize = nodesize.values, ntree = ntree.values)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
set.seed(345)
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- rfsrc(acquisition ~ acq_exp + 
                   industry +
                   revenue + 
                   employees, 
                 data = train.df,
                 mtry = hyper_grid$mtry[i],
                 nodesize = hyper_grid$nodesize[i],
                 ntree = hyper_grid$ntree[i])  
  
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[length(model$err.rate)]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])

model <- rfsrc(acquisition ~ acq_exp + 
                 industry +
                 revenue + 
                 employees, 
               data = train.df,
               mtry = 2,
               nodesize = 6,
               ntree = 6000)

preds <- ifelse(model$predicted.oob>0.5,1,0)
confusionMatrix(as.factor(preds), as.factor(train.df$acquisition))

pred.acq <- ifelse(predict(model, newdata = test.df)$predicted>0.5,1,0)
confusionMatrix(as.factor(pred.acq), as.factor(test.df$acquisition))

# Partial Dependence Plots
model <- randomForest(acquisition ~ acq_exp + 
                        industry +
                        revenue + 
                        employees, 
                      data = train.df,
                      mtry = 2,
                      nodesize = 6,
                      ntree = 5000) 

partialPlot(model, pred.data = train.df, x.var = "acq_exp")
partialPlot(model, pred.data = train.df, x.var = "industry")
partialPlot(model, pred.data = train.df, x.var = "revenue")
partialPlot(model, pred.data = train.df, x.var = "employees")

forestDuration.hyper = randomForest(duration ~ 
                               crossbuy +
                               acquisition +
                               ret_exp +
                               freq_sq +
                               ret_exp_sq +
                               freq + 
                               sow + 
                               profit,
                             data = train.df,
                             mtry = 4,
                             nodesize = 4,
                             ntree = 4000)

partialPlot(forestDuration.hyper, pred.data = train.df, x.var = "crossbuy")
partialPlot(forestDuration.hyper, pred.data = train.df, x.var = "acquisition")
partialPlot(forestDuration.hyper, pred.data = train.df, x.var = "ret_exp")
partialPlot(forestDuration.hyper, pred.data = train.df, x.var = "ret_exp_sq")
partialPlot(forestDuration.hyper, pred.data = train.df, x.var = "freq")
partialPlot(forestDuration.hyper, pred.data = train.df, x.var = "freq_sq")
partialPlot(forestDuration.hyper, pred.data = train.df, x.var = "sow")
partialPlot(forestDuration.hyper, pred.data = train.df, x.var = "profit")

nonzero.duration <- acquisition$duration[acquisition$acquisition>0]
mean(nonzero.duration)
mean(acquisition$employees)
