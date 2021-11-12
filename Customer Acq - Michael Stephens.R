library(SMCRM)
library(randomForest)
library(Metrics)

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
     main = 'Predicted vs Actual Values'


p = ggplot(error.df, aes(actual, pred1))
p + geom_point(colour = "blue")
p + geom_line(aes(y=actual,pred1))





