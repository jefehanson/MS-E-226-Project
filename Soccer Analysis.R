library(tidyverse)
library(dplyr)
library(cvTools) 
library(GGally)
library(readr)
library(lubridate)
library(boot)
library(ggplot2)
library(glmnet)

urlfile="https://raw.githubusercontent.com/jefehanson/MS-E-226-Project/main/premier%20soccer%20data.csv"

df_soccer_raw<-read_csv(url(urlfile))

df_soccer <- df_soccer_raw %>% select(-1, -24:-139) #removing division and all of the betting columns and naming it df_soccer2
df_soccer <- df_soccer[complete.cases(df_soccer), ]
df_soccer$Date <-  dmy(df_soccer$Date) #converting 'date' column from a character to a date
df_soccer$weekday <- weekdays(df_soccer$Date) #adding 'weekday' column to indicate date of week
df_soccer$day_of_year <- yday(df_soccer$Date)

count_days <- table(df_soccer$day_of_year)
df_count_days <- as.data.frame(count_days)#the first spike in games is at day 207
ggplot(data = df_count_days, mapping = aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") #wanted to visualize it to confirm

#calculating how far into season (days above 207)
df_soccer <- df_soccer %>% 
  mutate(days_into = ifelse(day_of_year > 207, day_of_year - 207, day_of_year +(366-207)))

# 1 = Yes, I want to watch game, 0 = No, I don't want to watch game. is total game goals higher than avg total game goals? yes=1, no=0
df_soccer <- df_soccer %>% 
  mutate(watch_game = ifelse(
    ((FTHG + FTAG) - mean(FTHG + FTAG))>0, 1, 0))  
#calc no. of points earned each game and replacing "FTR" with points
df_soccer$FTR <- ifelse(df_soccer$FTR == "A", 0,
                        ifelse(df_soccer$FTR == "D", 1,
                               ifelse(df_soccer$FTR == "H", 3, NA)
                        ))
names(df_soccer)[names(df_soccer) == "FTR"] <- "Points" #renaming FTR to #Points
df_soccer <- subset(df_soccer, select = -c(FTAG, FTHG))




#Splitting data so that we have a 20% holdout for the end of class
set.seed(1) 
df_soccer$id <- 1:nrow(df_soccer) #adding a unique ID column
in.train <-  sample(nrow(df_soccer), size = nrow(df_soccer)*.8) #setting 80% of the data for the train
df_soccer2 <-  df_soccer[in.train, ] #this is the 80% we can work with until the end
df_class_test <-  df_soccer[-in.train, ] #setting aside the holdout data to 





#Removing the categorical outcome variable (and "Referee" so cvFit will work)
df_soccer3 <- subset(df_soccer2, select = -c(Referee, id))

#Model & CV  *1*
model <-  lm(Points ~ ., data = df_soccer3)
summary(model)

predict_model <-  predict(model, data = df_soccer3)
rmse_model <-  sqrt(mean((df_soccer3$Points - predict_model)^2))
model_cv10 <- cvFit(model, data = df_soccer3, K=10, y=df_soccer3$Points, seed=1)
model_cv10

plot(model$fitted.values, model$residuals)

ggplot(data.frame(x = model$fitted.values, resid = model$residuals), aes(x, resid)) +
  geom_point() +
  stat_smooth(method = "loess")

var(model$residuals)

head(df_soccer3)

#model 1a. WITH LASSO 
x <- data.matrix(df_soccer3[, c('HTHG', 'HTAG', 'HTR', 'HS', 'AS', 'HST', 'AST', 'HC' , 'AC', 'HY', 'AY', 'HR', 'AR')])
test <- subset(df_class_test, select = c('Points', 'HTHG', 'HTAG', 'HTR', 'HS', 'AS', 'HST', 'AST', 'HC' , 'AC', 'HY', 'AY', 'HR', 'AR'))
head(test)
lasso <- glmnet(x, df_soccer3$Points)

### Use cross-validation to select the optimal lambda value
lasso_cv_model <- cv.glmnet(x, df_soccer3$Points, alpha = 1)
lambda <- lasso_cv_model$lambda.min

### Fit the model using the entire training set with the optimal lambda value
fit <- glmnet(x, df_soccer3$Points, alpha = 1, lambda = lambda)

### Evaluate the performance of the model on the test set
pred <- predict(fit, x)
lasso_mse <- mean((pred - df_soccer3$Points)^2)
lasso_mse

lasso_mse_test2 <- mean((pred - test$Points)^2)
lasso_mse_test2
mse_test

#Trying RIDGE Regression
ridge_model <- glmnet(x, df_soccer3$Points, alpha = 0)
summary(ridge_model)
cv_ridge <- cv.glmnet(x, df_soccer3$Points, alpha = 0)
best_ridge_lambda <-  cv_ridge$lambda.min

best_ridge_lambda
plot(cv_ridge)

best_model <- glmnet(x,df_soccer3$Points, alpha = 0, lambda = best_ridge_lambda )
coef(best_model)
plot(best_model, xvar = "lambda")
plot(ridge_model, xvar = "lambda")

y_predicted <-predict(ridge_model, s = best_ridge_lambda, newx = x)
ridge_mse <- mean((y_predicted - df_soccer3$Points)^2)
ridge_mse


#find SST and SSE
sst <- sum((df_soccer3$Points - mean(df_soccer3$Points))^2)
sse <- sum((y_predicted - df_soccer3$Points)^2)
sst
sse
#find R-Squared
rsq <- 1 - sse/sst
rsq

Mtrain.x <- x
train.x <- scale(train.x)
train.y <- 






#Model & CV *3* - removing range of covariates 
df_soccer5 <- subset(df_soccer3, select = -c(day_of_year, days_into, Date, HomeTeam, AwayTeam, weekday, HR, AR, HY, AY, HF, AF, watch_game))
model3 <-  lm(Points ~ ., data = df_soccer5)
predict_model3 <-  predict(model3, data = df_soccer5)
rmse_model3 <-  sqrt(mean((df_soccer5$Points - predict_model2)^2))
model3_cv10 <- cvFit(model3, data = df_soccer5, K=10, y=df_soccer5$Points, seed=1)
model3_cv10

#model 3a. WITH LASSO 
x <- data.matrix(df_soccer5[, c('HTHG', 'HTAG', 'HTR', 'HS', 'AS', 'HST', 'AST', 'HC' , 'AC')])
lasso <- glmnet(x, df_soccer5$Points)
lasso
predict(lasso, type = "coef", s = .00206)

### Use cross-validation to select the optimal lambda value
lasso_cv_model <- cv.glmnet(x, df_soccer5$Points, alpha = 1)
lambda <- lasso_cv_model$lambda.min

### Fit the model using the entire training set with the optimal lambda value
fit <- glmnet(x, df_soccer5$Points, alpha = 1, lambda = lambda)

### Evaluate the performance of the model on the test set
pred <- predict(fit, x)
mse <- mean((pred - df_soccer5$Points)^2)
mse

test_soccer5 <- subset(df_class_test, select = -c(Referee, id, day_of_year, days_into, Date, HomeTeam, AwayTeam, weekday, HR, AR, HY, AY, HF, AF, watch_game))
head(test_soccer5)
mse_test <- mean((pred - test_soccer5$Points)^2)
mse_test




#Classification Model
df_soccer_bin <- subset(df_soccer2, select = -c(Referee, id))
class_model = glm(formula = watch_game ~ ., family = "binomial", data = df_soccer_bin)
summary(class_model)
cv_class.model <- cv.glm(df_soccer_bin, class_model, K = 10)
cv_class.model




#removing data and columns in order to run ggpairs(train) since it takes forever to plot
df_soccer3_sub = sample(nrow(df_soccer3), size = nrow(df_soccer3)*.1) #creating a new subset of 10% of the training data so that we can easily run ggpairs 
df_soccer3_sub_in = df_soccer3[df_soccer3_sub, ] #10% subset of the training data  
#Removing a few categorical variables and then running ggpairs
df_soccer3_sub_in2 <- subset(df_soccer3_sub_in, select = -c(Date, HomeTeam, AwayTeam, HTR, id, day_of_year))#removing some of the categorical variables w/ many categories
#ggpairs(df_soccer3_sub_in2) #this takes a long time to plot
df_soccer3_sub_in3 <- subset(df_soccer3_sub_in, select = -c(Date, HomeTeam, AwayTeam, HTR, id, day_of_year, weekday, days_into))#removing some of the categorical variables w/ many categories
ggpairs(df_soccer3_sub_in3) #this takes a long time to plot
