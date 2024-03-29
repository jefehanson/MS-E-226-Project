library(tidyverse)
library(dplyr)
library(cvTools) 
library(GGally)
library(readr)
library(boot)
library(pROC)
library(ggplot2)
library(glmnet)
library(caret)



#####################################
#DATA IMPORT AND TRANSFORMATION
urlfile="https://raw.githubusercontent.com/jefehanson/MS-E-226-Project/main/premier%20soccer%20data.csv"

df_soccer_raw<-read_csv(url(urlfile))


df_soccer <- df_soccer_raw %>% select(-1, -24:-139) #removing division and all of the betting columns and naming it df_soccer2
df_soccer <- df_soccer[complete.cases(df_soccer), ]
library(lubridate)
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


#####################################
#DATA SPLITTING
#80% train data; 20% holdout data
set.seed(1) 
df_soccer$id <- 1:nrow(df_soccer) #adding a unique ID column
in.train <-  sample(nrow(df_soccer), size = nrow(df_soccer)*.8) #setting 80% of the data for the train
df_soccer2 <-  df_soccer[in.train, ] #this is the 80% we can work with until the end
df_soccer2_test <-  df_soccer[-in.train, ] #setting aside the holdout data to 

#################
#Replicating manipulations on test set
df_soccer3_test <- subset(df_soccer2_test, select = -c(Referee, id))
head(df_soccer3_test)
df_soccer3_num_test <- subset(df_soccer3_test, select = -c(Date, HomeTeam, AwayTeam, HTR, weekday))


#####################################
#REGRESSION MODELS

#Removing "Referee" and ID so cvFit will work
df_soccer3 <- subset(df_soccer2, select = -c(Referee, id))
head(df_soccer3)
df_soccer3_num <- subset(df_soccer3, select = -c(Date, HomeTeam, AwayTeam, HTR, weekday))#used for correlation matrix
view(df_soccer3)
#Model & CV  *1*
model <-  lm(Points ~ ., data = df_soccer3)
model
summary(model)
coef(model)

predict_model <-  predict(model, data = df_soccer3)
rmse_allvarLM <-  sqrt(mean((df_soccer3$Points - predict_model)^2))
cv10model <- cvFit(model, data = df_soccer3, K=10, y=df_soccer3$Points, seed=1)
cv10model
rmse_allvarLM_cv10 <- 0.9720125
cv5model <- cvFit(model, data = df_soccer3, K=5, y=df_soccer3$Points, seed=1)
cv100model <- cvFit(model, data = df_soccer3, K=100, y=df_soccer3$Points, seed=1)

#evaluating variance of allvarLM
var(model$residuals)
allVarLM_residuals <- residuals(model)
library(moments)

skewness(allVarLM_residuals)
allVarLM_skewness <- skewness(allVarLM_residuals)
allVarLM_skewness

qqnorm(allVarLM_residuals, col = rgb(red = 0, green = 0, blue = 1, alpha = .1), pch = 1)
qqline(allVarLM_residuals)
par(mar = c(5, 5, 4, 2) + 0.1, cex.lab = 1.2, cex.axis = 1.2)

#Visualizing AllVarLM

##Plots evaluating variance
plot(model$fitted.values, model$residuals)
ggplot(data.frame(x = model$fitted.values, resid = model$residuals), aes(x, resid)) +
  geom_point() +
  stat_smooth(method = "loess")

##Plots evaluating the correlation on the diagonal
library(corrplot)
cor_matrix <- cor(df_soccer3_num)
order <- order(abs(cor_matrix[, "Points"]), decreasing = TRUE)
corrplot(cor_matrix[order, order], method = "color", type = "upper", tl.cex = 0.7, number.cex = 0.7)

cor_matrix <- cor(df_soccer3_num)
order <- order(abs(cor_matrix[, "watch_game"]), decreasing = TRUE)
corrplot(cor_matrix[order, order], method = "color", type = "upper", tl.cex = 0.7, number.cex = 0.7)


#Histogram of residuals of allvarLM
hist(allVarLM_residuals, main = "Histogram of allvarLM Residuals", xlab = "Residuals", breaks = 100, 
     col = "blue", border = "white", ylab = "Frequency")

ggplot(df_soccer3) + 
  geom_point(mapping = aes(x = HS, y = Points))



#model 1a. LASSO 
x <- data.matrix(df_soccer3[, c('HTHG', 'HTAG', 'HTR', 'HS', 'AS', 'HST', 'AST', 'HC' , 'AC', 'HY', 'AY', 'HR', 'AR')])
lasso <- glmnet(x, df_soccer3$Points)

### Use cross-validation to select the optimal lambda value
lasso_cv_model <- cv.glmnet(x, df_soccer3$Points, alpha = 1)
plot(lasso_cv_model)
lambda <- lasso_cv_model$lambda.min
lambda

### Fit the model using the entire training set with the optimal lambda value
fit <- glmnet(x, df_soccer3$Points, alpha = 1, lambda = lambda)

### Evaluate the performance of the model on the test set
lasso_pred <- predict(fit, x)
mse_lasso <- mean((lasso_pred - df_soccer3$Points)^2)
rmse_lasso_cv10 <- sqrt(mean((lasso_pred - df_soccer3$Points)^2))
rmse_lasso_cv10
mse_lasso
sst <- sum((df_soccer3$Points - mean(df_soccer3$Points))^2)
sse_lasso <- sum((lasso_pred - df_soccer3$Points)^2)
sst
sse_lasso
#find R-Squared
rsq_lasso <- 1 - sse_lasso/sst
rsq_lasso

#Lasso Coefficients: 
lM_LASSO <- cv.glmnet(x, df_soccer3$Points, alpha = 1)
opt_lam <- lM_LASSO$lambda.min
opt_lam
lM_LASSO <- glmnet(x_test, df_soccer3_test$Points,
                   intercept=TRUE, alpha=1, lambda = opt_lam)
W <- as.matrix(coef(lasso_cv_model))
W

keep_X <- rownames(W)[W!=0]
keep_X
keep_X <- keep_X[!keep_X == "(Intercept)"]
keep_X
x_train <- x[,keep_X]
summary(lm(df_soccer3$Points ~ x_train))

####################
### evaluate *TEST* set using lasso & Lasso_cv10

df_soccer3_test <- na.omit(df_soccer3_test)
x_test <- data.matrix(df_soccer3_test[, c('HTHG', 'HTAG', 'HTR', 'HS', 'AS', 'HST', 'AST', 'HC' , 'AC', 'HY', 'AY', 'HR', 'AR')])
lasso_test <- glmnet(x_test, df_soccer3_test$Points, alpha = 1)
lasso_test
summary(lasso_test)
lasso_test_coefs <- coef(lasso_test, s = lambda_test)
print(lasso_test_coefs)

###alternate:https://stats.stackexchange.com/questions/410173/lasso-regression-p-values-and-coefficients
###TEST COEFFICIENTS
df_soccer3_test <- na.omit(df_soccer3_test)
nrow(df_soccer3_test)
x_test <- data.matrix(
  df_soccer3_test[, 
                  c('HTHG', 'HTAG', 'HTR', 'HS', 
                    'AS', 'HST', 'AST', 'HC' , 'AC', 
                    'HY', 'AY', 'HR', 'AR')])
lM_LASSO <- cv.glmnet(x_test, df_soccer3_test$Points,
                      intercept=TRUE, alpha=1, nfolds=nrow(ds),
                      parallel = T)
opt_lam <- lM_LASSO$lambda.min
lM_LASSO <- glmnet(x_test, df_soccer3_test$Points,
                   intercept=TRUE, alpha=1, lambda = opt_lam)
W <- as.matrix(coef(lM_LASSO))
W

keep_X <- rownames(W)[W!=0]
keep_X
keep_X <- keep_X[!keep_X == "(Intercept)"]
keep_X
x_test
x_test <- x_test[,keep_X]
summary(lm(df_soccer3_test$Points ~ x_test))




### Use cross-validation to select the optimal lambda value
lasso_cv_model_test <- cv.glmnet(x_test, df_soccer3_test$Points, alpha = 1)
lasso_cv_model_test
plot(lasso_cv_model_test)
lambda_test <- lasso_cv_model_test$lambda.min
lambda_test


### Fit the model using the entire training set with the optimal lambda value
fit_test <- glmnet(x_test, df_soccer3_test$Points, alpha = 1, lambda = lambda_test)

lasso_pred_test <- predict(fit_test, x_test)
summary(lasso_pred_test)
mse_lasso_test <- mean((lasso_pred_test - df_soccer3_test$Points)^2)
rmse_lasso_cv10_test <- sqrt(mean((lasso_pred_test - df_soccer3_test$Points)^2))
rmse_lasso_cv10_test
summary(rmse_lasso_cv10_test)
mse_lasso_test
sst_test <- sum((df_soccer3_test$Points - mean(df_soccer3_test$Points))^2)
sse_lasso_test <- sum((lasso_pred_test - df_soccer3_test$Points)^2)
sst_test
sse_lasso_test
#find R-Squared
rsq_lasso_test <- 1 - sse_lasso_test/sst_test
rsq_lasso_test

################################## END TEST 

#model 1b. RIDGE REGRESSION
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

ridge_pred <-predict(ridge_model, s = best_ridge_lambda, newx = x)
mse_ridge <- mean((ridge_pred - df_soccer3$Points)^2)
rmse_ridge <- sqrt(mean((ridge_pred - df_soccer3$Points)^2))
rmse_ridge
ridge_mse


#find SST and SSE
sst <- sum((df_soccer3$Points - mean(df_soccer3$Points))^2)
sse <- sum((ridge_pred - df_soccer3$Points)^2)
sst
sse
#find R-Squared
rsq <- 1 - sse/sst
rsq

#Model & CV *3* - removing range of covariates 
df_soccer5 <- subset(df_soccer3, select = -c(day_of_year, days_into, Date, HomeTeam, AwayTeam, weekday, HR, AR, HY, AY, HF, AF, watch_game))
model3 <-  lm(Points ~ ., data = df_soccer5)
predict_model3 <-  predict(model3, data = df_soccer5)
rmse_lessvarLM <-  sqrt(mean((df_soccer5$Points - predict_model3)^2))
model3_cv10 <- cvFit(model3, data = df_soccer5, K=10, y=df_soccer5$Points, seed=1)
model3_cv10
rmse_lessvarLM_cv10 <- 0.9837787

#model 3a. WITH LASSO 
x2 <- data.matrix(df_soccer5[, c('HTHG', 'HTAG', 'HTR', 'HS', 'AS', 'HST', 'AST', 'HC' , 'AC')])
lasso <- glmnet(x2, df_soccer5$Points)
lasso
predict(lasso, type = "coef", s = .00206)

### Use cross-validation to select the optimal lambda value
lasso_cv_model <- cv.glmnet(x2, df_soccer5$Points, alpha = 1)
lambda <- lasso_cv_model$lambda.min

### Fit the model using the entire training set with the optimal lambda value
fit <- glmnet(x2, df_soccer5$Points, alpha = 1, lambda = lambda)

### Evaluate the performance of the model on the test set
pred <- predict(fit, x2)
mse <- mean((pred - df_soccer5$Points)^2)
mse

test_soccer5 <- subset(df_class_test, select = -c(Referee, id, day_of_year, days_into, Date, HomeTeam, AwayTeam, weekday, HR, AR, HY, AY, HF, AF, watch_game))
head(test_soccer5)
mse_test <- mean((pred - test_soccer5$Points)^2)
mse_test



#####################################
#CLASSIFICATION MODELS

# Binary Classification Model
# Predicting the value of "watch_game", 0 or 1: Will this game have an above-average number of goals scored?

#Removing "Referee" and ID (so cvFit will work)
df_soccer_bin <- subset(df_soccer2, select = -c(Referee, id))
x_bin <- data.matrix(df_soccer_bin[, c('Points', 'HTHG', 'HTAG', 'HTR', 'HS', 'AS', 'HST', 'AST', 'HC' , 'AC', 'HY', 'AY', 'HR', 'AR', 'days_into')])

df_soccer_bin_test <- subset(df_soccer2_test, select = -c(Referee, id))
x_bin_test <- data.matrix(df_soccer_bin_test[, c('Points', 'HTHG', 'HTAG', 'HTR', 'HS', 'AS', 'HST', 'AST', 'HC' , 'AC', 'HY', 'AY', 'HR', 'AR', 'days_into')])


##### Other Models #####

# B: baseline model (all covariates)
set.seed(1)
bin_baseline = glm(formula = watch_game ~ ., family = "binomial", data = df_soccer_bin)
# summary(bin_baseline)
cv_bin_baseline <- cv.glm(df_soccer_bin, bin_baseline, K = 10)
(bin_error_b <- cv_bin_baseline$delta[1])

pred_b <- predict(bin_baseline, newx=x_bin, type="response")
roc_b <- roc(df_soccer_bin$watch_game, pred_b)
auc_b <- auc(roc_b)
auc_b
ggroc(roc_b)

t <- 0.5 # our classification threshold
class_b <- ifelse(pred_b > t, 1, 0)
confusionMatrix(factor(class_b), factor(df_soccer_bin$watch_game))
sens_b <- sensitivity(factor(class_b), factor(df_soccer_bin$watch_game))

# 1: Select only the covariates marked *** in ggpairs()
#set.seed(1)
bin_model_1 = glm(formula = watch_game ~ Points + HTHG + HTAG + HS + AS + HST + AST, family = "binomial", data = df_soccer_bin)
cv_bin_model_1 <- cv.glm(df_soccer_bin, bin_model_1, K = 10)
(bin_error_1 <- cv_bin_model_1$delta[1])

pred_1 <- predict(bin_model_1, newx=x_bin, type="response")
roc_1 <- roc(df_soccer_bin$watch_game, pred_1)
auc_1 <- auc(roc_1)
auc_1
ggroc(roc_1)

t <- 0.5 # our classification threshold
class_1 <- ifelse(pred_1 > t, 1, 0)
confusionMatrix(factor(class_1), factor(df_soccer_bin$watch_game))
sens_1 <- sensitivity(factor(class_1), factor(df_soccer_bin$watch_game))




### 2: Ridge regression
bin_model_2 <- glmnet(x_bin, df_soccer_bin$watch_game, family="binomial", alpha=0)

# find the optimal lambda
cv_bin_model_2 <- cv.glmnet(x_bin, df_soccer_bin$watch_game, nfolds=10)
summary(cv_bin_model_2)
ridge_lambda <- cv_bin_model_2$lambda.min

# use the optimal lambda to build a new model using ridge regression
bin_ridge_model_opt_lambda <- glmnet(x_bin, df_soccer_bin$watch_game, family="binomial", alpha=0, lambda=ridge_lambda)

# find the AUC error of the ridge regression model
pred_ridge <- predict(bin_ridge_model_opt_lambda, newx=x_bin, type="response")
roc_ridge <- roc(df_soccer_bin$watch_game, pred_ridge)
auc_ridge <- auc(roc_ridge)
auc_ridge
ggroc(roc_ridge)

# create the confusion matrix
t <- 0.5 # our classification threshold
class_ridge <- ifelse(pred_ridge > t, 1, 0)
confusionMatrix(factor(class_ridge), factor(df_soccer_bin$watch_game))
sens_ridge <- sensitivity(factor(class_ridge), factor(df_soccer_bin$watch_game))

######
### Lasso Regression

# find the optimal lambda
cv_bin_model_3 <- cv.glmnet(x_bin, df_soccer_bin$watch_game, nfolds=10)
lasso_lambda <- cv_bin_model_3$lambda.min

# use the optimal lambda to build a new model using lasso regression
bin_lasso_model <- glmnet(x_bin, df_soccer_bin$watch_game, family="binomial", alpha=1, lambda=lasso_lambda)

# find the AUC error of the lasso regression model
pred_lasso <- predict(bin_lasso_model, newx=x_bin, type="response")
roc_lasso <- roc(df_soccer_bin$watch_game, pred_lasso)
auc_lasso <- auc(roc_lasso)
auc_lasso
ggroc(roc_lasso) +
  labs(title="ROC of Lasso Model", "subtitle"="AUC = 0.7781")
legend("bottomright", title="ROC of Lasso Model", legend="Receiver Operating Curve")

# create the confusion matrix
t <- 0.5 # our classification threshold
class_lasso <- ifelse(pred_lasso > t, 1, 0)
confusionMatrix(factor(class_lasso), factor(df_soccer_bin$watch_game))
sens_lasso <- sensitivity(factor(class_lasso), factor(df_soccer_bin$watch_game))

## TEST BEST MODEL ON TEST DATA

#x_test_bin <- data.matrix( df_soccer3_test[, c('Points', 'HTHG', 'HTAG', 'HTR', 'HS', 'AS', 'HST', 'AST', 'HC' , 'AC', 'HY', 'AY', 'HR', 'AR', 'days_into')])
pred_lasso_test <- predict(bin_lasso_model, newx=x_bin_test, type="response")
roc_lasso_test <- roc(df_soccer_bin_test$watch_game, pred_lasso_test)
auc_lasso_test <- auc(roc_lasso_test)
auc_lasso_test
ggroc(roc_lasso)

t <- 0.5 # our classification threshold
class_lasso_test <- ifelse(pred_lasso_test > t, 1, 0)
confusionMatrix(factor(class_lasso_test), factor(df_soccer_bin_test$watch_game))
sens_lasso_test <- sensitivity(factor(class_lasso_test), factor(df_soccer_bin_test$watch_game))




















#####################################
#GRAVEYARD / OLD

#removing data and columns in order to run ggpairs(train) since it takes forever to plot
df_soccer3_sub = sample(nrow(df_soccer3), size = nrow(df_soccer3)*.1) #creating a new subset of 10% of the training data so that we can easily run ggpairs 
df_soccer3_sub_in = df_soccer3[df_soccer3_sub, ] #10% subset of the training data  
#Removing a few categorical variables and then running ggpairs
df_soccer3_sub_in2 <- subset(df_soccer3_sub_in, select = -c(Date, HomeTeam, AwayTeam, HTR, id, day_of_year))#removing some of the categorical variables w/ many categories
#ggpairs(df_soccer3_sub_in2) #this takes a long time to plot
df_soccer3_sub_in3 <- subset(df_soccer3_sub_in, select = -c(Date, HomeTeam, AwayTeam, HTR, id, day_of_year, weekday, days_into))#removing some of the categorical variables w/ many categories
ggpairs(df_soccer3_sub_in3) #this takes a long time to plot
