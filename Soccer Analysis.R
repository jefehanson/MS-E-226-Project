library(tidyverse)
library(dplyr)
library(cvTools) 
library(GGally)
library (readr)

urlfile="https://raw.githubusercontent.com/jefehanson/MS-E-226-Project/main/premier%20soccer%20data.csv"

df_soccer_raw<-read_csv(url(urlfile))

df_soccer <- df_soccer_raw %>% select(-1, -24:-139) #removing division and all of the betting columns and naming it df_soccer2
df_soccer <- df_soccer[complete.cases(df_soccer), ]

#Splitting data so that we have a 20% holdout for the end of class
set.seed(1) 
df_soccer$id <- 1:nrow(df_soccer) #adding a unique ID column
in.train <-  sample(nrow(df_soccer), size = nrow(df_soccer)*.8) #setting 80% of the data for the train
df_soccer2 <-  df_soccer[in.train, ] #this is the 80% we can work with until the end
df_class_test <-  df_soccer[-in.train, ] #setting aside the holdout data to test at the end

#Creating train and test data based on the 80% we can use
set.seed(1)
in.train2 = sample(nrow(df_soccer2), size=nrow(df_soccer2)*.8)
df_train = df_soccer2[in.train2, ]
df_test = df_soccer2[-in.train2, ]

df_train2 <- df_train %>%  select(-1, -10) #had to remove dates and referees, they were giving issues in the models and predicts
df_test2 <- df_test %>% select(-1, -10)

model <-  lm(FTAG ~ ., data = df_train2)
predict_model <-  predict(model, data = df_train2)
rmse_model <-  sqrt(mean((df_train2$FTAG - predict_model)^2))
predict_test <- predict(model, newdata = df_test2)
rmse_model_test <- sqrt(mean((df_test2$FTAG - predict_test)^2))
model_cv10 <- cvFit(model, data = df_train2, K=10, y=df_train2$FTAG, seed=1)
model_cv10



##MUTATE AND TEST ON MUTATES; CREATING DIFFERENCE (HOME-AWAY) VARIABLES

df_soccer3 <- df_soccer2 %>% 
  mutate(FTG_Dif = FTHG - FTAG, 
         HTG_Dif = HTHG - HTAG, 
         Shot_Dif = HS - AS, 
         ShotT_Dif = HST - AST, 
         Corner_Dif =HC - AC, 
         Foul_Dif =HF - AF, 
         Yellow_Dif =HY - AY, 
         Red_Dif =HR - AR)
df_soccer3 <- df_soccer3 %>%  select(-1, -4, -5, -7, -8, -10, -11, -12, -13,
                                     -14, -15, -16, -17, -18, -19, -20, -21, -22)

#Creating train and test data based on the 80% we can use
set.seed(1)
in.train3 = sample(nrow(df_soccer3), size=nrow(df_soccer3)*.8)
df_train3 = df_soccer3[in.train3, ]
df_test3 = df_soccer3[-in.train3, ]

model <-  lm(FTG_Dif ~ ., data = df_train3)
predict_model <-  predict(model, data = df_train3)
rmse_model2 <-  sqrt(mean((df_train3$FTG_Dif - predict_model)^2))
predict_test <- predict(model, newdata = df_test3)
rmse_model_test2 <- sqrt(mean((df_test3$FTG_Dif - predict_test)^2))
model_cv10 <- cvFit(model, data = df_train3, K=10, y=df_train3$FTG_Dif, seed=1)





#code for ggpairs; commented out b/c takes forever to run
"""
#removing data and columns in order to run ggpairs(train) since it takes forever to plot
df_in.train_subset = sample(nrow(df_soccer2_train), size = nrow(df_soccer2_train)*.1) #creating a new subset of 10% of the training data so that we can easily run ggpairs 
df_train_subset = df_soccer2_train[df_in.train_subset, ] #10% subset of the training data  
df_train_subrest = df_soccer2_train[-df_in.train_subset, ] #remaining 90% subset of the training data
#Removing a few categorical variables and then running ggpairs
train_subset_removecol <- df_train_subset %>% select(-1, -2, -3, -4, -11)#removing some of the categorical variables w/ many categories
#ggpairs(train_subset_removecol) #this takes a long time to plot
"""
