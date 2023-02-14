library(tidyverse)
library(dplyr)
library(cvTools) 
library(GGally)
library (readr)
library(lubridate)

urlfile="https://raw.githubusercontent.com/jefehanson/MS-E-226-Project/main/premier%20soccer%20data.csv"

df_soccer_raw<-read_csv(url(urlfile))

df_soccer <- df_soccer_raw %>% select(-1, -24:-139) #removing division and all of the betting columns and naming it df_soccer2
df_soccer <- df_soccer[complete.cases(df_soccer), ]
df_soccer$Date <-  dmy(df_soccer$Date) #converting 'date' column from a character to a date
df_soccer$weekday <- weekdays(df_soccer$Date) #adding 'weekday' column to indicate date of week
df_soccer <- df_soccer %>% 
  mutate(Fun_Game = FTHG + FTAG, 
         fun_avg_difference = Fun_Game - mean(df_soccer$Fun_Game), 
         watch_game = ifelse(fun_avg_difference>0, "Watch", "Don't Watch"), 
         watch_game2 = ifelse(((FTHG + FTAG) - mean(FTHG + FTAG))>0, "Watch", "Don't Watch")
         )
view(df_soccer)


df_soccer <- df_soccer %>% 
  mutate(fun_avg_difference = Fun_Game - mean(df_soccer$Fun_Game))


mean(df_soccer$Fun_Game)


#Splitting data so that we have a 20% holdout for the end of class
set.seed(1) 
df_soccer$id <- 1:nrow(df_soccer) #adding a unique ID column
in.train <-  sample(nrow(df_soccer), size = nrow(df_soccer)*.8) #setting 80% of the data for the train
df_soccer2 <-  df_soccer[in.train, ] #this is the 80% we can work with until the end
df_class_test <-  df_soccer[-in.train, ] #setting aside the holdout data to test at the end



#Model & CV
model <-  lm(FTHG ~ ., data = df_soccer2)
summary(model)
predict_model <-  predict(model, data = df_soccer2)
rmse_model <-  sqrt(mean((df_soccer2$FTAG - predict_model)^2))
predict_test <- predict(model, newdata = df_class_test)
rmse_model_test <- sqrt(mean((df_class_test$FTAG - predict_test)^2))
model_cv10 <- cvFit(model, data = df_soccer2, K=10, y=df_soccer2$FTHG, seed=1)
model_cv10





#code for ggpairs; commented out b/c takes forever to run
#removing data and columns in order to run ggpairs(train) since it takes forever to plot
df_in.train_subset = sample(nrow(df_train3), size = nrow(df_train3)*.1) #creating a new subset of 10% of the training data so that we can easily run ggpairs 
df_train_subset = df_train3[df_in.train_subset, ] #10% subset of the training data  
df_train_subrest = df_train3[-df_in.train_subset, ] #remaining 90% subset of the training data
#Removing a few categorical variables and then running ggpairs
train_subset_removecol <- df_train_subset %>% select(-1, -2, -5)#removing some of the categorical variables w/ many categories
view(train_subset_removecol)
ggpairs(train_subset_removecol) #this takes a long time to plot

