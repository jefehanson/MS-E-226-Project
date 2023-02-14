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
df_soccer$day_of_year <- yday(df_soccer$Date)

count_days <- table(df_soccer$day_of_year)
df_count_days <- as.data.frame(count_days)#the first spike in games is at day 207
ggplot(data = df_count_days, mapping = aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") #wanted to visualize it to confirm

#calculating how far into season (days above 207)
df_soccer <- df_soccer %>% 
  mutate(day_of_season = ifelse(day_of_year > 207, day_of_year - 207, day_of_year +(366-207)))

# 'Exciting' vs 'Boring': is total game goals higher than avg total game goals? yes=exciting, no=boring
df_soccer <- df_soccer %>% 
  mutate(watch_game = ifelse(
    ((FTHG + FTAG) - mean(FTHG + FTAG))>0, "Exciting", "Boring"))  
#calc no. of points earned each game and replacing "FTR" with points
df_soccer$FTR <- ifelse(df_soccer$FTR == "A", 0,
                        ifelse(df_soccer$FTR == "D", 1,
                               ifelse(df_soccer$FTR == "H", 3, NA)
                        ))
names(df_soccer)[names(df_soccer) == "FTR"] <- "Points" #renaming FTR to #Points
view(df_soccer)

#Splitting data so that we have a 20% holdout for the end of class
set.seed(1) 
df_soccer$id <- 1:nrow(df_soccer) #adding a unique ID column
in.train <-  sample(nrow(df_soccer), size = nrow(df_soccer)*.8) #setting 80% of the data for the train
df_soccer2 <-  df_soccer[in.train, ] #this is the 80% we can work with until the end
df_class_test <-  df_soccer[-in.train, ] #setting aside the holdout data to 

#Removing the categorical outcome variable (and "Referee" so cvFit will work)
df_soccer3 <- subset(df_soccer2, select = -c(watch_game, Referee))

#Model & CV
model <-  lm(Points ~ ., data = df_soccer3)
summary(model)
predict_model <-  predict(model, data = df_soccer3)
rmse_model <-  sqrt(mean((df_soccer3$Points - predict_model)^2))

#Comparing holdout vs Train and CV not working b/c referee
predict_test <- predict(model, newdata = df_class_test)
rmse_model_test <- sqrt(mean((df_class_test$Points - predict_test)^2))
model_cv10 <- cvFit(model, data = df_soccer3, K=10, y=df_soccer3$Points, seed=1)
model_cv10

#removing data and columns in order to run ggpairs(train) since it takes forever to plot
df_soccer3_sub = sample(nrow(df_soccer3), size = nrow(df_soccer3)*.1) #creating a new subset of 10% of the training data so that we can easily run ggpairs 
df_soccer3_sub_in = df_soccer3[df_soccer3_sub, ] #10% subset of the training data  
#Removing a few categorical variables and then running ggpairs
df_soccer3_sub_in2 <- subset(df_soccer3_sub_in, select = -c(Date, HomeTeam, AwayTeam, HTR, id, day_of_year))#removing some of the categorical variables w/ many categories
#ggpairs(df_soccer3_sub_in2) #this takes a long time to plot
