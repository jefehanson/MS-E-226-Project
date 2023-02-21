library(tidyverse)
library(dplyr)
library(cvTools) 
library(GGally)
library (readr)
library(lubridate)
install.packages("boot")
library(boot)

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


#Splitting data so that we have a 20% holdout for the end of class
set.seed(1) 
df_soccer$id <- 1:nrow(df_soccer) #adding a unique ID column
in.train <-  sample(nrow(df_soccer), size = nrow(df_soccer)*.8) #setting 80% of the data for the train
df_soccer2 <-  df_soccer[in.train, ] #this is the 80% we can work with until the end
df_class_test <-  df_soccer[-in.train, ] #setting aside the holdout data to 

#Removing the categorical outcome variable (and "Referee" so cvFit will work)
df_soccer3 <- subset(df_soccer2, select = -c(watch_game, Referee, id))

#Model & CV  *1*
model <-  lm(Points ~ ., data = df_soccer3)
summary(model)

predict_model <-  predict(model, data = df_soccer3)
rmse_model <-  sqrt(mean((df_soccer3$Points - predict_model)^2))
model_cv10 <- cvFit(model, data = df_soccer3, K=10, y=df_soccer3$Points, seed=1)
model_cv10


#Model & CV *2*
##removing low impact covariates to see how it changes model's accuracy: (day_of_year, days_int-, id, )
df_soccer4 <- subset(df_soccer3, select = -c(day_of_year, days_into, Date))
model2 <-  lm(Points ~ ., data = df_soccer4)
predict_model2 <-  predict(model2, data = df_soccer4)
rmse_model2 <-  sqrt(mean((df_soccer4$Points - predict_model2)^2))
model2_cv10 <- cvFit(model2, data = df_soccer4, K=10, y=df_soccer4$Points, seed=1)
model2_cv10
##performed slightly better, but not a big impact


#Model & CV *3*
##removing low impact covariates to see how it changes model's accuracy: (day_of_year, days_int-, id, )
df_soccer5 <- subset(df_soccer4, select = -c(HomeTeam, AwayTeam, weekday))
model3 <-  lm(Points ~ ., data = df_soccer5)
predict_model3 <-  predict(model3, data = df_soccer5)
rmse_model3 <-  sqrt(mean((df_soccer5$Points - predict_model2)^2))
model3_cv10 <- cvFit(model3, data = df_soccer5, K=10, y=df_soccer5$Points, seed=1)
model3_cv10

#Classification Model
view(df_soccer2)
df_soccer_bin <- subset(df_soccer2, select = -c(Referee, id, FTAG, FTHG))
class_model = glm(formula = watch_game ~ ., family = "binomial", data = df_soccer_bin)
summary(class_model)
cv_class.model <- cv.glm(df_soccer_bin, class_model, K = 10)



#removing data and columns in order to run ggpairs(train) since it takes forever to plot
df_soccer3_sub = sample(nrow(df_soccer3), size = nrow(df_soccer3)*.1) #creating a new subset of 10% of the training data so that we can easily run ggpairs 
df_soccer3_sub_in = df_soccer3[df_soccer3_sub, ] #10% subset of the training data  
#Removing a few categorical variables and then running ggpairs
df_soccer3_sub_in2 <- subset(df_soccer3_sub_in, select = -c(Date, HomeTeam, AwayTeam, HTR, id, day_of_year))#removing some of the categorical variables w/ many categories
#ggpairs(df_soccer3_sub_in2) #this takes a long time to plot
df_soccer3_sub_in3 <- subset(df_soccer3_sub_in, select = -c(Date, HomeTeam, AwayTeam, HTR, id, day_of_year, weekday, days_into))#removing some of the categorical variables w/ many categories
ggpairs(df_soccer3_sub_in3) #this takes a long time to plot
