
library(tidyverse)
library(MASS)
library(dplyr)
library(stargazer)
library(caret)
library(leaps)
library(ggplot2)
library(readr)
lego_sets <- read_csv("C:/Users/Hp/Downloads/archive (2)/lego_sets.csv")
View(lego_sets)

#descriptive analytics      
str(lego_sets)
summary(lego_sets)

#histogram 
hist(lego_sets$list_price)
boxplot(lego_sets$list_price,width = 0.7)
boxplot(log(lego_sets$list_price))


#correlations 
numeric_data <- lego_sets[, sapply(lego_sets, is.numeric)]
cor_matrix <- cor(numeric_data)
print(cor_matrix)        
cor(lego_sets$list_price,lego_sets$piece_count)

#first_model
attach(lego_sets)
fm<-lm(list_price~piece_count)
summary(fm)

sm<-lm(list_price~prod_id+piece_count+play_star_rating+num_reviews+review_difficulty+star_rating+country)
#sm <- lm(list_price ~ ., data = lego_sets)
summary(sm)

#diagnostic 

fmResids <- fm$residuals
fmlFitted <- fm$fitted.value

plot(fmlFitted,fmResids)
dev.new(width = 10, height = 8)
par(mar = c(5, 5, 2, 2))


smResids <- sm$residuals
smlFitted <- sm$fitted.value
plot(smlFitted,smResids)
qqnorm(fmResids)
qqnorm(smResids)

#extension 
summary(sm)
smp<-lm(list_price~prod_id+I(prod_id^2)+piece_count+prod_id:play_star_rating+play_star_rating+num_reviews+review_difficulty+star_rating+country)

summary(smp)

#Feature_selection
step<-stepAIC(smp,direction= "forward",trace=FALSE)
step$anova

step1<-stepAIC(smp,direction= "backward",trace=FALSE)
step1$anova

smnp<-lm(list_price~prod_id+piece_count+prod_id:play_star_rating+play_star_rating+review_difficulty+star_rating+country)
summary(smnp)


summary(fm)


#prediction_model

piece_count_predictions <- data.frame(piece_count = c(10, 20, 30))
predict(fm,piece_count_predictions)

fm<-lm(list_price~piece_count)
piece_count_predictions <- data.frame(piece_count = c(10, 20, 30))

prediction_interval <- predict(fm, newdata = piece_count_predictions, interval = "confidence", level = 0.95)
prediction_interval

