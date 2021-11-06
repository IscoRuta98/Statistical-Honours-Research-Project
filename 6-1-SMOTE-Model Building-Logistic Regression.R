rm(list = ls())
setwd("~/Honours/THESIS/StatsBomb/Thesis Code")

library(StatsBombR)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(gbm)
library(ranger)
library(randomForest)
library(caret)
library(DMwR)

# Load  Women Shots & FreezeFrame Data
load("~/Honours/THESIS/StatsBomb/Thesis Code/Data for Modelling")

# Remove non numeric/factor variables
rm_vars = c("id","player.name","team.name")
shots.varsdata = shots.varsdata[,!(names(shots.varsdata) %in% rm_vars)]

# Change factor variables into numeric values

shots.varsdata$play_pattern.name = as.numeric(shots.varsdata$play_pattern.name)   - 1
shots.varsdata$shot.body_part.name = as.numeric(shots.varsdata$shot.body_part.name) - 1
shots.varsdata$shot.type.name = as.numeric(shots.varsdata$shot.type.name) - 1
shots.varsdata$shot.first_time = as.numeric(shots.varsdata$shot.first_time) - 1
shots.varsdata$shot.technique.name = as.numeric(shots.varsdata$shot.technique.name) - 1
shots.varsdata$shot.one_on_one = as.numeric(shots.varsdata$shot.one_on_one) - 1
shots.varsdata$position.name.2 = as.numeric(shots.varsdata$position.name.2) - 1
shots.varsdata$under_pressure = as.numeric(shots.varsdata$under_pressure) - 1

#### Split Data into Train & Test ####
set.seed(2020)
index = sample(1:nrow(shots.varsdata),size = 0.8*nrow(shots.varsdata),replace = F)

train_shots = shots.varsdata[index,]
test_shots  = shots.varsdata[-index,]

# Change target variable into a factor
train_shots = as.data.frame(train_shots)

train_shots$is.goal = as.factor(train_shots$is.goal)

set.seed(2020)
train_shots = SMOTE(is.goal~.,train_shots,perc.over = 60,perc.under = 500)
prop.table(table(train_shots$is.goal))

#### BASIC MODEL ####

basic.log = glm(formula = is.goal~location.x+location.y+DistToKeeper+DistToGoal+
                  AngleToKeeper+AngleToGoal+shot.body_part.name+shot.type.name,
                data = train_shots,family = 'binomial')


pred.basic.log = predict(object = basic.log,newdata = test_shots,type = "response")

# Brier Score
round(sum((test_shots$is.goal- pred.basic.log)^2) / nrow(test_shots),5) # 0.14042

# AUC ROC score
round(AUC(y_pred = pred.basic.log,y_true = test_shots$is.goal),5) # 0.74936


#### COMPLEX MODEL ####

complex.log = glm(formula = is.goal~location.x+location.y+DistToKeeper+DistToGoal+
                    AngleToKeeper+AngleToGoal+shot.body_part.name+shot.type.name+
                    shot.technique.name+play_pattern.name+TimeInPoss+under_pressure+
                    avevelocity+position.name.2+shot.first_time+shot.one_on_one,
                  data = train_shots,family = 'binomial')

pred.complex.log = predict(object = complex.log,newdata = test_shots,type = "response")

# Brier Score
round(sum((test_shots$is.goal- pred.complex.log)^2) / nrow(test_shots),5) # 0.14016

# AUC ROC score
round(AUC(y_pred = pred.complex.log,y_true = test_shots$is.goal),5) # 0.74263


#### SPATIO-TEMPORAL MODEL ####

spat_log = glm(formula = is.goal~.-shot.statsbomb_xg,data = train_shots,
               family = 'binomial')

pred.spat.log = predict(object = spat_log,newdata = test_shots,type = "response")

# Brier Score
round(sum((test_shots$is.goal- pred.spat.log)^2) / nrow(test_shots),5) # 0.14124

# AUC ROC score
round(AUC(y_pred = pred.spat.log,y_true = test_shots$is.goal),5) # 0.75809


# Save the predictions
log_predictions = data.frame(pred.basic.log,pred.complex.log,pred.spat.log)
colnames(log_predictions) = c("Logistic Regression Basic Model Predictions",
                              "Logistic Regression Complex Model Predictions",
                              "Logistic Regression Spatial-Temporal Model Predictions")
