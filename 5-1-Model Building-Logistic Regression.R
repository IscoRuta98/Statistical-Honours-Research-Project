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

# Load  Women Shots & FreezeFrame Data
load("~/Honours/THESIS/StatsBomb/Thesis Code/Data for Modelling")

# Remove non numeric/factor variables
rm_vars = c("id","player.name","team.name")
shots.varsdata = shots.varsdata[,!(names(shots.varsdata) %in% rm_vars)]

# shots.varsdata$is.goal = as.factor(shots.varsdata$is.goal)

#### Split Data into Train & Test ####
set.seed(2020)
index = sample(1:nrow(shots.varsdata),size = 0.8*nrow(shots.varsdata),replace = F)

train_shots = shots.varsdata[index,]
test_shots  = shots.varsdata[-index,]


#### BASIC MODEL ####

basic.log = glm(formula = is.goal~location.x+location.y+DistToKeeper+DistToGoal+
                    AngleToKeeper+AngleToGoal+shot.body_part.name+shot.type.name,
                  data = train_shots,family = 'binomial')

summary(basic.log)
stargazer(basic.log,type = "text",out = "basic_logistic_model.html")

pred.basic.log = predict(object = basic.log,newdata = test_shots,type = "response")

# Brier Score
round(sum((test_shots$is.goal- pred.basic.log)^2) / nrow(test_shots),5) # 0.08442

# AUC ROC score
round(AUC(y_pred = pred.basic.log,y_true = test_shots$is.goal),5)


#### COMPLEX MODEL ####

complex.log = glm(formula = is.goal~location.x+location.y+DistToKeeper+DistToGoal+
                  AngleToKeeper+AngleToGoal+shot.body_part.name+shot.type.name+
                  shot.technique.name+play_pattern.name+TimeInPoss+under_pressure+
                  avevelocity+position.name.2+shot.first_time+shot.one_on_one,
                  data = train_shots,family = 'binomial')

summary(complex.log)
stargazer(complex.log,type = "text",out = "complex_logistic_model.html")

pred.complex.log = predict(object = complex.log,newdata = test_shots,type = "response")

# Brier Score
round(sum((test_shots$is.goal- pred.complex.log)^2) / nrow(test_shots),5) # 0.0844

# AUC ROC score
round(AUC(y_pred = pred.complex.log,y_true = test_shots$is.goal),5)


#### SPATIO-TEMPORAL MODEL ####

spat_log = glm(formula = is.goal~.-shot.statsbomb_xg,data = train_shots,
               family = 'binomial')

summary(spat_log)
stargazer(spat_log,type = "text",out = "spat_full_model.html")

pred.spat.log = predict(object = spat_log,newdata = test_shots,type = "response")

# Brier Score
round(sum((test_shots$is.goal- pred.spat.log)^2) / nrow(test_shots),5) # 0.08368

# AUC ROC score
round(AUC(y_pred = pred.spat.log,y_true = test_shots$is.goal),5)


# Save the predictions
log_predictions = data.frame(pred.basic.log,pred.complex.log,pred.spat.log)
colnames(log_predictions) = c("Logistic Regression Basic Model Predictions",
                              "Logistic Regression Complex Model Predictions",
                              "Logistic Regression Spatial-Temporal Model Predictions")
