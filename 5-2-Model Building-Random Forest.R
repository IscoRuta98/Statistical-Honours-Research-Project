rm(list = ls())
setwd("~/Honours/THESIS/StatsBomb/Thesis Code")

library(StatsBombR)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(data.table)
library(scales)
library(plotROC)
library(reshape2)
library(MLmetrics)
library(lubridate)
library(gridExtra)
library(cowplot)
library(ggthemes)
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

train_shots$is.goal = as.factor(train_shots$is.goal)

#### BASIC MODEL ####

set.seed(2020)
basic.rf = randomForest(formula = is.goal~location.x+location.y+DistToKeeper+DistToGoal+
                        AngleToKeeper+AngleToGoal+shot.body_part.name+shot.type.name,
                        data = train_shots,
                        ntree = 5000,
                        importance = TRUE,
                        do.trace = 500)

# Variable Importance Plot
varImpPlot(x = basic.rf,sort = TRUE,type = 2)
var.imp.basic = basic.rf$importance[,4]

rf_basic_varImp = randomForest::importance(basic.rf, type = 2)
rf_basic_varImp = rf_basic_varImp[order(rf_basic_varImp,decreasing = FALSE)]

par(mar = c(5.1,6,4.1,2.1))
barplot(rf_basic_varImp, horiz = T, col = 'navy', las = 1,
        xlab = 'Mean decrease in Gini index', cex.lab = 1.1, cex.axis = 1, 
        main = 'Random Forest: Basic Model', cex.main = 1.5, cex.names = 0.7,
        names.arg = c("shot.type.name","shot.body_part.name","AngleToKeeper",
                      "location.y","location.x","AngleToGoa","DistToKeeper",
                      "DistToGoal"))


pred.basic.rf = predict(object = basic.rf,newdata = test_shots,type = "prob")

# Brier Score
round(sum((test_shots$is.goal- pred.basic.rf[,2])^2) / nrow(test_shots),5) # 0.08666

# AUC ROC score
round(AUC(y_pred = pred.basic.rf[,2],y_true = test_shots$is.goal),5) # 0.73179


#### COMPLEX MODEL ####

set.seed(2020)
complex.rf = randomForest(formula = is.goal~location.x+location.y+DistToKeeper+DistToGoal+
                          AngleToKeeper+AngleToGoal+shot.body_part.name+shot.type.name+
                          shot.technique.name+play_pattern.name+TimeInPoss+under_pressure+
                          avevelocity+position.name.2+shot.first_time+shot.one_on_one,
                          data = train_shots,
                          ntree = 5000,
                          importance = TRUE,
                          do.trace = 500)

# Variable Importance Plot
varImpPlot(x = complex.rf,sort = TRUE,type = 2)
var.imp.complex = complex.rf$importance[,4]

rf_complex_varImp = randomForest::importance(complex.rf, type = 2)
rf_complex_varImp = rf_complex_varImp[order(rf_complex_varImp,decreasing = FALSE)]

par(mar = c(5.1,6,4.1,2.1))
barplot(rf_complex_varImp, horiz = T, col = 'navy', las = 1,
        xlab = 'Mean decrease in Gini index', cex.lab = 1.1, cex.axis = 1, 
        main = 'Random Forest: Complex Model', cex.main = 1.5, cex.names = 0.7,
        names.arg = c('shot.type.name','shot.one_on_one','under_pressure',
                      'shot.first_time','shot.body_part.name','shot.technique.name',
                      'position.name.2','play_pattern.name','AngleToKeeper',
                      'location.y','AngleToGoal','location.x','TimeInPoss',
                      'DistToKeeper','avevelocity','DistToGoal'))

pred.complex.rf = predict(object = complex.rf,newdata = test_shots,type = "prob")

# Brier Score
round(sum((test_shots$is.goal- pred.complex.rf[,2])^2) / nrow(test_shots),5) # 0.08083

# AUC ROC score
round(AUC(y_pred = pred.complex.rf[,2],y_true = test_shots$is.goal),5) # 0.7665


#### SPATIAL-TEMPORAL ####

set.seed(2020)
spat.rf = randomForest(formula = is.goal~.-shot.statsbomb_xg,
                          data = train_shots,
                          ntree = 5000,
                          importance = TRUE,
                          do.trace = 500)


# Variable Importance Plot
varImpPlot(x = spat.rf,sort = TRUE,type = 2)
var.imp.spat = spat.rf$importance[,4]

rf_spat_varImp = randomForest::importance(spat.rf, type = 2)
rf_spat_varImp = rf_spat_varImp[order(rf_spat_varImp,decreasing = FALSE)]

par(mar = c(5.1,6,4.1,2.1))
barplot(rf_spat_varImp, horiz = T, col = 'navy', las = 1,
        xlab = 'Mean decrease in Gini index', cex.lab = 1.1, cex.axis = 1, 
        main = 'Random Forest: Spatio-Temporal Model', cex.main = 1.5, cex.names = 0.7,
        names.arg = c('shot.type.name','shot.one_on_one','under_pressure',
                      'shot.first_time','shot.body_part.name','shot.technique.name',
                      'AttackersBehindBall','position.name.2','play_pattern.name',
                      'DefendersBehindBall','AngleToKeeper','distance.ToD1.360',
                      'minute','AngleToGoal','location.y','distance.ToD2.360',
                      'TimeInPoss','location.x','attack_dominance','DistToKeeper',
                      'avevelocity','DistToGoal'))
        

pred.spat.rf = predict(object = spat.rf,newdata = test_shots,type = "prob")

# Brier Score
round(sum((test_shots$is.goal- pred.spat.rf[,2])^2) / nrow(test_shots),5) # 0.08153

# AUC ROC score
round(AUC(y_pred = pred.spat.rf[,2],y_true = test_shots$is.goal),5) # 0.75846

# Save Enviroment
save.image("~/Honours/THESIS/StatsBomb/Thesis Code/random_forest_models.RData")