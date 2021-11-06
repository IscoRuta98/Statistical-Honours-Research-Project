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

#GBM requires target variable to be numeric.
train_shots$is.goal = as.numeric(train_shots$is.goal) - 1

#### BASIC MODEL ####

set.seed(2020)
basic.gbm = gbm(is.goal~location.x+location.y+DistToKeeper+DistToGoal+
                  AngleToKeeper+AngleToGoal+shot.body_part.name+shot.type.name,
                data = train_shots, 
                distribution = 'bernoulli', 
                n.trees = 5000, #B
                interaction.depth = 2, #d
                shrinkage = 0.01, #lambda
                bag.fraction = 1, #default = 0.5 for extra randomisation. 
                cv.folds = 10, #built-in CV
                n.cores = 3, #which can be parallelised
                verbose = F)

#CV Errors per tree
best_basic_B = gbm.perf(basic.gbm, method = 'cv') 

#Variable importance/ Relative Influence
basic.var.imp.gbm = summary(basic.gbm, n.trees = best_basic_B, main = 'xG variable importance - Basic GBM') 

par(mar = c(5.1,6,4.1,2.1))
barplot(height = basic.var.imp.gbm$rel.inf,horiz = T,
        names.arg = basic.var.imp.gbm$var,col = 'green',xlab = "Relative Influence",
        cex.names = 0.7,las = 1,main = "GBM: Basic Model")


pred.basic.gbm = predict(object = basic.gbm,newdata = test_shots,type = "response")
# Brier Score
round(sum((test_shots$is.goal- pred.basic.gbm)^2) / nrow(test_shots),5) # 0.13242

# AUC ROC score
round(AUC(y_pred = pred.basic.gbm,y_true = test_shots$is.goal),5) # 0.76011

#### COMPLEX MODEL ####

set.seed(2020)
complex.gbm = gbm(is.goal~location.x+location.y+DistToKeeper+DistToGoal+
                    AngleToKeeper+AngleToGoal+shot.body_part.name+shot.type.name+
                    shot.technique.name+play_pattern.name+TimeInPoss+under_pressure+
                    avevelocity+position.name.2+shot.first_time+shot.one_on_one,
                  data = train_shots, 
                  distribution = 'bernoulli', 
                  n.trees = 5000, #B
                  interaction.depth = 2, #d
                  shrinkage = 0.01, #lambda
                  bag.fraction = 1, #default = 0.5 for extra randomisation. 
                  cv.folds = 10, #built-in CV
                  n.cores = 4, #which can be parallelised
                  verbose = F)

#CV Errors per tree
best_complex_B = gbm.perf(complex.gbm, method = 'cv') 

#Variable importance/ Relative Influence
complex.var.imp.gbm = summary(complex.gbm, n.trees = best_complex_B, main = 'xG variable importance - Complex GBM') 

par(mar = c(5.1,6,4.1,2.1))
barplot(height = complex.var.imp.gbm$rel.inf,horiz = T,
        names.arg = complex.var.imp.gbm$var,col = 'green',xlab = "Relative Influence",
        cex.names = 0.7,las = 1,main = "GBM: Complex Model",xlim = c(0,50))

pred.complex.gbm = predict(object = complex.gbm,newdata = test_shots,type = "response")

# Brier Score
round(sum((test_shots$is.goal- pred.complex.gbm)^2) / nrow(test_shots),5) # 0.12338

# AUC ROC score
round(AUC(y_pred = pred.complex.gbm,y_true = test_shots$is.goal),5) # 0.77327


#### SPATIAL-TEMPORAL MODEL ####

set.seed(2020)
spat.gbm = gbm(is.goal~.-shot.statsbomb_xg,
               data = train_shots, 
               distribution = 'bernoulli', 
               n.trees = 5000, #B
               interaction.depth = 2, #d
               shrinkage = 0.01, #lambda
               bag.fraction = 1, #default = 0.5 for extra randomisation. 
               cv.folds = 10, #built-in CV
               n.cores = 4, #which can be parallelised
               verbose = F)

#CV Errors per tree
best_spat_B = gbm.perf(spat.gbm, method = 'cv') 

#Variable importance/ Relative Influence
spat.var.imp.gbm = summary(spat.gbm, n.trees = best_spat_B, main = 'xG variable importance - Spatial-Temporal GBM') 

par(mar = c(5.1,6,4.1,2.1))
barplot(height = spat.var.imp.gbm$rel.inf,horiz = T,
        names.arg = spat.var.imp.gbm$var,col = 'green',xlab = "Relative Influence",
        cex.names = 0.7,las = 1,main = "GBM: Spatio-Temporal Model",xlim = c(0,45))



pred.spat.gbm = predict(object = spat.gbm,newdata = test_shots,type = "response")

# Brier Score
round(sum((test_shots$is.goal- pred.spat.gbm)^2) / nrow(test_shots),5) # 0.12525

# AUC ROC score
round(AUC(y_pred = pred.spat.gbm,y_true = test_shots$is.goal),5) # 0.76336


#### StatsBomb AUC & Brier Score ####

# StatsBomb Brier Score
round(sum((test_shots$is.goal- test_shots$shot.statsbomb_xg)^2) / nrow(test_shots),5) # 0.07739

# AUC ROC score
round(AUC(y_pred = test_shots$shot.statsbomb_xg,y_true = test_shots$is.goal),5)
