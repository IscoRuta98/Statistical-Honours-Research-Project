rm(list = ls())

setwd("~/Honours/THESIS/StatsBomb/Thesis Code")
load("~/Honours/THESIS/StatsBomb/Thesis Code/MODEL_BUILDING_RESULTS_without_SMOTE.RData")

# Load libraries
library(StatsBombR)
library(dplyr)
library(tidyverse)
library(data.table)
library(scales)
library(caret)
library(RANN)
library(plotROC)
library(tidyr)
library(reshape2)
library(MLmetrics)
library(lubridate)
library(gridExtra)
library(cowplot)
library(ggthemes)
library(randomForest)
library(Metrics)
library(gbm)
library(pROC)


#### ROC plots ####

# Worst, Best, and StatsBomb xG models.
par(pty = "s")
roc(test_shots$is.goal,pred.basic.log,plot = TRUE,legacy.axes = TRUE,percent = TRUE,
    xlab = "False Positivity Percentage", ylab = "True Positivity Percentage",
    col = "Blue", lwd = 3,print.auc = TRUE)
plot.roc(test_shots$is.goal,pred.spat.gbm,percent = TRUE, col = "Green",lwd = 3,
         print.auc = TRUE,add = TRUE,print.auc.y=40)
plot.roc(test_shots$is.goal,test_shots$shot.statsbomb_xg,percent = TRUE, col = "Red",lwd = 3,
         print.auc = TRUE,add = TRUE,print.auc.y=30)
legend("bottomright", legend = c("Basic: Logistic Regression","Spatio-Temporal: GBM","StatsBomb xG Model"),
       col = c("Blue","Green","Red"), lwd = 1,cex = 0.6)

# Logistic Regression Models
par(pty = "s")
roc(test_shots$is.goal,pred.basic.log,plot = TRUE,legacy.axes = TRUE,percent = TRUE,
    xlab = "False Positivity Percentage", ylab = "True Positivity Percentage",
    col = "Blue", lwd = 3,print.auc = TRUE)
plot.roc(test_shots$is.goal,pred.complex.log,percent = TRUE, col = "Green",lwd = 3,
         print.auc = TRUE,add = TRUE,print.auc.y=40)
plot.roc(test_shots$is.goal,pred.spat.log,percent = TRUE, col = "Red",lwd = 3,
         print.auc = TRUE,add = TRUE,print.auc.y=30)
legend("bottomright", legend = c("Basic: Logistic Regression",
                                 "Complex: Logistic Regression",
                                 "Spatio-Temporal: Logistic Regression"),
       col = c("Blue","Green","Red"), lwd = 1,cex = 0.55)


# Random Forest Models
par(pty = "s")
roc(test_shots$is.goal,pred.basic.rf[,2],plot = TRUE,legacy.axes = TRUE,percent = TRUE,
    xlab = "False Positivity Percentage", ylab = "True Positivity Percentage",
    col = "Blue", lwd = 3,print.auc = TRUE)
plot.roc(test_shots$is.goal,pred.complex.rf[,2],percent = TRUE, col = "Green",lwd = 3,
         print.auc = TRUE,add = TRUE,print.auc.y=40)
plot.roc(test_shots$is.goal,pred.spat.rf[,2],percent = TRUE, col = "Red",lwd = 3,
         print.auc = TRUE,add = TRUE,print.auc.y=30)
legend("bottomright", legend = c("Basic: Random Forest",
                                 "Complex: Random Forest",
                                 "Spatio-Temporal: Random Forest"),
       col = c("Blue","Green","Red"), lwd = 1,cex = 0.55)


# GBM
par(pty = "s")
roc(test_shots$is.goal,pred.basic.gbm,plot = TRUE,legacy.axes = TRUE,percent = TRUE,
    xlab = "False Positivity Percentage", ylab = "True Positivity Percentage",
    col = "Blue", lwd = 3,print.auc = TRUE)
plot.roc(test_shots$is.goal,pred.complex.gbm,percent = TRUE, col = "Green",lwd = 3,
         print.auc = TRUE,add = TRUE,print.auc.y=40)
plot.roc(test_shots$is.goal,pred.spat.gbm,percent = TRUE, col = "Red",lwd = 3,
         print.auc = TRUE,add = TRUE,print.auc.y=30)
legend("bottomright", legend = c("Basic: GBM",
                                 "Complex: GBM",
                                 "Spatio-Temporal: GBM"),
       col = c("Blue","Green","Red"), lwd = 1,cex = 0.55)