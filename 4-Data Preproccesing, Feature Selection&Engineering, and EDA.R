rm(list = ls())
setwd("~/Honours/THESIS/StatsBomb/Thesis Code")

library(StatsBombR)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(arsenal)
library(stargazer)
library(corrplot)
require(GGally)

# Load  Women Shots & FreezeFrame Data
load("~/Honours/THESIS/StatsBomb/Thesis Code/Shots Data-Including attack_domin")

# Change character variables into factors/categories.
shots.varsdata$is.goal = as.numeric(shots.varsdata$is.goal)
shots.varsdata$shot.body_part.name = as.factor(shots.varsdata$shot.body_part.name)
shots.varsdata$shot.type.name = as.factor(shots.varsdata$shot.type.name)
shots.varsdata$shot.technique.name = as.factor(shots.varsdata$shot.technique.name)
shots.varsdata$position.name = as.factor(shots.varsdata$position.name)
shots.varsdata$play_pattern.name = as.factor(shots.varsdata$play_pattern.name)
shots.varsdata$under_pressure = as.factor(shots.varsdata$under_pressure)
shots.varsdata$shot.first_time = as.factor(shots.varsdata$shot.first_time)
shots.varsdata$shot.one_on_one = as.factor(shots.varsdata$shot.one_on_one)

# Remove variables that are redundant
rm_vars = c('location',"density.incone","DefendersInCone","InCone.GK",
            "DefArea", "distance.ToD1","distance.ToD2")
shots.varsdata = shots.varsdata[,!(names(shots.varsdata) %in% rm_vars)]

#### EDA ####

#### Goal or No Goal ###

# Proportion of shots converted to goals.
table(shots.varsdata$is.goal)

# Proportion of shots converted to goals, in terms of player position.
table(shots.varsdata$position.name,shots.varsdata$is.goal)

# Proportion of shots converted to goals, in terms of body part
table(shots.varsdata$shot.body_part.name,shots.varsdata$is.goal)

# Proportion of shots converted to goals, in terms of technique
table(shots.varsdata$shot.technique.name,shots.varsdata$is.goal)

# Proportion of shots converted to goals, in terms of player being under pressure
table(shots.varsdata$under_pressure,shots.varsdata$is.goal)

# Proportion of shots converted to goals, in terms of whether the player is one-on-one
table(shots.varsdata$shot.one_on_one,shots.varsdata$is.goal)

# Proportion of shots converted to goals, in terms of whether the player is one-on-one
table(shots.varsdata$shot.one_on_one,shots.varsdata$is.goal)

# Proportion of shots converted to goals, in terms of whether the shot is first time.
table(shots.varsdata$shot.first_time,shots.varsdata$is.goal)

#### Distance and Angle ####
dist_angle = ggplot() +
  layer(data = shots.varsdata,
        stat = "identity",
        geom = "point",
        mapping = aes(x = DistToGoal, y = AngleToGoal,color = as.factor(is.goal)),
        position = "identity") + 
  ggtitle("Distance and Angle of Shots",subtitle = "Source: StatsBomb Open Data") +
  xlab("Distance to goal (in meters)") +
  ylab("Angle to goal")

dist_angle + labs(title = "Distance and Angle of Shots",
                  subtitle = "Source: StatsBomb(2018)",
                  color = "Shots",caption = "0 = No Goal, 1 = Goal")
#### Summary stats Table ####
descriptve_stats = tableby(is.goal ~ DistToGoal+AngleToGoal+DistToKeeper+AngleToKeeper+
                             TimeInPoss+avevelocity+attack_dominance+distance.ToD1.360+
                             distance.ToD2.360+minute+second+AttackersBehindBall+
                             DefendersBehindBall,data = shots.varsdata)

summary(descriptve_stats,title = "Descriptive stats")

#### correlation matrix ####
numerical_vars = c('is.goal','DistToGoal','AngleToGoal','DistToKeeper','AngleToKeeper',
                     'TimeInPoss','avevelocity','attack_dominance','distance.ToD1.360',
                     'distance.ToD2.360','minute','AttackersBehindBall',
                     'DefendersBehindBall')

numeric_df = shots.varsdata[,numerical_vars]

# View(round(cor(numeric_df),3))
corrplot(corr = cor(numeric_df),method = "circle")

### Attacking Dominance ####

# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram 
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(shots.varsdata$attack_dominance, horizontal=TRUE, xaxt="n" , 
        col=rgb(0.8,0.8,0,0.5) , frame=F,
        main="Histogram and Boxplot of Attakcing Dominance")
par(mar=c(4, 3.1, 1.1, 2.1))
hist(shots.varsdata$attack_dominance , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F ,
     xlab="Attacking Dominance",ylab = '',main = '')

# Boxplot
par(mfrow = c(1,1))
boxplot(attack_dominance ~ is.goal,data = shots.varsdata)

attack_dom_boxplot = qplot(as.factor(is.goal),attack_dominance,data = shots.varsdata,
                           geom = ('boxplot'),main = "Boxplot of goal outcome versus Attacking Dominance",
                           xlab = "Goal vs No Goal",ylab = "Attacking Dominance")

attack_dom_boxplot


#### Feature Selection ####

# Further features that have to be removed.
rm_vars_2 = c("tb.to.shot.time","dribble.to.shot.time","second")
shots.varsdata = shots.varsdata[,!(names(shots.varsdata) %in% rm_vars_2)]

# Create a new 'position.name' feature that reduces the number of categories to 6.

shots.varsdata$position.name.2[shots.varsdata$position.name == "Goalkeeper"] = "Goalkeeper"

shots.varsdata$position.name.2[shots.varsdata$position.name == "Center Back"] = "Defender"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Left Back"] = "Defender"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Left Center Back"] = "Defender"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Left Wing Back"] = "Defender"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Right Wing Back"] = "Defender"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Right Back"] = "Defender"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Right Center Back"] = "Defender"


shots.varsdata$position.name.2[shots.varsdata$position.name == "Center Attacking Midfield"] = "Attacking Midfielder"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Center Defensive Midfield"] = "Midfielder"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Center Midfield"] = "Midfielder"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Left Attacking Midfield"] = "Attacking Midfielder"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Left Center Midfield"] = "Midfielder"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Left Defensive Midfield"] = "Midfielder"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Left Midfield"] = "Midfielder"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Right Attacking Midfield"] = "Attacking Midfielder"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Right Center Midfield"] = "Midfielder"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Right Defensive Midfield"] = "Midfielder"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Right Midfield"] = "Midfielder"

shots.varsdata$position.name.2[shots.varsdata$position.name == "Right Wing"] = "Winger"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Left Wing"] = "Winger"

shots.varsdata$position.name.2[shots.varsdata$position.name == "Center Forward"] = "Forward"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Left Center Forward"] = "Forward"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Right Center Forward"] = "Forward"
shots.varsdata$position.name.2[shots.varsdata$position.name == "Secondary Striker"] = "Forward"

shots.varsdata$position.name.2 = as.factor(shots.varsdata$position.name.2)

# Remove position.name (original) variable and the freeze-frame feature
rm_vars_3 = c("position.name","shot.freeze_frame")
shots.varsdata = shots.varsdata[,!(names(shots.varsdata) %in% rm_vars_3)]

# Save the above data frame
save(shots.varsdata,file = "Data for Modelling")
