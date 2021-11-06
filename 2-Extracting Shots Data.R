rm(list = ls())
setwd("~/Honours/THESIS/StatsBomb/Thesis Code")

library(StatsBombR)
library(dplyr)
library(tidyr)
library(tidyverse)
library(imputeTS)

# Load All Women Events Data
load("~/Honours/THESIS/StatsBomb/Thesis Code/All WOmen Events Data")

# Flagging all possessions with a through ball and dribbles

all_events = all_events_37_49 %>% group_by(match_id, possession) %>%
  mutate(tb.flag = any(pass.through_ball == T),
         shot.flag = any(type.name == 'Shot'),
         dribble.flag = any(dribble.outcome.name == 'Complete'),
         tb.time = ifelse(shot.flag == T & pass.through_ball == T, TimeInPoss, NA),
         shot.time = ifelse(type.name == 'Shot', TimeInPoss, NA),
         dribble.time = ifelse(shot.flag == T & dribble.outcome.name == 'Complete', TimeInPoss, NA)) %>%
  fill(tb.time, shot.time, dribble.time) %>%
  mutate(dribble.to.shot.time = shot.time - dribble.time,
         tb.to.shot.time = shot.time - tb.time)
# freezeframeinfo() extracts the following features which I've added them to ind_vars:
# "shot.freeze_frame","density.incone", "distance.ToD1",
# "distance.ToD2", "AttackersBehindBall", "DefendersBehindBall",
# "DefendersInCone", "InCone.GK", "DefArea", "distance.ToD1.360",
# "distance.ToD2.360"

all_events = freezeframeinfo(all_events)

all_shots = filter(all_events, type.name == 'Shot')
all_shots$is.goal = ifelse(all_shots$shot.outcome.name == 'Goal', "1","0")


logical.vars = names(Filter(is.logical, all_shots))
df = all_shots[logical.vars]
df[is.na(df)] = FALSE
all_shots[logical.vars] = df

#choosing independent variables
flag_vars = c('dribble.flag', 'tb.flag', 'dribble.to.shot.time', 'tb.to.shot.time')
 all_shots[flag_vars][is.na(all_shots[flag_vars])] = 5000

ind.vars = c('id','player.name', 'team.name','location.x','location.y','location'
             ,'minute', 'second', 'is.goal',
             'DistToKeeper', 'DistToGoal', 'AngleToKeeper', 
             'AngleToGoal', 'play_pattern.name',
             'shot.body_part.name', 'shot.type.name', 'shot.technique.name', 
             'dribble.to.shot.time', 'tb.to.shot.time','TimeInPoss',
             'under_pressure',
             'avevelocity',
             'position.name',
             'shot.first_time',
             'shot.one_on_one',"shot.freeze_frame","density.incone", "distance.ToD1",
             "distance.ToD2", "AttackersBehindBall", "DefendersBehindBall",'shot.statsbomb_xg',
             "DefendersInCone", "InCone.GK", "DefArea", "distance.ToD1.360",
             "distance.ToD2.360")

not_all_na = function(x){
  any(!is.na(x))
}
all_shots_valid = all_shots %>% select_if(not_all_na)

shots.varsdata = subset(all_shots_valid, select = ind.vars) %>% drop_na()

save(shots.varsdata,file = "Women Shots Data")

#### Extracting FreezeFrame data as its own dataframe ####

FreezeFrameData = shots.varsdata %>% 
  select(minute, second, shot.freeze_frame)

FreezeFrame = FreezeFrameData %>% 
  filter(!map_lgl(shot.freeze_frame, is.null)) %>% 
  unnest()

# Save the above freeze frame dataframes.
save(FreezeFrameData,file = "FreezeFrameData")
save(FreezeFrame,file = "FreezeFrame")
