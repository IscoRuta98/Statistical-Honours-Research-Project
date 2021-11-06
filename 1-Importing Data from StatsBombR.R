rm(list = ls())
setwd("~/Honours/THESIS/StatsBomb/Thesis Code")

library(dplyr)
library(tidyverse)
library(StatsBombR)


#### All Women Event Data ####
all_comp = FreeCompetitions()
Comp = subset(x = all_comp,subset = competition_id== 37 | competition_id==49)
Matches_37_49 = FreeMatches(Comp)

all_events_37_49 = data.frame()
for(i in 1:nrow(Matches_37_49)){
  if(i %in% c(94)) next
  temp = get.matchFree(Matches_37_49[i,])
  print(i)
  temp = allclean(temp)
  all_events_37_49 = bind_rows(all_events_37_49, temp)
  rm(temp)
}

save(all_events_37_49, file = "All Women Events Data")


#### Extract Player and Team Names ####

TeamNames = all_events_37_49 %>% 
  select(team.name, team.id) %>% 
  unique()
names(TeamNames) <- c("Team Name", "Team ID")

PlayerNames = all_events_37_49 %>% 
  select(player.name, player.id, team.name, team.id) %>% 
  unique() %>%
  filter(player.name != "NA")
names(PlayerNames) = c("Player Name", "Player ID", "Team Name", "Team ID")

