rm(list = ls())
setwd("~/Honours/THESIS/StatsBomb/Thesis Code")
library(deldir)


load(file = "Women Shots Data")

# Remove observations that have identical locations of two or more players.

rm_shots = c(2396,2403,2407,2421,2469,2480,2482,2490,2494,2503,2733,
             2755,2871,2898,2905,3166,3183,3616,3694,3767,3773,3946,
             3984,3986,3994,4029,4045,4062,4163,4293,4852,4869,5012,
             5123,5260,5525)
shots.varsdata = shots.varsdata[-rm_shots,]

attack_domin = rep(0,nrow(shots.varsdata))

for (i in 1:nrow(shots.varsdata)) {
  freeze_frame = shots.varsdata$shot.freeze_frame[[i]]
  dy12 = deldir(c(matrix(data = unlist(freeze_frame$location),
                         nrow = nrow(freeze_frame),ncol = 2,
                         byrow = T)[,1],shots.varsdata$location[[i]][1])
                ,c(matrix(data = unlist(freeze_frame$location),
                          nrow = nrow(freeze_frame),ncol = 2,
                          byrow = T)[,2],shots.varsdata$location[[i]][2])
  )
  freeze_frame$area = dy12$summary$del.area[-length(dy12$summary$del.area)]
  freeze_frame$teammate = as.factor(freeze_frame$teammate)
  agg = aggregate(freeze_frame$area, by=list(freeze_frame$teammate=="TRUE"), FUN = sum)
  
  attack_domin[i] = 1 - (agg$x[1] / sum(dy12$summary$del.area))
# print(i) ; print(attack_domin[i])
}

shots.varsdata$attack_dominance = attack_domin

# Save the above data frame
save(shots.varsdata,file = "Shots Data-Including attack_domin")
