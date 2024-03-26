summary(SoccerData)
View(SoccerData)

library(dplyr)
library(ggplot2)

is_tourn <- SoccerData %>%
  filter((tournament == "Oceania Nations Cup") | (tournament == "Cyprus International Tournament"))

ggplot(is_tourn, aes(tournament,home_score))+
  geom_boxplot()

tapply(is_tourn$home_score, is_tourn$tournament, mean)
tapply(is_tourn$home_score, is_tourn$tournament, length)


check <- SoccerData %>%
  filter(tournament == "Oceania Nations Cup")
hscore <- check$home_score

2+3*IQR(hscore)
max(hscore)

set.seed(22)                                      
num_sim <- 1000
diffs <- numeric(num_sim)                         
for(i in 1:num_sim){
  ocean <- sample(1:59,54)                          
  o_mean <- mean(is_tourn$home_score[ocean])      
  not_o_mean <- mean(is_tourn$home_score[-ocean]) 
  diffs[i] <- not_o_mean - o_mean           
}

o_mean <- mean(is_tourn$home_score[is_tourn$tournament == "Cyprus International Tournament"])
not_o_mean <- mean(is_tourn$home_score[is_tourn$tournament != "Cyprus International Tournament"])
obs_diff <- not_o_mean - o_mean
pval <- mean((diffs >= obs_diff) | (diffs <= -obs_diff))
pval

t.test(home_score~is_tourn$tournament, data=is_tourn)
