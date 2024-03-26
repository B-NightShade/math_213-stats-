my.lm <- lm(away_score~home_score, data=SoccerData)
summary(my.lm)

new.dat <- data.frame(speed=30)
predict(my.lm, newdata = new.dat, interval = 'confidence')

confint(my.lm, level=0.95)

library(ggplot2)
library(dplyr)

ggplot(SoccerData, aes(home_score, away_score))+
  geom_point()+
  geom_smooth(method=lm)


popular <- SoccerData %>%
  filter(home_team == "England"  | home_team == "Argentina" 
         | home_team == "France" | home_team == "Germany" 
         | away_team == "England"  | away_team == "Argentina" 
           | away_team == "France" | away_team == "Germany")


my.lm <- lm(away_score~home_score, data=popular)

ggplot(SoccerData, aes(home_score, away_score))+
  geom_point()+
  geom_smooth(method=lm)


plot(SoccerData$away_score, SoccerData$home_score, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)
abline(lm(away_score ~ home_score, data = SoccerData), col = "blue")

tourn <- SoccerData %>%
  filter((tournament == "COSAFA Cup") | (tournament == "FIFA World Cup") | 
           (tournament == "UEFA Euro"))

my.lm <- lm(away_score~home_score, data=tourn)
summary(my.lm)

plot(tourn$away_score, tourn$home_score, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
    frame = FALSE)
abline(lm(away_score ~ home_score, data = tourn), col = "red")


