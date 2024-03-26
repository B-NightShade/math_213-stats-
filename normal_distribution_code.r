library(ggplot2)
library(dplyr)
library(openintro)

head(fastfood)
summary(fastfood)
unique(fastfood$restaurant)

subway <- fastfood %>%
  filter(restaurant == "Subway")
taco_bell <- fastfood %>%
  filter(restaurant == "Taco Bell")
subway_and_taco_bell <- fastfood %>%
  filter(restaurant %in% c("Subway", "Taco Bell"))

ggplot(subway, aes(x=calories))+
  geom_histogram(binwidth = 100)
ggplot(subway_and_taco_bell, aes(x=restaurant, y=calories))+
  geom_boxplot()
ggplot(subway_and_taco_bell, aes(x=calories))+
  geom_histogram()+
  facet_grid(~restaurant)

submean <- mean(subway$calories)
subsd   <- sd(subway$calories)

ggplot(data = subway, aes(x = calories)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = submean, sd = subsd), color = "tomato")

ggplot(data = subway, aes(sample = calories)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(subway), mean = submean, sd = subsd)
ggplot(subway, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()

qqnormsim(sample = calories, data = subway)

pnorm(600, mean = submean, sd = subsd)

mean(subway$calories < 600)
