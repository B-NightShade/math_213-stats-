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
ggplot(taco_bell, aes(x=calories))+
  geom_histogram(binwidth = 100)
ggplot(subway_and_taco_bell, aes(x=restaurant, y=calories))+
  geom_boxplot()
ggplot(subway_and_taco_bell, aes(x=calories))+
  geom_histogram(binwidth = 100)+
  facet_grid(~restaurant)


#excerise 2
submean <- mean(subway$calories)
subsd   <- sd(subway$calories)

ggplot(data = subway, aes(x = calories)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = submean, sd = subsd), color = "tomato")

tacomean <- mean(taco_bell$calories)
tacosd <- sd(taco_bell$calories)


ggplot(data = taco_bell, aes(x = calories)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = tacomean, sd = tacosd), color = "tomato")


#exercise 3
ggplot(data = subway, aes(sample = calories)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(subway), mean = submean, sd = subsd)
ggplot(subway, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()

#exercise 4
qqnormsim(sample = calories, data = subway)

#exercise 5
ggplot(data = taco_bell, aes(sample = calories)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(taco_bell), mean = tacomean, sd = tacosd)
ggplot(taco_bell, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()

qqnormsim(sample = calories, data = taco_bell)

#exercise 6
pnorm(600, mean = submean, sd = subsd)

mean(subway$calories < 600)

1- pnorm(500, mean = submean, sd = subsd)
mean(subway$calories > 500)


1- pnorm(500, mean = tacomean, sd = tacosd)
mean(taco_bell$calories > 500)

#exercise 7
taco <- fastfood %>%
  filter(restaurant == "Taco Bell")

tacmean <- mean(taco$sodium)
tacsd   <- sd(taco$sodium)

ggplot(data = taco, aes(x = sodium)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = tacmean, sd = tacsd), color = "tomato")

ggplot(data = taco, aes(sample = sodium)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(taco), mean = tacmean, sd = tacsd)
ggplot(taco, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()


sub <- fastfood %>%
  filter(restaurant == "Subway")

submean <- mean(sub$sodium)
subsd   <- sd(sub$sodium)

ggplot(data = sub, aes(x = sodium)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = submean, sd = subsd), color = "tomato")

ggplot(data = sub, aes(sample = sodium)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(sub), mean = submean, sd = subsd)
ggplot(sub, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()


Mc <- fastfood %>%
  filter(restaurant == "Mcdonalds")

Mcmean <- mean(Mc$sodium)
Mcsd   <- sd(Mc$sodium)

ggplot(data = Mc, aes(x = sodium)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = Mcmean, sd = Mcsd), color = "tomato")

ggplot(data = Mc, aes(sample = sodium)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(Mc), mean = Mcmean, sd = Mcsd)
ggplot(Mc, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()


chick <- fastfood %>%
  filter(restaurant == "Chick Fil-A")

chickmean <- mean(chick$sodium)
chicksd   <- sd(chick$sodium)

ggplot(data = chick, aes(x = sodium)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = chickmean, sd = chicksd), color = "tomato")

ggplot(data = chick, aes(sample = sodium)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(chick), mean = chickmean, sd = chicksd)
ggplot(chick, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()


sonic <- fastfood %>%
  filter(restaurant == "Sonic")

sonicmean <- mean(sonic$sodium)
sonicsd   <- sd(sonic$sodium)

ggplot(data = sonic, aes(x = sodium)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = sonicmean, sd = sonicsd), color = "tomato")

ggplot(data = sonic, aes(sample = sodium)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(sonic), mean = sonicmean, sd = sonicsd)
ggplot(sonic, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()



arbys <- fastfood %>%
  filter(restaurant == "Arbys")

arbysmean <- mean(arbys$sodium)
arbyssd   <- sd(arbys$sodium)

ggplot(data = arbys, aes(x = sodium)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = arbysmean, sd = arbyssd), color = "tomato")

ggplot(data = arbys, aes(sample = sodium)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(arbys), mean = arbysmean, sd = arbyssd)
ggplot(arbys, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()


burger <- fastfood %>%
  filter(restaurant == "Burger King")

burgermean <- mean(burger$sodium)
burgersd   <- sd(burger$sodium)

ggplot(data = burger, aes(x = sodium)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = burgermean, sd = burgersd), color = "tomato")

ggplot(data = burger, aes(sample = sodium)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(burger), mean = burgermean, sd = burgersd)
ggplot(burger, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()


dairy <- fastfood %>%
  filter(restaurant == "Dairy Queen")

dairymean <- mean(dairy$sodium)
dairysd   <- sd(dairy$sodium)

ggplot(data = dairy, aes(x = sodium)) +
  geom_histogram(aes(y = ..density..), binwidth = 100) +
  stat_function(fun = dnorm, args = c(mean = dairymean, sd = dairysd), color = "tomato")

ggplot(data = dairy, aes(sample = sodium)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(dairy), mean = dairymean, sd = dairysd)
ggplot(dairy, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()

#exercise 8

taco <- fastfood %>%
  filter(restaurant == "Taco Bell")

tacmean <- mean(taco$sodium)
tacsd   <- sd(taco$sodium)

sd(taco$sodium)
mean(taco$sodium)-sd(taco$sodium)
mean(taco$sodium)+sd(taco$sodium)

mean(taco$sodium < 1487.967) - mean(taco$sodium < 539.8587)

mean(taco$sodium)- 2*(sd(taco$sodium))
mean(taco$sodium)+ 2*(sd(taco$sodium))

mean(taco$sodium < 1962.022) - mean(taco$sodium < 65.804)

mean(taco$sodium)- 3*(sd(taco$sodium))
mean(taco$sodium)+ 3*(sd(taco$sodium))

mean(taco$sodium < 2436.076) - mean(taco$sodium < -408.25)

#exercise 9


arbys <- fastfood %>%
  filter(restaurant == "Arbys")

arbysmean <- mean(arbys$total_carb)
arbyssd   <- sd(arbys$total_carb)

ggplot(arbys, aes(x=total_carb))+
  geom_histogram(binwidth=50)

ggplot(data = arbys, aes(sample = total_carb)) + 
  stat_qq()+
  stat_qq_line()

sim_norm <- rnorm(n = nrow(arbys), mean = arbysmean, sd = arbyssd)
ggplot(arbys, aes(sample = sim_norm)) + 
  stat_qq()+
  stat_qq_line()


burger <- fastfood %>%
  filter(restaurant == "Burger King")

burgermean <- mean(burger$total_carb)
burgersd   <- sd(burger$total_carb)

ggplot(burger, aes(x=total_carb))+
  geom_histogram(binwidth = 50)

ggplot(data = burger, aes(sample = total_carb)) + 
  stat_qq()+
  stat_qq_line()

