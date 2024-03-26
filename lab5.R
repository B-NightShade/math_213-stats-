library(ggplot2)
library(dplyr)

streams <- read.csv("medley.csv")

View(streams)


#ex1
ggplot(streams, aes(zinc,diversity))+
  geom_boxplot()

streams$zinc <- factor(streams$zinc, levels = c("BACK", "LOW", "MED", "HIGH"))

streams$zinc.high <- streams$zinc == "HIGH"

ggplot(streams, aes(zinc,diversity))+
  geom_boxplot()

streams$zinc <- factor(streams$zinc, levels = c("BACK", "LOW", "MED", "HIGH"))

streams$zinc.high <- streams$zinc == "HIGH"


ggplot(streams, aes(zinc.high,diversity))+
  geom_boxplot()

#ex2
tapply(streams$diversity, streams$zinc.high, mean)


streams %>% 
  group_by(zinc.high) %>%
  summarize(mean=mean(diversity))

#ex3
tapply(streams$diversity, streams$zinc.high, length)

streams %>% 
  group_by(zinc.high) %>%
  summarize(n=n())


ggplot(data = streams, aes(x = diversity)) +
  geom_histogram(bins=5) +
  facet_grid(~zinc.high)

ggplot(data = streams, aes(sample = diversity)) + 
  stat_qq()+
  stat_qq_line() +
  facet_grid(~zinc.high)


#ex4
t.test(diversity~zinc.high, data=streams)

set.seed(17)                                      
num_sim <- 1000
diffs <- numeric(num_sim)                         
for(i in 1:num_sim){
  high <- sample(1:34,9)                          
  high_mean <- mean(streams$diversity[high])      
  not_high_mean <- mean(streams$diversity[-high]) 
  diffs[i] <- not_high_mean - high_mean           
}

high_mean <- mean(streams$diversity[streams$zinc.high])
not_high_mean <- mean(streams$diversity[!streams$zinc.high])
obs_diff <- not_high_mean - high_mean
pval <- mean((diffs >= obs_diff) | (diffs <= -obs_diff))
pval

#ex 5
diversity.zinc.lm <- lm(diversity~zinc, data=streams)
anova(diversity.zinc.lm)

#ex 6
pairwise.t.test(streams$diversity, streams$zinc, p.adjust.method = "bonferroni")


#ex7
ggplot(data = streams, aes(x = diversity)) +
  geom_histogram(bins=5) +
  facet_grid(~zinc)

ggplot(data = streams, aes(sample = diversity)) + 
  stat_qq()+
  stat_qq_line() +
  facet_grid(~zinc)

tapply(streams$diversity, streams$zinc, sd)

streams %>% 
  group_by(zinc) %>%
  summarize(mean=mean(diversity),
            sd=sd(diversity),
            n=n())
