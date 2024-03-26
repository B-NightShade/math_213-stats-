# 8/31

sleep <- c(7,8,12,6,5,7,8,5,8,5,7,8,8,3,8,7,6,9,4,7)
hist(sleep)
mean(sleep)
var(sleep)
sd(sleep)
median(sleep)
sort(sleep)
quantile(sleep)
summary(sleep)
IQR(sleep)

#9/16

library(openintro)
library(dplyr)

?treeDiag
treeDiag(main = c('Disease status', 'Test result'), 
         out1 = c("sick","not sick"), 
         out2 = c("positive","negative"), 
         p1 = c(0.02, 0.98), 
         p2 = list(c(0.95, 0.05), c(0.2, 0.8)), 
         showWork = TRUE)

treeDiag(c("Flight on time?","Luggage on time?"),
         c(0.8, 0.2), list(c(0.97, 0.03), c(0.15, 0.85)))
treeDiag(c("Breakfast?","Go to class"), c(.4,.6),
         list(c(0.4,0.36,0.34), c(0.6,0.3,0.1)), c("Yes", "No"),
         c("Statistics","English","Sociology"), showWork = TRUE)
treeDiag(c("Breakfast?","Go to class"), c(0.4, 0.11, 0.49),
         list(c(0.4, 0.36, 0.24), c(0.6, 0.3, 0.1), c(0.1, 0.4, 0.5)),
         c("one", "two", "three"), c("Statistics", "English", "Sociology"))
treeDiag(c("Dow Jones rise?", "NASDAQ rise?"),
         c(0.53, 0.47), list(c(0.75, 0.25), c(0.72, 0.28)),
         solSub = list(c("(a)", "(b)"), c("(c)", "(d)")), solwd = 0.08)

data("nycflights")
View(nycflights)

nycflights %>%
  group_by(carrier) %>%
  summarise(flights = n()) %>%
  arrange(desc(flights))

top5 <- nycflights %>%
  filter(carrier %in% c("UA", "B6", "EV", "DL", "AA"))

top5 <- top5 %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"),
         arr_type = ifelse(arr_delay < 5, "on time", "delayed"))

table(top5$dep_type)/nrow(top5)

table(top5$dep_type, top5$arr_type)
dtable <-  table(top5$dep_type, top5$arr_type)
proportions(dtable)
proportions(dtable,1)
proportions(dtable,2)

treeDiag(c("Departure", "Arrival"),
         out1 = c("delayed", "on time"),
         out2 = c("delayed", "on time"),
         p1 = c(0.3222, 0.6778),
         p2 = list(c(0.7552, 0.2448),c(0.1558, 0.8442))
         )

0.2433/(0.2433+0.1056)
0.5722/(0.5722+0.0789)


library(caret)
sensitivity(factor(top5$dep_type), factor(top5$arr_type))
specificity(factor(top5$dep_type), factor(top5$arr_type))


top5 <- top5 %>%
  mutate(dep_type = cut(dep_delay, 
                        breaks = c(min(dep_delay), 5, 45, max(dep_delay)+1), 
                        labels = c("on time", "delayed", "very delayed"),
                        right = FALSE),
         arr_type = cut(arr_delay, 
                        breaks = c(min(arr_delay), 5, 45, max(arr_delay)+1), 
                        labels = c("on time", "delayed", "very delayed"),
                        right = FALSE))

ggplot(top5, aes(x=dep_type, fill=arr_type))+
  geom_bar(position = "fill")+
  facet_grid(~carrier)+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="",y="")

top5 <- top5 %>%
  mutate(dep_type = cut(dep_delay, 
                        breaks = c(min(dep_delay), 5, 45, max(dep_delay)+1), 
                        labels = c("on time", "delayed", "very delayed"),
                        right = FALSE),
         time_type = cut(air_time, 
                        breaks = c(min(arr_delay), 60, 200, max(arr_delay)+1), 
                        labels = c("short", "normal", "long"),
                        right = FALSE))

ggplot(top5, aes(x=dep_type, fill=time_type))+
  geom_bar(position = "fill")+
  facet_grid(~carrier)+
  scale_fill_brewer(palette = "Accent")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="",y="")

