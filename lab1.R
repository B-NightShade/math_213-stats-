library(ggplot2)
library(dplyr)
library(openintro)

data(nycflights)

names(nycflights)

str(nycflights)

nycflights$month <- factor(nycflights$month)
str(nycflights)

#exercise 1
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram()

ggplot(data = nycflights, aes(x = dep_delay))+
  geom_histogram(binwidth = 15)

ggplot(data = nycflights, aes(x = dep_delay))+
  geom_histogram(binwidth = 150)



den_flights <- nycflights %>%
  filter(dest == "DEN")

ggplot(data = den_flights, aes(x=dep_delay))+
  geom_histogram()

den_flights %>%
  summarise(mean_dd  = mean(dep_delay),
            median_dd = median(dep_delay),
            n = n())

#exercise 2
sfo_feb_flights <- nycflights %>%
  filter(dest == "SFO", month == 2)

str(sfo_feb_flights)

#exercise 3
ggplot(data = sfo_feb_flights, aes(arr_delay))+
  geom_histogram(binwidth = 15)

ggplot(data = sfo_feb_flights, aes(arr_delay))+
  geom_histogram(binwidth = 10)

sfo_feb_flights %>%
  summarise(mean_dd = mean(arr_delay),
            median_dd = median(arr_delay),
            sd_dd = sd(arr_delay),
            n = n())


sfo_feb_flights %>%
  group_by(origin) %>%
  summarise(median_dd = median(dep_delay), 
            iqr_dd = IQR(dep_delay),
            n_flights = n())

#exercise 4
sfo_feb_flights %>%
  group_by (carrier) %>%
  summarise(median_dd = median(arr_delay),
            iqr_dd = IQR(arr_delay),
            n_flights = n())


nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))

ggplot(nycflights, aes(x=month, y=dep_delay))+
  geom_boxplot()

ggplot(nycflights, aes(x=month, y=dep_delay))+
  geom_boxplot()+
  scale_y_log10()

ggplot(nycflights, aes(x=month, y=dep_delay))+
  geom_boxplot()+
  coord_cartesian(ylim = c(-15, 60))


#exercise 5
nycflights %>%
  group_by(month) %>%
  summarise(median_dd = median(dep_delay)) %>%
  arrange(desc(median_dd))



nycflights$dep_type <- ifelse(nycflights$dep_delay < 5, "on time",
                              "delayed")

nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time")/n()) %>%
  arrange(desc(ot_dep_rate))

#exercise 6
ggplot(data = nycflights, aes (x = origin, fill = dep_type))+
  geom_bar()

nycflights %>%
  group_by(origin) %>%
  summarise(countot_dd = sum(dep_type == "on time"),
            countd_dd = sum(dep_type == "delayed"),
            n_flights = n())


#exercise 7
nycflights$avg_speed <- (nycflights$distance)/(nycflights$air_time / 60)

#exercise 8
ggplot(data = nycflights, aes(x = avg_speed, y = distance))+
  geom_point()

#exercise 9
nycflights %>%
  filter(carrier == "AA" | carrier == "DL" | carrier == "UA") %>%
  ggplot(aes(x = dep_delay, y = arr_delay, color = carrier))+
  geom_point()


