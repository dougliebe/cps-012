
library(tidyverse)

### Predicting Intake

head(d2)

######### Total Refusals per Date
d2 %>%
  group_by(Date) %>%
  summarise(ref = sum(Refusals.Adjusted.For.Target, na.rm = T)) %>%
  ggplot(aes(Date, ref)) +
  geom_line() + geom_smooth()

######### Avg Refusals by Day
d2 %>%
  filter(Day >= 0 , Day < 9) %>%
  group_by(Day) %>%
  summarise(ref = mean(Refusals.Adjusted.For.Target, na.rm = T),n = n()) %>%
  ggplot() +
    geom_line(aes(Day, ref))

######### Avg Efficiency by Day
d2 %>%
  filter(Day >= 0 , Day < 9) %>%
  group_by(Day) %>%
  summarise(ref = mean(Eff.max, na.rm = T), n = n()) %>%
  ggplot(aes(Day, ref)) +
  geom_line()


######### Efficiency compared to Target Fed
d2 %>%
  filter(Day >= 0 , Day < 9) %>%
  group_by(Animal.ID) %>%
  mutate(max.target = Total.Target/max(Total.Target)) %>%
  ungroup() %>%
  group_by(Day) %>%
  summarise(target.day = mean(max.target, na.rm = T),
            eff.day = mean(Eff.max, na.rm = T), n = n()) %>%
  ggplot() +
  geom_line(aes(Day, eff.day), color = "blue")+
  geom_line(aes(Day, target.day), color = "red")+
  ylab("Percent") 

######### Efficiency compared to Target Fed per Animal
d2 %>%
  filter(Day >= 0 , Day < 9) %>%
  group_by(Animal.ID) %>%
  mutate(max.target = Total.Target/max(Total.Target)) %>%
  ungroup() %>%
  group_by(Animal.ID, Day) %>%
  summarise(target.day = mean(max.target, na.rm = T),
            eff.day = mean(Eff.max, na.rm = T), n = n()) %>%
  ggplot() +
  geom_line(aes(Day, eff.day), color = "blue")+
  geom_line(aes(Day, target.day), color = "red")+
  ylab("Percent") + facet_wrap(~Animal.ID)