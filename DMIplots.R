
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
  group_by(Animal.ID) %>%
  mutate(max.ref = Refusals.Adjusted.For.Target/max(Refusals.Adjusted.For.Target)) %>%
  group_by(Day) %>%
  summarise(ref = mean(max.ref, na.rm = T),n = n()) %>%
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
  mutate(max.target = Total.Target/max(Total.Target),
         max.ref = Refusals.Adjusted.For.Target/max(Refusals.Adjusted.For.Target)) %>%
  ungroup() %>%
  group_by(Day) %>%
  summarise(target.day = mean(max.target, na.rm = T),
            eff.day = mean(Eff.max, na.rm = T),
            ref.day = mean(max.ref, na.rm = T),n = n()) %>%
  ggplot() +
  geom_line(aes(Day, eff.day), color = "blue", size = 1)+
  geom_line(aes(Day, target.day), color = "red", size = 1)+
  geom_line(aes(Day, ref.day), color = "green", size = 1)+
  ylab("Percent")+ylim(0,1)

######### Efficiency compared to Target Fed per Animal
d2 %>%
  filter(Day >= 0 , Day < 9) %>%
  group_by(Animal.ID) %>%
  mutate(max.target = Total.Target/max(Total.Target),
         max.ref = Refusals.Adjusted.For.Target/max(Refusals.Adjusted.For.Target)) %>%
  ungroup() %>%
  group_by(Animal.ID, Day) %>%
  summarise(target.day = mean(max.target, na.rm = T),
            eff.day = mean(Eff.max, na.rm = T),
            ref.day = mean(max.ref, na.rm = T),n = n()) %>%
  ggplot() +
  geom_line(aes(Day, eff.day), color = "blue", size = 1)+
  geom_line(aes(Day, target.day), color = "red", size = 1)+
  geom_line(aes(Day, ref.day), color = "green", size = 1)+
  ylab("Percent") + facet_wrap(~Animal.ID)

######### Efficiency compared to Target Fed per TD
d2 %>%
  filter(Day >= 0 , Day < 9) %>%
  group_by(Animal.ID) %>%
  mutate(max.target = Total.Target/max(Total.Target),
         max.ref = Refusals.Adjusted.For.Target/max(Refusals.Adjusted.For.Target)) %>%
  ungroup() %>%
  group_by(Top.Dress, Day) %>%
  summarise(target.day = mean(max.target, na.rm = T),
            eff.day = mean(Eff.max, na.rm = T),
            ref.day = mean(max.ref, na.rm = T),n = n()) %>%
  ggplot() +
  geom_line(aes(Day, eff.day), color = "blue", size = 1)+
  geom_line(aes(Day, target.day), color = "red", size = 1)+
  geom_line(aes(Day, ref.day), color = "green", size = 1)+
  ylab("Percent") + facet_wrap(~Top.Dress)



############ How often did we move a TMR adj up, just to move it back?
############ about 27% of the time
d2 %>%
  group_by(Animal.ID) %>%
  arrange(Date) %>%
  mutate(up_yest = ifelse(TMR.Adjustment > lag(TMR.Adjustment,1),1,
                          ifelse(TMR.Adjustment < lag(TMR.Adjustment),-1, 0))) %>%
  mutate(went_back = ifelse((up_yest+lag(up_yest) != abs(up_yest)+abs(lag(up_yest))) &
                              abs(up_yest)+abs(lag(up_yest)) == 2, 1, 0)) %>%
  data.frame() %>%
  summary()

         