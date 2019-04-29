### Exploring data more

mine = c(5096,
         5175,
         5225,
         5293,
         5476,
         5558,
         5587,
         5611)

robins = c(5320,
           5333,
           5421,
           5463,
           5578,
           5601,
           5603,
           5617)


#from analysis
d2 <- d %>% 
  mutate(DIM = as.numeric(DIM),
         MY.m1 = as.numeric(as.character(MY.m1)),
         MY.m2 = as.numeric(as.character(MY.m2)),
         MY = as.numeric(as.character(MY)),
         Period = as.factor(Period),
         Top.Dress = as.factor(Top.Dress),
         TMR.Adjustment = as.numeric(TMR.Adjustment),
         NEl.Target = as.numeric(NEl.Target),
         TMR.Target = as.numeric(TMR.Target),
         CG.Target = as.numeric(CG.Target),
         SBM.Target = as.numeric(SBM.Target),
         GH.Target = as.numeric(GH.Target),
         Top.Dress.pct = (CG.Target+SBM.Target+GH.Target)/TMR.Target) %>%
  # 2x single milk measurements
  mutate(MY.adj = ifelse(is.na(MY.m1+MY.m2),
                         (ifelse(is.na(MY.m1),0,MY.m1)+ifelse(is.na(MY.m2),0,MY.m2))*2, MY),
         MY.adj = replace(MY.adj, MY.adj == 0, NA)) %>% 
  group_by(Animal.ID) %>%
  mutate(MY.shift = lead(MY.adj, 1),
         Refusal.shift= lead(Refusal..lbs.,1),
         Eff = (Total.Actual-Refusal.shift)/MY.shift,
         Eff.max = Eff/max(Eff,na.rm = T),
         Eff.max.scale = scale(Eff.max)) %>%
  filter(Eff.max.scale>-5, Eff.max.scale <5) %>% #remove +- 5 sd points in Eff max
  data.frame()

d2 %>%
  group_by(Animal.ID) %>%
  mutate(MY.pct.max = MY.adj/max(MY.adj, na.rm = T)) %>%
  ggplot(aes(Date, Eff.max, color = Top.Dress)) +
  geom_point() + 
  facet_wrap(~Animal.ID)


### Looking at adj refusals 
d2$pred.group <- ifelse(d2$Animal.ID %in% mine, "doug",
                        ifelse(d2$Animal.ID %in% robins, 'robin', 'control'))


## by animal and TD
d2 %>%
  group_by(Animal.ID) %>%
  # mutate(ref.pct.max = Refusals.Adjusted.For.Target/max(Refusals.Adjusted.For.Target, na.rm = T)) %>%
  ggplot(aes(Date, Refusals.Adjusted.For.Target, color = Top.Dress)) +
  geom_point() + ylim(c(-5,NA)) +
  facet_wrap(~Animal.ID)

d2 %>%
  group_by(Date, pred.group) %>%
  summarise(Refusals = mean(ifelse(Refusals.Adjusted.For.Target>0, Refusals.Adjusted.For.Target,0), na.rm = T)) %>%
  filter(Refusals != 0) %>%
  ggplot(aes(Date, Refusals, color = pred.group)) +
  geom_line(size = 0.5)+theme_minimal()

#boxplot by group
d2 %>%
  filter(Date > "2019-03-26") %>%
  ggplot(aes(pred.group,Refusals.Adjusted.For.Target)) +
  geom_boxplot()
