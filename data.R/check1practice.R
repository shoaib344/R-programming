library(tidyverse)
animals = read.csv('animals.csv')
colnames(animals)

#a-part
animals %>%
  group_by(System) %>%
  summarise(n())

# OR
animals %>%
  group_by(System) %>%
  tally()
animals %>%
  count(System)

#b-part
hippo = animals %>% filter(Genus=="Hippopotamus") %>% select(Country) %>% distinct()
hippo

# OR

animals %>%
 filter(Genus=='Hippopotamus') %>%
 select(Country) %>%
 distinct()



#c-part
animals %>%
  filter(T_realm!='NULL') %>%
  ggplot(aes( y=T_realm,fill=Class)) +
  geom_bar() +
  ggtitle('Frequency of Terrestrial Realm by Animal Class')













#d-part
animals %>%
  filter(Common_name=='Green turtle') %>%
  ggplot(aes(x=Longitude,y=Latitude ,colour= Region)) +
  geom_point()+
  ggtitle('Location of Green turtle')




#e-part
animals %>%
  filter(Common_name=='Green turtle') %>%
  filter(Latitude==min(Latitude)) %>%
  select(Location)





