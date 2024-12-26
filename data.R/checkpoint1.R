library(tidyverse)
land = read_csv('landcover.csv')
 
#Land 

#a-part
ncol(land)
nrow(land)
#OR
dim(land)

#b-part
unique(land$Class)

#OR

land %>%
  select(Class) %>%
  unique()




#c-part

# Consider only 2018
landsub = filter(land,Year==2018)
# Total area for each value of Country
landsub %>%
  group_by(Country) %>%
  summarise(total_area=sum(Area))

# Total area for each value of Class
landsub %>%
  group_by(Class) %>%
  summarise(total_area=sum(Area)) %>%
  arrange(desc(total_area))

#d-part
land %>%
  pivot_wider(names_from=Year, values_from=Area) %>%
  rename(year2018='2018', year2008='2008') %>%
  ggplot(aes(x=year2018-year2008,y=Class,fill=Country))+
  geom_col(position="dodge") +
  ggtitle('Change in landcover between 2008 and 2018')



#Dino
library(tidyverse)
dino = read_csv('jurassic.csv')
view(dino)

  
#a-part
nrow(dino)
colnames(dino)

view(dino)
#b-part
dino %>%
  group_by(diet) %>%
  summarise(count=n())


dino %>%
  group_by(diet) %>%
  summarise(n())



#OR

dino %>%
  count(diet)




#c-part
newdino = separate(dino, length, c('L', NA), sep='m', convert=TRUE)
newdino


#d-part
ggplot(newdino,aes(x=L,y=type))+
  geom_boxplot() + ggtitle("Distributions of dino's length by type")

#e-part
ggplot(newdino ,aes(x=type,fill=diet)) +
  geom_bar(position="dodge")




#Lego
library(tidyverse)
lego = read_csv('legosets.csv')

#a-part
nrow(lego)
colnames(lego)

#b-part

lego %>% 
  group_by(theme) %>%
  summarise(n=n()) %>%
  arrange(desc(n))


#OR
lego %>% count (theme) %>%
  arrange(desc(n))


#c-part
newlego = mutate(lego, decade=as.character(year-year%%10))

#d-part
ggplot(newlego,aes(x=minifigures,y=decade)) +
  geom_boxplot() + ggtitle("Distribution of number of lego by decade ")


#e-part
harry = filter(lego,theme == "Harry Potter")
harry %>% group_by(subtheme) %>%
  summarise(mean(pieces))
# OR 

harry = filter(lego,theme=='Harry Potter')
harry %>%
  group_by(subtheme) %>%
  summarise(av_pieces=mean(pieces))



qnorm(0.2,232,5)


