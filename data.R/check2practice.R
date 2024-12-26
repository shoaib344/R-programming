library(tidyverse)
penguins = read_csv("penguins_subset.csv")
view(penguins)


library(GGally)
ggpairs(penguins, aes(colour=sex))



model = lm(body_mass~bill_depth, data=penguins)
summary(model)



ggplot(penguins, aes(x=bill_depth, y=body_mass)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)



library(ggfortify)
autoplot(model, data=penguins, colour='species')

model = lm(body_mass~bill_depth+species, data=penguins)
summary(model)























#Doing all again for practice
library(tidyverse)
penguins = read.csv("penguins_subset.csv")
view(penguins)

#a-part
library(GGally)
ggpairs(penguins, aes(col =  sex))


#b-part
model=lm(body_mass~bill_depth,data = penguins)
summary(model)

ggplot(penguins,aes(x= bill_depth,y=body_mass))+
  geom_point() + geom_smooth(method = lm,se=FALSE)

#c-part
library(ggfortify)
autoplot(model,data= penguins,col="species")


#d-part
model=lm(body_mass~bill_depth+species,data = penguins)
summary(model)

ggplot(penguins,aes(x= bill_depth,y=body_mass))+
  geom_point() + geom_smooth(method = lm,se=FALSE)



#d-part
library(ggfortify)
autoplot(model,data= penguins,col="species")
