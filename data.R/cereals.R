library(tidyverse)
cereals= read.csv("cereals.csv")
view(cereals)

library(GGally)
ggpairs(select(cereals,-name),aes(colour=manufacturer))

model= lm(energy~dietary_fibre, data = cereals)
summary(model)

ggplot(cereals, aes(x=dietary_fibre,y=energy))+
  geom_point() + 
  geom_smooth(method =lm,se=FALSE)


library(ggfortify)
autoplot(model, data=cereals, colour="manufacturer")

model= lm(energy~dietary_fibre+protein+carbohydrate, data = cereals)
summary(model)

library(ggfortify)
autoplot(model, data=cereals, colour="manufacturer")


























#doing it again for practice
library(tidyverse)
cereals = read.csv("cereals.csv")
view(cereals)

#a-part
library(GGally)
ggpairs(select(cereals,-name),aes(col=manufacturer))



#b-part
model=lm(energy~dietary_fibre ,data = cereals)
summary(model)

ggplot(cereals,aes(x=dietary_fibre,y=energy))+
  geom_point() + geom_smooth(method=lm,se =FALSE)


#c-part
library(ggfortify)
autoplot(model, data=cereals, colour="manufacturer",label.n = 21,label.repel=TRUE)








