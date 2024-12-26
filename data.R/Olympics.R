library(tidyverse)
hept = read_csv('hept.csv')
colnames(hept)
view(hept)



library(GGally)
ggpairs(select(hept,-name), aes(colour=year))



model=lm(points~longjump,data = hept)
summary(model)


ggplot(hept, aes(x=points, y=longjump)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)


library(ggfortify)
autoplot(model, data=hept, colour='year')



model=lm(points~sprint+longjump+javelin+year,data = hept)
summary(model)


library(ggfortify)
autoplot(model, data=hept, colour='year',label.n=39, label.repel=TRUE)

















#Doing it again for Practice

library(tidyverse)
hept = read.csv("hept.csv")


#a-part
library(GGally)
ggpairs(select(hept,-name),aes(col=year))


#b-part
model=lm(points~longjump,data = hept)
summary(model)

ggplot(hept ,aes(x=longjump,y=points)) +
  geom_point()+ geom_smooth(method = lm,se=FALSE)


#c-part
autoplot(model,data = hept,col = 'year')
autoplot(model, data=hept, colour='year', label.n=39, label.repel=TRUE)

#d-part
model=lm(points~sprint+longjump+javelin+year,data = hept)
summary(model)

#re-doing d part 
model=lm(points~sprint+longjump+javelin,data = hept)
summary(model)


autoplot(model, data=hept, colour='year', label.n=39, label.repel=TRUE)