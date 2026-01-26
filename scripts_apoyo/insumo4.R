library(readxl)
library(tidyverse)
library(openintro)
library(DT)
library(gapminder)

gapminder%>%
  filter(year==2007)%>%
  ggplot(aes(x=gdpPercap, y= lifeExp, colour = continent))+
  geom_point()

quiz1 <- read_excel('_admin_materiales/notas.xlsx')%>%
  select(1,4)
nrow(quiz1)

summary(quiz1$quiz1)


quiz1%>%
  ggplot(aes(x=quiz1,y=0))+
  geom_point(col='blue')



datatable(quiz1)
hist(quiz1$quiz1) 

openintro::ames


View(head(openintro::ames))

ames_subset <- ames%>%
  sample_n(200)
View(ames_subset)

ames_subset%>%
  ggplot(aes(x=area, y=price))+
  geom_point()

ames_subset%>%
  sample_n(200)%>%
  ggplot(aes(x=price, y=area))+
  geom_point()


#BsmtFin.SF.1
#Mas.Vnr.Area
ames_subset%>%
  sample_n(100)%>%
  ggplot(aes(x=area, 
             y=BsmtFin.SF.1))+
  geom_point()
  