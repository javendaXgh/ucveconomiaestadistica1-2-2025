#####
library(openintro)
library(ggplot2)
library(dplyr)

# curva sesgada
# añadir media, mediana, sd
hist(age_at_mar$age)

ggplot(data=age_at_mar, 
       aes(x=age))+
  geom_histogram(bins = 8)

# relacion entre variables
plot(ames$price~ames$Year.Built)

plot(ames$Year.Built~ames$price)

View(head(ames))
unique(ames$Street)

# representar en los graficos las medias, medianas y las sd
hist(ames$price)
hist(ames$area)
dim(ames)

unique(ames$Overall.Qual)

ames_select <- ames%>%
  select(area, price, Kitchen.Qual, MS.SubClass,Overall.Qual, Year.Built,TotRms.AbvGrd)%>%
  filter(Kitchen.Qual!='TA')%>%
  sample_n(5)

names(ames_select)
ames_select
dim(ames_select)
area: area en mts2
# price: precio de venta
# Kitchen.Qual: calidad de la cocina
# MS.SubClass: Identifica el tipo de vivienda objeto de la venta.
# Overall.Qual: califación emitida sobre  el material y el acabado general de la casa, del 1 al 10
# year.build: año de construcción
# TotRms.AbvGrd: total habitaciones

# sobre este completar la tabla de frecuencia
table(ames$Overall.Qual)
table(ames$Overall.Cond)

ames_select2 <- ames%>%
  sample_n(500)
table(ames_select2$Overall.Qual)

datos_boxplot <- gapminder::gapminder%>%
  filter(year== min(year) & continent %in% c('Africa','Europe') )
# datos_boxplot
ggplot(data = datos_boxplot,
       mapping = aes(x=continent, 
                     y= lifeExp)) +
  geom_boxplot(fill= "#fb8b24",
               color='#00a6fb',
               outlier.colour = 'red')


read.csv('data/indices.csv', 
                    sep = ';')%>%
  janitor:: clean_names()

library(dplyr)
rowMeans()
read.csv('data/indices.csv', 
         sep = ';')%>%
  janitor:: clean_names()%>%
  as_data_frame()%>%
  select(1,3,5,7)%>%
  rowwise()%>%
  mutate(promedio= round(mean(c(rel, rel_1, rel_2)),0))

apply(val_ind, 2, class)

View(val_ind)
t1 <- table(ames$Overall.Qual)


# identificar usar en segunda versi'on de curso
# usar
transform(t1, 
          cumulative = cumsum(Freq), 
          relative = prop,
          table(Freq))

?transform
# ver referencia p'agina 13 libro Statistics for Data Science Kaptein
# investigar funcion tabulate
