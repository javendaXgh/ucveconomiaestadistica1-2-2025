library(ggplot2)
library(openintro)

####
loan50$interest_rate
max(loan50$interest_rate)
min(loan50$interest_rate)

plot(x= loan50$interest_rate, 
     y=rep(0,length(loan50$interest_rate)),
     main='Plot de puntos: Tasa de interés',
     xlab='%',
     ylab='',
     pch= 1)

par(new=TRUE)
points(mean(loan50$interest_rate),
       -.05, 
       pch = 17, 
       col = "red",
       cex = 2)

ggplot(loan50, 
       aes(x = interest_rate)) + 
  geom_dotplot()+
  scale_y_continuous(NULL, breaks = NULL)

ggplot(loan50, 
       aes(x = interest_rate)) + 
  geom_dotplot()+
  scale_y_continuous(NULL, breaks = NULL)+
  annotate("pointrange",
           x = mean(loan50$interest_rate),
           y = -.01, 
           ymin = -.01,
           ymax = .2,
           colour = "red")

# sesgo a la derecha
ggplot(data= loan50, aes(x=interest_rate))+
  geom_histogram(bins=8, 
                 fill='blue')
####################################################

set.seed(5313)
babies_subset <- babies%>%
  sample_n(50)

babies_subset$age
min(babies_subset$age)
max(babies_subset$age)
max(babies_subset$age)-min(babies_subset$age)

ggplot(data= babies_subset) + 
  geom_dotplot(aes(x = age))+
  scale_y_continuous(NULL, breaks = NULL)

ggplot(data= babies_subset) + 
  geom_dotplot(aes(x = age))+
  scale_y_continuous(NULL, breaks = NULL)+
  annotate("point",
           x = mean(babies_subset$age),
           y = -.01, 
           col = "red")


ggplot(data= babies_subset) +
  geom_dotplot(aes(x = age),
               fill = "steelblue") + 
  scale_y_continuous(NULL, breaks = NULL)+
  annotate('point',
           x=mean(babies_subset$age),
           y=-.01,
           col='red')


ggplot(data=babies_subset, 
       aes(x=age))+
  geom_histogram()

ggplot(data=babies_subset, 
       aes(x=age))+
  geom_histogram(bins = 15)

ggplot(data=babies_subset, 
       aes(x=age))+
  geom_histogram(bins = 10)

ggplot(data=babies, 
       aes(x=age))+
  geom_histogram()

hist(babies$age, breaks = 21)



#### simulacion
set.seed(5431) # generación de semi
# generar muestra aleatoria simulada para 12 personas que tienen entre 0 y 4 hermanos

calificacion <- sample(c('⭐️',
                          '⭐️⭐️',
                          '⭐️⭐️⭐️',
                          '⭐️⭐️⭐️⭐️',
                          '⭐️⭐️⭐️⭐️⭐️')
                       , size= 30, 
                       replace= TRUE)
calificacion
# crear las clases (intervalos) para 1 y 2\\

table(calificacion)%>%
  as_data_frame()%>%
  rename(fi=n)%>%
  # rename(Xi=1)%>%
  mutate(hi= (fi/sum(fi))*100)%>%
  mutate(Fi= cumsum(fi))%>%
  mutate(Hi= cumsum(hi))

table(calificacion)%>%
  as_data_frame()%>%
  rename(fi=n)%>%
  # rename(Xi=1)%>%
  mutate(hi= (fi/sum(fi))*100)%>%
  mutate(Fi= cumsum(fi))%>%
  mutate(Hi= cumsum(hi))

hist(calificacion)
# caso 1

cdad_hermanos <- sample(0:4, 12 ,replace=TRUE)
table(cdad_hermanos)

class(freq_table)
freq_table%>%
  as_data_frame()

# se expresan en intervalos o en clases
table(cdad_hermanos)%>%
  as_data_frame()%>%
  rename(clases= cdad_hermanos, fi= n)%>%
  mutate(clases=as.numeric(clases))%>%
  mutate(pto_medio = (lag(clases)+clases)/2)
  mutate(clases=as.numeric(clases),
         pto_medio = (lag(clases)+clases)/2,
         clases2= paste(lag(clases),'-',clases),
         hi= (fi/sum(fi))*100)
  # group_by(clases)%>%
  # mutate()%>%
  # mutate()%>%
  slice(-1)


  

mutate(clases=as.numeric(clases),
         pto_medio = (lag(clases)+clases)/2,
         clases2= paste(lag(clases),'-',clases),
         hi= (fi/sum(fi))*100)%>%
  # group_by(clases)%>%
  # mutate()%>%
  # mutate()%>%
  slice(-1)



cdad_hermanos%>%
  as_data_frame()%>%
  group_by(values) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))
# caso 2
muestra_clases <- hist(cdad_hermanos, breaks=4, plot=FALSE)
muestra_clases
