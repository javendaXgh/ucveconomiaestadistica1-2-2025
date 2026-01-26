library(openintro)
library(dplyr)
library(gapminder)
library(googlesheets4)
library(readxl)

https://javenda.shinyapps.io/gapminder2007/
https://javenda.shinyapps.io/app_recolecion/


# https://docs.google.com/spreadsheets/d/1aj4vZmvJtIiNy4ET1F0kMJXsqaYeyuEIFQ0tuHKI3YU/edit?gid=0#gid=0
df_curso <-  read_sheet("https://docs.google.com/spreadsheets/d/1aj4vZmvJtIiNy4ET1F0kMJXsqaYeyuEIFQ0tuHKI3YU/edit?usp=sharing")

quiz1 <- read_excel('_admin_materiales/notas.xlsx')%>%
  select(1,4)

nrow(quiz1)
View(df_curso)

gap_2007 <- gapminder%>%
  filter(year==2007)


write.csv(df_curso, 
          file = "data/df_curso.csv",
          row.names = FALSE)
write.csv(quiz1, 
          file = "data/quiz1.csv",
          row.names = FALSE)

write.csv(gap_2007, 
          file = "data/gap_2007.csv",
          row.names = FALSE)



summary(quiz1$quiz1)


quiz1%>%
  ggplot(aes(x=quiz1,y=0))+
  geom_point(col='blue')




View(gap_2007)

hist(gap_2007$pop)

boxplot(gap_2007$lifeExp)

variable_estudio <- gap_2007$lifeExp
nombre_variable <- "Esperanza de vida (años)"

# hacer dotplot
dotchart(variable_estudio)

# hacer dotplot stacked
library(ggplot2)
ggplot(data=data.frame(variable_estudio), 
       aes(x=variable_estudio)) +
  geom_dotplot() 

ggplot(data=data.frame(variable_estudio),
       aes(x=variable_estudio)) +
  geom_histogram(binwidth=1, 
               fill="blue") +
  xlab(nombre_variable)

ggplot(data=gap_2007, 
       aes(x=variable_estudio)) +
  geom_dotplot(binwidth=2, 
               fill="blue") +
  xlab(nombre_variable) +
  ylab("Frecuencia") 



ggplot(data=gap_2007,
       aes(x=lifeExp,
           fill=continent)) +
  geom_dotplot(binwidth=1) +
  xlab(nombre_variable) +
  ylab("Frecuencia") +
  ggtitle("Dotplot de la variable de estudio ")

ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5) + # 'binwidth' agrupa los puntos
  labs(title = "Dotplot de Distribución de MPG",
       x = "Millas por Galón (MPG)",
       y = "Frecuencia") +
  theme_minimal()






# hacer histograma
hist(variable_estudio)
# hacer boxplot
boxplot(variable_estudio)
# hacer boxplot con ggplot2
ggplot(data=data.frame(variable_estudio), aes(y=variable_estudio)) +
  geom_boxplot(fill="lightblue", color="black") +
  ylab("Variable de estudio") +
  ggtitle("Boxplot de la variable de estudio")
# hacer boxplot horizontal
boxplot(variable_estudio, horizontal=TRUE)
# hacer boxplot con ggplot2 horizontal
ggplot(data=data.frame(variable_estudio), aes(x=variable_estudio)) +
  geom_boxplot(fill="lightblue", color="black") +
  xlab("Variable de estudio") +
  ggtitle("Boxplot horizontal de la variable de estudio")

# hacer tabla de frecuencias
table(variable_estudio)
# hacer tabla de frecuencias con porcentajes
prop.table(table(variable_estudio)) * 100



