set.seed(123)

library(tidyverse)
altura <- round(c(rnorm(32, mean = 168, sd = 8), 190))
peso <- round(rnorm(33, mean = 70, sd = 12))
sexo <- sample(c("M", "F"), 33, replace = TRUE)
datos_curso <- data.frame(altura, peso, sexo)



datos_curso
datapasta::vector_paste()
mean(c(64L, 64L, 80L,  67L, 67L, 45L, 105L, 75L, 70L, 65L))


altura <- round(c(rnorm(149, mean = 168, sd = 8), 210))
peso <- round(rnorm(150, mean = 70, sd = 12))
sexo <- sample(c("M", "F"), 150, replace = TRUE)
datos <- data.frame(altura, peso, sexo)


datos_curso$altura
min(datos_curso$altura) # altura mínima

max(datos_curso$altura) # altura máxima

range(datos_curso$altura) # rango de alturas

(max( datos_curso$altura)- min(datos_curso$altura)) /6

seq(min(datos_curso$altura), 
    max(datos_curso$altura),
    8) # crear secuencias con brincos de 8

# tiene 6 grupos o intervalos

# library(tidyverse)
datos_curso %>%
  mutate(
    # Creamos los cortes de nuevo
    intervalo_cat = cut(altura, 
                        breaks = seq(from= 140, 
                                     to= 220,
                                     by = 8), 
                        right = FALSE)
  ) %>%
  group_by(intervalo_cat) %>%
  summarise(
    ni = n(), # frecuencia absoluta
  ) %>%
  mutate(
    # fi = ni / sum(ni), #frecuencia relativa
    fr = ni / sum(ni),
    Ni = cumsum(ni), # frecuencia acumulada
    Fra = cumsum(ni / sum(ni)),# frecuencia acumulada relativa
    # Extraemos numéricamente los límites para calcular el punto medio
    limite_inf = as.numeric(sub("\\[([0-9]+),.*", "\\1", intervalo_cat)),
    limite_sup = as.numeric(sub(".*,([0-9]+)\\)", "\\1", intervalo_cat)),
    marca_clase = (limite_inf + limite_sup) / 2,
  )

media_altura <- mean(datos_curso$altura)
mediana_altura <- median(datos_curso$altura)

mediana_altura
ggplot(datos_curso, 
       aes(x = altura)) +
  geom_histogram(binwidth = 5, 
                 fill = "lightblue", 
                 color = "black") +
  geom_vline(aes(xintercept = media_altura), 
             color = "red", 
             linetype = "dashed", 
             size = 2) +
  geom_vline(aes(xintercept = mediana_altura), 
             color = "green", 
             linetype = "dashed", 
             size = 1) +
  labs(title = "Histograma de Altura con Media y Mediana",
       x = "Altura",
       y = "Frecuencia") +
  theme_minimal()


datos_curso%>%
  group_by(altura)%>%
  summarise(ni = n(),
            fr = ni/nrow(datos_curso))%>%
  mutate(Ni = cumsum(ni),
         Fra = cumsum(ni / sum(ni)))



summary_stats <- datos_curso %>% 
  summarise(
    media = mean(altura),
    mediana = median(altura),
    Q1 = quantile(altura, 0.25),
    Q3 = quantile(altura, 0.75),
    RIC = Q3 - Q1
  )

class(summary_stats)

library(gt)
library(gtExtras)
gt(summary_stats)%>%
  gt_theme_excel()


cut(altura, 
    breaks = seq(from= 140, 
                 to= 220,
                 by = 8), 
    right = FALSE)

datos_curso %>%
  mutate(
    # Creamos los cortes de nuevo
    intervalo_cat = cut(altura, 
                        breaks = seq(from= 140, 
                                     to= 220,
                                     by = 8), 
                        right = FALSE)
  )


estadisticos <- datos_curso %>%
  summarise(
    Media = mean(altura),
    Mediana = median(altura),
    Q1 = quantile(altura, 0.25),
    Q3 = quantile(altura, 0.75),
    IQR = IQR(altura),
    Min = min(altura),
    Max = max(altura)
  )

q1 <- estadisticos$Q1
q3 <- estadisticos$Q3

ric_vals <- datos_curso %>% 
  filter(altura >= q1, 
         altura <= q3)

ric_vals %>% 
  gt()

lim_inf <- q1 - 1.5 * estadisticos$RIC
lim_sup <- q3 + 1.5 * estadisticos$RIC

outliers <- datos_curso %>% 
  filter(altura < lim_inf | altura > lim_sup)


gt(outliers)%>%
  gt_theme_excel()



##########


x <- c(10, 10, 10, 12, 12, 14, 15, 16, 17, 18, 19, 20) 

# 1. Moda (Mo): 10
Mo <- 10

# 2. Media (x_bar): 169 / 11 = 15.36
Media <- mean(x)

# 3. Desviación Estándar (s): 4.88
s <- sd(x)

# 4. Cálculo Sk2
Sk1 <- (Media - Mo) / s

########
x <- c(2, 5, 6, 6, 7, 7, 8, 10, 11, 18)

# 1. Mediana (Me): (7 + 8) / 2 = 7.5
Me <- median(x)

# 2. Media (x_bar): 80 / 10 = 8
Media <- mean(x)

# 3. Desviación Estándar (s): 4.108
s <- sd(x)

# 4. Cálculo Sk1
Sk2 <- 3*(Media - Me) / s

#cat("Media:", round(Media, 2), "| Moda:", Mo, "| s:", round(s, 2), "\n")
#cat("Resultado Sk1:", round(Sk2, 4))
# Interpretación: 1.21xx ( Asimetría Positiva


