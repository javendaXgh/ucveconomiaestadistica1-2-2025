library(dplyr) # manipular datos
library(ggplot2)# crear gráficos



set.seed(123) # para reproducibilidad
# 1. Generar Altura: 32 valores normales + 1 outlier (195 cm)
altura <- round(c(rnorm(32, mean = 168, sd = 8),195))

# 2. Generar Ruido: Un componente aleatorio (varianza) para el peso
#    Esto simula factores no relacionados con la altura (metabolismo, etc.)
ruido_peso <- rnorm(33, mean = 0, sd = 6) # Ruido con media 0 y Desv. Típica 6

# 3. Generar Peso Asociado a Altura:
#    Peso_base = (Altura * Coeficiente) + Intercepto + Ruido
#    Usamos una fórmula simple (ej: IMC aproximado) para ligar el peso a la altura.
peso <- round(0.7 * altura - 50 + ruido_peso)

# 4. Generar Sexo e ID
sexo <- sample(c("M", "F"), 33, replace = TRUE)

# generar consecutivos para identificación de observaciones (filas)
id=1:33

datos_curso <- data.frame(id,
                          altura, 
                          peso, 
                          sexo)


freq_abs <- datos_curso %>%
  group_by(altura) %>%
  summarise(ni = n())


freq_sexo <- datos_curso %>%
  group_by(sexo) %>%
  summarise(ni = n())


freq_rel <- datos_curso %>%
  group_by(altura) %>%
  summarise(
    ni = n(),
    fi = ni/nrow(datos_curso)
  )