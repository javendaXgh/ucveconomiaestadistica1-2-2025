

# Simulación de una distribución mesocúrtica (normal)
n <- 1000  # Tamaño de la muestra
media <- 0
desviacion_estandar <- 1
datos_mesocurtica <- rnorm(n, mean = media, sd = desviacion_estandar)

# Visualización
hist(datos_mesocurtica, 
     main = "Distribución Mesocúrtica (Normal)",
     xlab = "Valores", 
     ylab = "Densidad",
     col = "orange",
     freq = FALSE)
curve(dnorm(x,
            mean = media,
            sd = desviacion_estandar),
      from = min(datos_mesocurtica), 
      to = max(datos_mesocurtica), 
      col = "red", 
      lw=4,
      add = TRUE)
#####
# Simulación de una distribución leptocúrtica (t de Student con df bajos)
df <- 3      # Grados de libertad (bajos para leptocurtosis)
n <- 1000
datos_leptocurtica <- rt(n, df = df)

# Visualización (opcional)
hist(datos_leptocurtica,
     main = "Distribución Leptocúrtica (t de Student, df=3)",
     xlab = "Valores", 
     ylab = "Densidad", 
     col = "orange",
     freq = FALSE)
curve(dt(x, df = df),
      from = min(datos_leptocurtica),
      to = max(datos_leptocurtica), 
      col = "red", 
      lw=3,
      add = TRUE)

######
# Simulación de una distribución platicúrtica (uniforme)
n <- 1000
min_val <- -3
max_val <- 3
datos_platicurtica_uniforme <- runif(n, min = min_val, max = max_val)

# Visualización de la uniforme (opcional)
hist(datos_platicurtica_uniforme, main = "Distribución Platicúrtica (Uniforme)", xlab = "Valores", ylab = "Frecuencia", col = "lightgreen")

# Otra forma de platicúrtica (mezcla de normales)
n <- 1000
datos1 <- rnorm(n/2, mean = -2, sd = 1)
datos2 <- rnorm(n/2, mean = 2, sd = 1)
datos_platicurtica_mezcla <- c(datos1, datos2)

# Visualización de la mezcla de normales (opcional)
hist(datos_platicurtica_mezcla, main = "Distribución Platicúrtica (Mezcla de Normales)", xlab = "Valores", ylab = "Frecuencia", col = "lightsalmon")



#######
# Simulación de una distribución platicúrtica (uniforme)
n <- 1000
min_val <- -3
max_val <- 3
datos_platicurtica_uniforme <- runif(n, min = min_val, max = max_val)

# Visualización de la uniforme
hist(datos_platicurtica_uniforme, main = "Distribución Platicúrtica (Uniforme)", xlab = "Valores", ylab = "Densidad", col = "lightgreen", freq = FALSE)
curve(dunif(x, min = min_val, max = max_val), from = min(datos_platicurtica_uniforme), to = max(datos_platicurtica_uniforme), col = "red", add = TRUE)


#####
# Simulación de una distribución platicúrtica (Beta centrada y escalada)
n <- 1000
alpha <- 2
beta <- 2

# Generar datos beta
datos_beta <- rbeta(n, shape1 = alpha, shape2 = beta)

# Centrar y escalar aproximadamente al rango de -3 a 3
datos_platicurtica_beta <- (datos_beta - 0.5) * 2

# Función de densidad beta centrada y escalada
densidad_beta_escalada <- function(x) {
  y <- (x + 6) / 6 # Desescalar para el argumento de dbeta
  ifelse(y >= 0 & y <= 1, dbeta(y, shape1 = alpha, shape2 = beta) / 6, 0) # Dividir por el factor de escala para la densidad
}

# Visualización
hist(datos_platicurtica_beta,
     main = "Distribución Platicúrtica (Beta Centrada y Escalada)", 
     xlab = "Valores",
     ylab = "Densidad",
     col = "lightgoldenrodyellow",
     freq = FALSE)

curve(densidad_beta_escalada(x), 
      from = min(datos_platicurtica_beta),
      to = max(datos_platicurtica_beta), 
      col = "red",
      lw=3,
      add = TRUE)

# Simulación de una distribución platicúrtica (Beta centrada y escalada - más plana)
# Simulación de una distribución platicúrtica (Beta centrada y escalada - aún más plana)
n <- 1000
alpha <- 10
beta <- 10

# Generar datos beta
datos_beta <- rbeta(n, shape1 = alpha, shape2 = beta)

# Centrar y escalar aproximadamente al rango de -3 a 3
datos_platicurtica_beta_muy_plana <- (datos_beta - 0.5) * 6
library(e1071)

kurtosis(datos_platicurtica_beta_muy_plana)
# Función de densidad beta centrada y escalada
densidad_beta_escalada_muy_plana <- function(x) {
  y <- (x + 3) / 6 # Desescalar para el argumento de dbeta
  ifelse(y >= 0 & y <= 1, dbeta(y, shape1 = alpha, shape2 = beta) / 6, 0) # Dividir por el factor de escala para la densidad
}

# Visualización
hist(datos_platicurtica_beta_muy_plana, main = "Distribución Platicúrtica (Beta Centrada y Escalada - muy plana)", xlab = "Valores", ylab = "Densidad", col = "lightsalmon", freq = FALSE)
curve(densidad_beta_escalada_muy_plana(x), from = min(datos_platicurtica_beta_muy_plana), to = max(datos_platicurtica_beta_muy_plana), col = "red", add = TRUE)


########
set.seed(123)
n <- 10000
x <- rnorm(n)

# Transformación que reduce la curtosis
x_platicurtic <- sign(x) * abs(x)^0.5  # También puedes probar con ^0.4 o ^0.3

# Visualización
hist(x_platicurtic, probability = TRUE, breaks = 50, col = "lightgray",
     main = "Distribución Platicúrtica Transformada",
     xlab = "Valor", ylab = "Densidad")
lines(density(x_platicurtic), col = "red", lwd = 2)

# Comparar con normal
curve(dnorm(x), col = "blue", lwd = 2, add = TRUE, lty = 2)
legend("topright", legend = c("Transformada", "Normal"),
       col = c("red", "blue"), lty = c(1, 2), lwd = 2)


######
# Simular datos con alta curtosis (distribución t de Student con pocos grados de libertad)
datos_alta_curtosis <- rt(1000, df = 3)
hist(datos_alta_curtosis, main="Distribución con Alta Curtosis", xlab="Valor", ylab="Frecuencia", col="lightcoral", border="black")
abline(v=mean(datos_alta_curtosis), col="red", lty="dashed", lwd=2)

# Comparar con una normal
datos_normal <- rnorm(1000)
lines(density(datos_normal), col="blue", lwd=2)
legend("topright", legend=c("Alta Curtosis", "Normal"), col=c("lightcoral", "blue"), lty=1)
#######
# Simulación de una distribución platicúrtica (Beta centrada y escalada - más plana)
n <- 1000
alpha <- 5
beta <- 5

# Generar datos beta
datos_beta <- rbeta(n, shape1 = alpha, shape2 = beta)

# Centrar y escalar aproximadamente al rango de -3 a 3
datos_platicurtica_beta_plana <- (datos_beta - 0.5) * 6

# Función de densidad beta centrada y escalada
densidad_beta_escalada_plana <- function(x) {
  y <- (x + 3) / 6 # Desescalar para el argumento de dbeta
  ifelse(y >= 0 & y <= 1, dbeta(y, shape1 = alpha, shape2 = beta) / 6, 0) # Dividir por el factor de escala para la densidad
}

# Visualización
hist(datos_platicurtica_beta_plana, main = "Distribución Platicúrtica (Beta Centrada y Escalada - más plana)", xlab = "Valores", ylab = "Densidad", col = "lightgoldenrodyellow", freq = FALSE)
curve(densidad_beta_escalada_plana(x), from = min(datos_platicurtica_beta_plana), to = max(datos_platicurtica_beta_plana), col = "red", add = TRUE)



#######