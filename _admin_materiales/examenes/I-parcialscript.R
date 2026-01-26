if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(moments)) install.packages("moments")
library(ggplot2)
library(dplyr)
library(moments)


# DATASET 1: MACROECONOMÍA (50 Países ficticios)
datos_macro <- data.frame(
  Pais = paste("Pais", 1:50),
  PIB_per_capita = round(rlnorm(50, meanlog = 9, sdlog = 1), 2), # Log-normal (común en ingresos)
  Esperanza_Vida = round(rnorm(50, 70, 8), 1),
  Tasa_Desempleo = round(runif(50, 2, 25), 1),
  Region = sample(c("Latam", "Europa", "Asia", "Africa"), 50, replace = TRUE)#,
  # Indice_Libertad_Econ = round(runif(50, 40, 90), 1)
)


# DATASET 2: MICROECONOMÍA (200 Hogares)
datos_hogares <- data.frame(
  Hogar_ID = 1:200,
  Ingreso_Mensual = round(rlnorm(200, meanlog = 6.5, sdlog = 0.8), 2),
  Gasto_Alimentacion = numeric(200),
  Miembros = sample(1:6, 200, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.2, 0.1, 0.1))
)
# El gasto depende del ingreso (Ley de Engel: a mayor ingreso, menor proporción en comida)
datos_hogares$Gasto_Alimentacion <- datos_hogares$Ingreso_Mensual * runif(200, 0.3, 0.6) / (log(datos_hogares$Ingreso_Mensual)/5)


ggplot(datos_macro, aes(x = PIB_per_capita)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribución del PIB per Cápita", x = "USD", y = "Frecuencia")


ggplot(datos_macro, aes(y  = PIB_per_capita)) +
  geom_boxplot() +
  facet_grid(~Region) +
  theme_minimal() +
  labs(title = "Distribución del Gasto en Alimentación",
       x = "USD", 
       y = "Frecuencia")

library(owidapi)

catalog <- owid_get_catalog()
catalog
View(owid_search(catalog, c("food")))

expect_vida <- owid_get("life-expectancy")

test <- expect_vida%>%
  filter(year==2020)
View(test)

library(gapminder)
max(gapminder$year)


ggplot(datos_hogares,
       aes(y  = Ingreso_Mensual)) +
  geom_boxplot() 

summary(datos_hogares$Ingreso_Mensual)

ggplot(datos_hogares,
       aes(y  = Gasto_Alimentacion)) +
  geom_boxplot() +
  facet_wrap(~Miembros, nrow=1)



#########


set.seed(456) # Cambiamos la semilla para variar los datos

# 1. Generar estructura base
datos_hogares <- data.frame(
  Hogar_ID = 1:200,
  # Ingresos log-normales (común en economía)
  Ingreso_Mensual = round(rlnorm(200, meanlog = 6.8, sdlog = 0.7), 2), 
  Miembros = sample(1:7, 200, replace = TRUE, prob = c(0.15, 0.25, 0.25, 0.15, 0.1, 0.05, 0.05))
)

# 2. Calcular Gasto en Alimentación con lógica económica
# Fórmula: Gasto Fijo por Persona (Necesidad) + Propensión Marginal al Consumo (Lujo/Variedad)
datos_hogares <- datos_hogares %>%
  mutate(
    # Costo base por persona (entre 40$ y 60$ por cabeza + ruido aleatorio)
    Costo_Basico = Miembros * runif(n(), 40, 60),
    
    # Gasto adicional según ingreso (la gente con más dinero compra comida más cara)
    # Usamos logaritmo para que decrezca proporcionalmente (Ley de Engel)
    Gasto_Extra = Ingreso_Mensual * runif(n(), 0.10, 0.20),
    
    # Gasto Total preliminar
    Gasto_Alimentacion = round(Costo_Basico + Gasto_Extra, 2)
  )

# 3. Ajuste de Realidad (Constraint)
# El gasto en comida no puede superar el 90% del ingreso (caso pobreza extrema)
datos_hogares$Gasto_Alimentacion <- pmin(
  datos_hogares$Gasto_Alimentacion, 
  datos_hogares$Ingreso_Mensual * 0.90
)

# Limpiar columnas auxiliares
datos_hogares <- datos_hogares %>% select(Hogar_ID, Ingreso_Mensual, Gasto_Alimentacion, Miembros)
datos_hogares <- datos_hogares%>%
  mutate(Ingreso_Mensual=Ingreso_Mensual/10,
         Gasto_Alimentacion= Gasto_Alimentacion/10)

datos_hogares <- datos_hogares%>%
  bind_cols(urban_ccs_df%>%
              sample_n(20, replace = TRUE)%>%
              sample_n(200, replace = TRUE))%>%
  mutate(Ingreso_Mensual=round(Ingreso_Mensual),
         Gasto_Alimentacion=round( Gasto_Alimentacion))%>%
  rename(zona=urban_rural)

# --- VERIFICACIÓN VISUAL ---
# Graficamos para demostrarte que ahora sí existe la relación
ggplot(datos_hogares, aes(x = factor(Miembros), y = Gasto_Alimentacion)) +
  geom_boxplot(fill = "lightgreen", 
               alpha = 0.6,
               outlier.colour = "red",
                outlier.size = 2,
                staplewidth = 0.2) +
  theme_minimal() +
  labs(
    title = "Miembros vs Gasto en Alimentación",
    x = "Número de Miembros en el Hogar",
    y = "Gasto en Alimentación "
  )

ggplot(datos_hogares, aes(y = Gasto_Alimentacion)) +
  geom_boxplot(fill = "lightgreen", 
               alpha = 0.6,
               outlier.colour = "red",
               outlier.size = 2,
               whisker.colour = 'blue',
               staplewidth = 0.2) +
  theme_minimal() +
  labs(
    title = "Relación Corregida: Miembros vs Gasto en Alimentación",
    x = "Número de Miembros en el Hogar",
    y = "Gasto en Alimentación"
  )

ggplot(datos_hogares, 
       aes(x = Gasto_Alimentacion)) +
  geom_histogram(fill = "lightgreen", alpha = 0.6) +
  # facet_wrap(~Miembros)+
  theme_minimal() +
  labs(
    title = "Relación Corregida: Miembros vs Gasto en Alimentación",
    x = "Número de Miembros en el Hogar",
    y = "Gasto en Alimentación ($)"
  )+ #añadir linea media y mediana
  geom_vline(aes(xintercept=mean(Gasto_Alimentacion)),
             color='blue',
             linetype='dashed',
             size=1)+
  geom_vline(aes(xintercept=median(Gasto_Alimentacion)),
             color='red',
             linetype='dashed',
             size=1)

ggplot(datos_hogares, 
       aes(x = Ingreso_Mensual)) +
  geom_histogram( alpha = 0.6, bins =10) +
  theme_minimal() +
  labs(title = "Ingresos Mensual",
       subtitle = "n = 200 hogares",
    x = "Ingreso Mensual",
    y = "Frecuencia"
  )+ #añadir linea media y mediana
  geom_vline(aes(xintercept=mean(Ingreso_Mensual)),
             color='blue',
             linetype='dashed',
             size=1)+
  geom_vline(aes(xintercept=median(Ingreso_Mensual)),
             color='red',
             # linetype='dashed',
             size=1)

ggplot(datos_hogares, 
       aes(x = Miembros)) +
  geom_histogram(fill = "lightgreen", alpha = 0.6)
summary(datos_hogares$Ingreso_Mensual)%>%
  as.data.frame()

plot(datos_hogares$Ingreso_Mensual,
     datos_hogares$Gasto_Alimentacion,
     cex = .1,)
