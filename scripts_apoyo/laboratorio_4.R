
data <- read.csv('https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/MoneyUS.csv')%>%
  rename(GDP= y, m1=m)%>%
  select(-rownames)

names(data)
write.csv(data,'data/datos_rl.csv',row.names = FALSE)

#"https://vincentarelbundock.github.io/Rdatasets/doc/Ecdat/MoneyUS.html"
View(data)
names(data)
plot(m~y,data=data)

cor(data$y, data$m)

rl <- lm(m~y, data=data)
summary(rl)$r.squared



#######
# --- 1. Gráfico de Dispersión para el conjunto de datos ---
# Ejercicio 1.1: Cargar datos y generar gráfico de dispersión

# Genera el gráfico de dispersión
plot(data$GDP, data$m1,
     main = "Gráfico de Dispersión de m1 vs. GDP", # Título del gráfico
     xlab = "Producto Interno Bruto (GDP)",     # Etiqueta del eje X
     ylab = "Oferta Monetaria (m1)",             # Etiqueta del eje Y
     pch = 16,                                   # Tipo de punto (círculos rellenos)
     col = "blue",                               # Color de los puntos
     cex = 1.2)                                  # Tamaño de los puntos

# Añadir una línea de referencia (opcional, para visualización inicial)
abline(lm(data$m1 ~ data$GDP), col="black", lty=2)


# --- 2. Determinar coeficiente de correlación de Pearson ---
# Ejercicio 2.1: Cálculo del coeficiente de correlación de Pearson

# Calcula el coeficiente de correlación de Pearson entre 'GDP' y 'm1'
coef_correlacion <- cor(data$GDP, data$m1, method = "pearson")
paste("Coeficiente de Correlación de Pearson (GDP vs. m1):", round(coef_correlacion, 4))


# --- 3. Generar regresión lineal con la función lm ---
# Ejercicio 3.1: Generar el modelo de regresión lineal

# Crea el modelo de regresión lineal. La sintaxis es 'variable_dependiente ~ variable_independiente'
modelo_regresion <- lm(m1 ~ GDP, data = data)

# Puedes imprimir el resumen del modelo para ver los coeficientes y R-cuadrado
summary(modelo_regresion)


# --- 4. Extraer del resultado de la regresión lineal el r cuadrado y los coeficientes o parámetros ---
# Ejercicio 4.1: Extraer R cuadrado y coeficientes

# Obtiene el resumen completo del modelo de regresión
resumen_modelo <- summary(modelo_regresion)

# Extrae el valor de R cuadrado
r_cuadrado_modelo <- resumen_modelo$r.squared
paste("R Cuadrado del Modelo:", round(r_cuadrado_modelo, 4))

# Extrae los coeficientes (parámetros) del modelo
coeficientes_modelo <- coef(modelo_regresion)
intercepto <- coeficientes_modelo[1]
pendiente <- coeficientes_modelo[2]

paste("Intercepto (beta_0):", round(intercepto, 4))
paste("Pendiente (beta_1):", round(pendiente, 4))


# --- 5. Graficar sobre el grafico de dispersión la línea de la regresión lineal ---
# Ejercicio 5.1: Graficar sobre el gráfico de dispersión la línea de la regresión lineal

# Volvemos a generar el gráfico de dispersión para asegurarnos de que la línea se dibuje sobre él
plot(data$GDP, data$m1,
     main = "Gráfico de Dispersión con Línea de Regresión",
     xlab = "Producto Interno Bruto (GDP)",
     ylab = "Oferta Monetaria (m1)",
     pch = 16,
     col = "blue",
     cex = 1.2)

# Añade la línea de regresión al gráfico
abline(modelo_regresion, col = "red", lwd = 2) # 'lwd' es el grosor de la línea


# --- 6. Extracción de media de m1, media de GDP ---
# Ejercicio 6.1a: Extracción de medias

media_m1_calculada <- mean(data$m1)
media_GDP_calculada <- mean(data$GDP)

paste("Media de m1:", round(media_m1_calculada, 4))
paste("Media de GDP:", round(media_GDP_calculada, 4))


# --- 7. Cálculo del coeficiente de correlación sin usar la función cor ---
# Ejercicio 6.1b: Cálculo del coeficiente de correlación sin usar 'cor()'

# Calcula las desviaciones de cada punto con respecto a sus respectivas medias
desviacion_GDP <- data$GDP - media_GDP_calculada
desviacion_m1 <- data$m1 - media_m1_calculada

# Calcula el numerador de la fórmula de Pearson
numerador_manual <- sum(desviacion_GDP * desviacion_m1)

# Calcula las partes del denominador de la fórmula de Pearson
denominador_parte_GDP_cuadrado <- sum(desviacion_GDP^2)
denominador_parte_m1_cuadrado <- sum(desviacion_m1^2)
denominador_manual <- sqrt(denominador_parte_GDP_cuadrado * denominador_parte_m1_cuadrado)

# Calcula el coeficiente de correlación manualmente
coef_correlacion_manual <- numerador_manual / denominador_manual
paste("Coeficiente de Correlación de Pearson (Cálculo Manual):", round(coef_correlacion_manual, 4))


# --- 8. Calcular los parámetros (pendiente e intercepto) de la regresión lineal mediante las fórmulas ---
# Ejercicio 6.2: Cálculo de los parámetros (pendiente e intercepto) manualmente

# Calcula la pendiente (beta_1) usando la fórmula: Sum((Xi - X_bar)(Yi - Y_bar)) / Sum((Xi - X_bar)^2)
# Nota: Ya tenemos el numerador_manual y denominador_parte_GDP_cuadrado del ejercicio anterior
pendiente_manual <- numerador_manual / denominador_parte_GDP_cuadrado
paste("Pendiente (beta_1) calculada manualmente:", round(pendiente_manual, 4))

# Calcula el intercepto (beta_0) usando la fórmula: Y_bar - beta_1 * X_bar
intercepto_manual <- media_m1_calculada - pendiente_manual * media_GDP_calculada
paste("Intercepto (beta_0) calculado manualmente:", round(intercepto_manual, 4))


# --- 9. Obtención de los errores residuales ---
# Ejercicio 7.1a: Obtención de los errores residuales

# Los errores residuales son la diferencia entre los valores observados y los valores predichos
# Se pueden extraer directamente del objeto del modelo de regresión
errores_residuales <- residuals(modelo_regresion)

# También puedes calcularlos manualmente si lo deseas:
# valores_predichos <- predict(modelo_regresion, newdata = data)
# errores_residuales_manual <- data$m1 - valores_predichos

# Muestra los primeros 5 errores residuales
errores_residuales[1:5]


# --- 10. Graficar los errores residuales ---
# Ejercicio 7.1b: Graficar los errores residuales

# Crea un gráfico de dispersión de los errores residuales vs. la variable independiente (GDP)
plot(data$GDP, errores_residuales,
     main = "Gráfico de Residuos vs. GDP", # Título del gráfico
     xlab = "Producto Interno Bruto (GDP)",
     ylab = "Errores Residuales",
     pch = 16,
     col = "darkgreen")

# Añade una línea horizontal en cero para referencia
abline(h = 0, col = "red", lty = 2) # lty = 2 para línea discontinua
