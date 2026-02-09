# Generar el dataset de ventas
set.seed(42)
n_obs <- 300
productos <- c("Pan", "Leche", "Huevos", "Arroz", "Pasta", "Aceite", "Azucar", "Cafe", "Harina", "Manteca",
               "Yogur", "Queso", "Jamon", "Pollo", "Carne_Res", "Pescado", "Tomates", "Papas", "Cebollas", "Manzanas",
               "Platanos", "Detergente", "Champu", "Jabon", "Papel_Higienico", "Refresco", "Cerveza", "Vino", "Snacks", "Chocolate")

df <- as.data.frame(matrix(sample(c(0, 1), n_obs * 30, replace = TRUE, prob = c(0.8, 0.2)), 
                           nrow = n_obs, ncol = 30))
colnames(df) <- productos
df$ID_Transaccion <- 1:n_obs
df <- df[, c(31, 1:30)] # Reordenar ID al inicio

# Guardar como CSV
write.csv(df, "ventas_supermercado.csv", row.names = FALSE)