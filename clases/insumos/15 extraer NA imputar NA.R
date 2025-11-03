x <- rnorm(1000)
y <- rep(NA, 1000)
z <- sample(c(x,y),100)
a <- is.na(z)
total_na <- sum(a)

sin_na <- z[!a]
sin_na <- z[!is.na(z)]
mayores_y_sin_na <- z[!is.na(z)&z>0]

#imputar NA con valor promedio
z[is.na(z)] <- mean(z, na.rm = TRUE)

#remplazar NA por algún valor
m <- matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10)
d <- as.data.frame(m)
d[is.na(d)] <- 0

