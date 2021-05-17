#Instalación de paquetes
install.packages("RSQLite")
install.packages("ggplot2")
install.packages("nortest")
install.packages("e1071")
install.packages("fitdistrplus")

#Inclusion de librerias
library(RSQLite)
library(MASS)
library(ggplot2)
library(nortest)
library(e1071)
library(fitdistrplus)

# Generación de modelo para simular duración de canciones ----------------------

# conexión con la base de datos
conn = dbConnect(RSQLite::SQLite(), "track_metadata.db")

# consulta para obtener canciones de artistas con inicial S o H
query = "SELECT * from songs WHERE artist_name LIKE 'S%' OR artist_name LIKE 'H%'"
result = dbGetQuery(conn, query)

# histograma de duracion de canciones
ggplot(result, aes(x = duration)) +
        geom_histogram(breaks=seq(0, 1000, by=20), fill = "white", colour = "black") +
        labs(title="Histograma de duración de canciones", x="Duración", y="Frecuencia")
# media de la duración
mean(result$duration)

# grafico Q-Q con línea normal 
qqnorm(result$duration, pch = 1, frame = FALSE)
qqline(result$duration, duration = "steelblue", lwd = 2)

# prueba de normalidad chi-cuadrado
pearson.test(result$duration)

# diagrama de caja sobre duracion
boxplot(result$duration,data=result, main="Diagrama de caja sobre duracion de canciones",
        ylab="Duracion en segundos")
summary(result$duration)

# consulta para obtener canciones de artistas con inicial S o H y duracion entre
# 16,91 y 464.16 segundos.
query = "SELECT * from songs WHERE (duration BETWEEN 16.91 AND 464.16) AND (artist_name LIKE 'S%' OR artist_name LIKE 'H%')"
result = dbGetQuery(conn, query)

# histograma de duracion de canciones sin valores extremos
ggplot(result, aes(x = duration)) +
  geom_histogram(breaks=seq(0, 1000, by=20), fill = "white", colour = "black") +
  labs(title="Histograma de duración de canciones", x="Duración", y="Frecuencia")

# prueba de simetria para determinar distribucion
sim = skewness(result$duration) # < 0 = beta , > 0 = gamma
print(sim)

# ajustar distribucion gamma para obtener parametros
dist = fitdist(result$duration, distr="gamma")
summary(dist)
shape = 6.76
rate = 0.03

# creacion de modelo basado en parametros obtenidos
Model = rgamma(10000, shape, rate=rate)
hist(Model, breaks=50)

# Generacion de modelo para simular tiempo de procesamiento --------------------

# cargar datos de canciones y sus tiempos de procesamiento
df= (read.csv(file = 'Processing_Time.csv', header=T, encoding = "UTF-8"))
attach(df)
summary(df)

# grafico de dispersión de tiempo de procesamiento por duracion
plot(df$Seconds, df$Processing..s., pch = 19, col = "black",main = 'Grafico de Dispersión', 
     xlab = 'Duracion en segundos', ylab = 'Tiempo de procesamiento en segundos')

# regresion lineal sobre los datos
regln <- lm(df$Processing..s.~ df$Seconds)
abline(regln, col = "orange", lwd = 3)

# cálculo de coeficiente de determinación R^2
r2 <- cor(df$Processing..s., df$Seconds)^2

# Análisis de pendiente e intersección con sus p-value.
summary(regln)

# simulacion de ventas ---------------------------------------------------------
p = 0.9
e = 0.05
# N = cantidad de experimentos necesarios
N = p * (1 - p) * (qnorm(1 - (e/2))/e)^2
comissionLimit = 1000000
goal = 1000000

testTime = 2678400 # segundos en un mes
multiplier = 12
sales = rep(0,N) #vector de ventas totales por corrida
longSongSales = rep(0,N) # vector de ventas de canciones largas por corrida
longSongFraction = rep(0,N) # vector de canciones largas por corrida
for(i in 1:N) {
  timeCounter = 0 # tiempo transcurrido en la simulacion
  songCounter = 0
  while (timeCounter < testTime) {
    randSongDuration = rgamma(1, shape, rate=rate)
    songCounter = songCounter + 1
    procTime = 0.098 * randSongDuration # usando el modelo lineal obtenido
    
    if (randSongDuration < 360) {
      sales[i] = sales[i] + 0.99
    }
    else {
      sale = ceiling(randSongDuration/360) * 0.99
      sales[i] = sales[i] + sale
      longSongSales[i] = longSongSales[i] + sale
      longSongFraction[i] = longSongFraction[i] + 1 
    }
    
    timeCounter = timeCounter + procTime
  }
  longSongFraction[i] = longSongFraction[i]/songCounter
}

cat("Valor esperado de ventas:", mean(sales*multiplier))
cat("Error estandar:",sd(sales*multiplier))
earnings = ifelse(sales>comissionLimit, sales - sales*0.3, sales - sales*0.15)
cat("Ganancias esperadas:",mean(earnings*multiplier))
cat("Fraccion de canciones mayores a 6  min:",mean(longSongFraction))
cat("Fraccion de ventas mayores a 6  min:",mean(longSongSales)/mean(sales))
cat("Probabilidad de cumplir la meta de ventas en 1 año:",sum(sales*multiplier>goal)/N)




