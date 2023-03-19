#Ejercicio 1. Aplicar el contraste de hipótesis basado en la media a y1 e y2
x <- rbinom(1000,1,prob = 0.1)
y <- rbinom(1000,1,prob = 0.1)

muestra1 <- rnorm(1000)
muestra2 <- rnorm(1000)

y1 <- x*muestra1
y2 <- y*muestra2

hist(y1)
hist(y2)

set.seed(1)
z <- rnorm(100)
x <- rpois(100,10.3)
y <- rbinom(100,1,0.25)
y1 <- 5*z+x*10+rnorm(100,2,1)
y2 <- 5*z+x*12+rnorm(100,2,1)

hist(y1)
hist(y2)

t.test(y1)
t.test(y2)
#Con los datos que tengo, esta función es como un juez. En base a unos valores se han escogido esos

#Ejercicio 2. ¿Por qué decimos que la correlación lineal es una prueba de correlación paramétrica?
#¿En qué se diferencian las pruebas paramétricas de las no paramétricas?
Se considera correlación lineal porque asume que los datos que va a analizar tienen una distribución y una relación lineal entre ellos
Estas se diferencian en que las pruebas paramétricas suponen cosas de los datos, mientras que las otras no

¿Por qué decimos que la correlación lineal es una prueba de correlación paramétrica?
  #¿En qué se diferencian las pruebas paramétricas de las no paramétricas?
  Se considera correlación lineal porque asume que los datos que va a analizar tienen una distribución y una relación lineal entre ellos
Estas se diferencian en que las pruebas paramétricas suponen cosas de los datos, mientras que las otras no

#Ejercicio3. Calcula la correlación entre las variables almacenadas en la tabla ‘data’.
correlacion <- cor(data$longitud, data$ancho)
correlacion
correlacion2 <- cor(data$grosor, data$peso)
correlacion2

#Ejericio.4 Calcula los coeficientes de correlación de las variables y el nivel de significancia
install.packages("GGally")
library(GGally)
cmatriz <- cor(data)
pm <- cor.mtest(data)$p
ggcorr(cmatriz , p.mat = pm, label = TRUE, label_round = 2)

#Ejercicio 5. Emplea una función para obtener en una matriz de correlación lineal, IC 95% y pvalue de todas las variables en el data frame ‘data’
library(correlation)
resultados <- correlation(data)
resultados

#Ejercicio 6. Visualiza gráficamente la correlación lineal existente entre las variables ‘longitud’ y ‘peso’
plot(data$longitud, data$peso, xlab="Longitud", ylab="Peso")
ajuste <- lm(peso ~ longitud, data=data)
abline(ajuste, col="red")

#Ejercicio 7. Emplea la librería `corrplot()` para visualizar la correlación entre variables
install.packages("corrplot")
library(corrplot)
install.packages("see")
library(see)
install.packages("tidygraph")
library(tidygraph)
install.packages("ggraph")
library(ggraph)
gráfica <- plot(resultados)
gráfica

#Ejercicio 8. A partir de la siguiente secuencia de valores numéricos
a. Crear los vectores
distancia <- c( 1.1,100.2,90.3,5.4,57.5,6.6,34.7,65.8,57.9,86.1)
n_piezas <- c(110,2,6,98,40,94,31,5,8,10)
data_frame <- data.frame(distancia = distancia, n_piezas = n_piezas)

b. Calcular el coeficiente de correlación
cor(data_frame$distancia,data_frame$n_piezas)

c. Calcular el nivel de significancia
t.test(data_frame, mu = 5)

d.Calcula el intervalo de confianza al 95%
t.test(data_frame, mu = 95)

e. ¿Qué intensidad y dirección presentan ambas variables?
cor(distancia, n_piezas)