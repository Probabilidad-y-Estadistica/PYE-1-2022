# Ejercicio 2
# Distrubucion continua: Exponencial (lambda=3,5)

############################## Parte A #######################################

# La función rexp para generar muestras con distribución exponencial tiene dos parámetros: n y rate.
# n es el tamano
# rate es la tasa de crecimiento (lambda). 

lambda = 3.5

# Se generan 4 muestras aleatorias aplicando la distribución exponencial para los tamanos de 
# muestra 10^2, 10^3, 10^4 y 10^5 respectivamente
a = rexp(10^2, lambda)
b = rexp(10^3, lambda)
c = rexp(10^4, lambda)
d = rexp(10^5, lambda)

# Con boxplot generamos el gráfico de cajas agregado color para distiguir 
# cada gráfico y borramos el eje "y" para agregar uno más exacto
boxplot(#values ~ group,
  #data,
  a,b,c,d,
  col = c("blue",
          "green", 
          "orange", 
          "yellow"),
  main = "Distribución Exponencial",
  xlab = "Muestras",
  ylab = "Valor variable aleatoria",
  yaxt='n')

#Mostramos los datos particulares de cada una de las muestras obtenidas.
boxplot.stats(a)
boxplot.stats(b)
boxplot.stats(c)
boxplot.stats(d)

# Agregamos eje "y" mas exacto
axis(2, at=seq(0, 60, 1),las=2)

############################## Parte B #######################################
# Utilizamos "mean" para calcular la esperanza empírica de cada muestra
# y guardamos cada resultado para su posterior comparación
espA = mean(a)
espB = mean(b)
espC = mean(c)
espD = mean(d)

# Esperanza teórica: E(X) = 1/lambda
espT = 1/lambda

# Se compara el valor teórico contra el valor empirico obtenido en cada caso
comparEA = espA - espT
comparEB = espB - espT
comparEC = espC - espT
comparED = espD - espT

# Utilizamos el comando "var" para calcular la varainza empírica de cada muestra
# y guardamos cada resultado para su posterior comparación
varA = var(a)
varB = var(b)
varC = var(c)
varD = var(d)

# Varianza teórica: Var(X) = 1/lambda^2
varT = 1/(lambda^2)

# Se compara el valor teorico de la varianza contra el valor empirico obtenido en cada caso
comparVA = varA - varT
comparVB = varB - varT
comparVC = varC - varT
comparVD = varD - varT

# Cuanto mas grande es la muestra la esperanza se acerca mas a la teórica. Lo mismo con la varianza

############################## Parte C #######################################
#Presentar para n = 100, en un mismo grafico, el histograma de los valores simulados y la densidad
#teorica de la variable simulada. Hacer otro grafico similar, pero para n = 10ˆ5

## dexp: La función en R para calcular la función de densidad de probabilidad exponencial de λ https://r-coder.com/distribucion-exponencial-r/

# Con n=100 (muestra 10^2)
muestra102 = rexp(n = 100, rate = lambda)
hist(muestra102, prob = TRUE, main = "Histograma de densidad", # Densidad
     ylab = "Densidad")
lines(density(muestra102), lwd = 2, col = 'red') # densidad empirica ?? https://r-coder.com/grafico-densidad-r/

# Con n= 10^5 (muestra 10^5)
muestra105 = rexp(n = 10^5, rate = lambda)
hist(muestra105, freq = FALSE, main = "Curva densidad", ylab = "Densidad")
lines(density(muestra105), lwd = 2, col = 'red')

