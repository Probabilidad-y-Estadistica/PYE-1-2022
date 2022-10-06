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
boxplot(
  a,b,c,d,
  col = c("darkseagreen1",
          "darkseagreen2", 
          "darkseagreen3", 
          "darkseagreen4"),
  main = "Distribución Exponencial",
  xlab = "Muestras",
  ylab = "Valor variable aleatoria",
  yaxt='n')

# Mostramos los datos particulares de cada una de las muestras obtenidas.
boxplot.stats(a)
boxplot.stats(b)
boxplot.stats(c)
boxplot.stats(d)

# Agregamos eje "y" mas exacto
axis(2, at=seq(0, 10, 0.2),las=2)

median(a) # 0.1972078 $conf[1] 0.1610968 0.2333188
median(b) # 0.2024430 $conf[1] 0.1870399 0.2178462
median(c) # 0.1995386 $conf[1] 0.1945816 0.2044957
median(d) # 0.1983504 $conf [1] 0.1967769 0.1999239

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

# Histogramas
# Generamos un histograma con los valores obtenidos de realizar los experimentos aleatorios
# Rejilla del eje X
x = seq(0, 8, 0.1)
# Histograma para  muestra 10^2
hist(a, 
     main="Distribución Exponencial - muestra 10^2",
     breaks = 20, 
     xaxt="n",
     ylab="Densidad",
     xlab="x",
     freq=FALSE)
# Dibujamos una línea con la función de densidad
lines(x, dexp(x, rate = 3.5), col = "blue", lty = 1, lwd = 2)

# Agregamos una leyenda
legend("topright", 
       legend = c("Valores aleatorios",
                  "Función de densidad"),
       lty = 1, col = c("grey","blue"), lwd =1, box.lty =1)

# Realizamos el histograma para el tamaño de muestra 10^5 análogamente al 10^2
# Rejilla del eje X
x = seq(0, 8, 0.1)

# Histograma para  muestra 10^5
hist(d, 
     main="Distribución Exponencial - muestra 10^5",
     ylab="Densidad",
     xlab="x",
     breaks = 50,
     xaxt="n", 
     freq=FALSE)

# Dibujamos una línea con la función de densidad
lines(x, dexp(x, rate = 3.5), col = "blue", lty = 1, lwd = 2)

# Agregamos una leyenda
legend("topright", 
       legend = c("Valores aleatorios",
                  "Función de densidad"),
       lty = 1, col = c("grey","blue"), lwd =1, box.lty =1)


