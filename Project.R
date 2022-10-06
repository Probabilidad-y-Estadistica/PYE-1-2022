# EJ 1
# distribución poisson (lambda = 8 ) 
# n = 10 ^ (2 al 5)
# rpois(
#       n,     Numero de observaciones aleatorias a ser generadas
#       lambda, vector de medias no negativas )

# Generamos 4 constantes "a", "b", "c" y "d" aplicando la distribución de poisson
# para los tamaños de muestra 10^2, 10^3, 10^4 y 10^5 respectivamente
a = rpois(10^2,lambda = 8)
b = rpois(10^3,lambda = 8)
c = rpois(10^4,lambda = 8)
d = rpois(10^5,lambda = 8)

# Con boxplot generamos el gráfico de cajas agregado color para distiguir 
# cada gráfico y borramos el eje "y" para agregar uno más exacto
boxplot(#values ~ group,
  #data,
  a,b,c,d,
  col = c("pink",
          "red", 
          "orange", 
          "yellow"),
  main = "Distribución de Poisson",
  xlab = "Muestras",
  ylab = "Valor variable aleatoria",
  yaxt='n')

# Agregamos eje "y" mas exacto
axis(2, at=seq(0, 30, 1),las=2)

#Mostramos los datos particulares de cada una de los boxplot obtenidos.
boxplot.stats(a)
boxplot.stats(b)
boxplot.stats(c)
boxplot.stats(d)

# Utilizamos "mean" para calcular la esperanza empírica de cada muestra
# y guardamos cada resultado para su posterior comparación
espA = mean(a)
espB = mean(b)
espC = mean(c)
espD = mean(d)

# Por el teórico se sabe que lambda coincide con la esperanza teórica
espT = 8

# Se compara el valor teórico contra el valor empirico obtenido en cada caso
comparEA = espA - espT
comparEB = espB - espT
comparEC = espC - espT
comparED = espD - espT
# Se observa que en efecto cuando se aumenta la cantidad de ensayos el resultado se parece más al teorico

# Utilizamos el comando "var" para calcular la varainza empírica de cada muestra
# y guardamos cada resultado para su posterior comparación
varA = var(a)
varB = var(b)
varC = var(c)
varD = var(d)

# Por el teórico se sabe que lambda coincide con el valor de la varianza
varT = 8

# Se compara el valor teorico de la varianza contra el valor empirico obtenido en cada caso
comparVA = varA - varT
comparVB = varB - varT
comparVC = varC - varT
comparVD = varD - varT
# Se observa que en efecto cuando se aumenta la cantidad de ensayos el resultado se parece más al teorico

# Con "ecdf" calculamos la distribución empírica para las muestras
# de tamaño 10^2 y 10^5
distA = ecdf(a)
distD = ecdf(d)

# Utilizamos "stepfun" con una lista de 1 a 10 junto a ppois para obetner
# obtener la funcion de distribucion acumulada teorica y poder graficarla
# utilizando "plot". genrando los escalones caracteristicos. Nos aseguramos
# de eliminar las verticales y de agregar las etiquetas correspondientes
plot(stepfun(c(1:10),ppois(c(0,1:10), lambda = 8)), 
     main = "Función de distribución acumulada para 10^2", 
     xlab = "k",
     ylab = "F(k)",
     col="blue")


# Sobre el grafico anterior graficamos nuestra desitribucion empirica 
# con tamaño de muestera 10^2 que obtuvimos previmante con un color
# diferente para distinguirla de la distribucion teorica
lines(distA, col="orange")

# Agregamos una leyenda para hacer mas clara la identificacion
# de los graficos
legend("bottomright", 
       legend = c("Distribución Acumulada Empírica - 10^2",
                  "Distribución Acumulada Teórica"),
       lty = 1, col = c("blue","orange"), lwd = 2, box.lty =1)


# Analogamente al grafico anterior graficamos la distribucion acumulada teorica

plot(stepfun(c(1:10),ppois(c(0,1:10), lambda = 8)), 
     main = "Función de distribución acumulada para 10^5", 
     xlab = "k",
     ylab = "F(k)",
     col="red")
lines(distD, col="blue")
legend("bottomright", 
       legend = c("Distribución Acumulada Empírica - 10^5",
                  "Distribución Acumulada Teórica"),
       lty = 1, col = c("blue","red"), lwd = 2, box.lty =1)



#---------------------------------------------------------------------------------------

# EJ 2
# Distribucion Exponencial(3.5)
# n = 10 ^ (2 al 5)

# Antes de generar las distribuciones fijamos una semilla para que las salidas sean reproducibles
set.seed(3.5)

# Generamos 4 constantes "a", "b", "c" y "d" aplicando la distribucion exponencial
# para los tamanos de muestra 10^2, 10^3, 10^4 y 10^5 respectivamente
a = rexp(10^2, rate = 3.5)
b = rexp(10^3, rate = 3.5)
c = rexp(10^4, rate = 3.5)
d = rexp(10^5, rate = 3.5)


# Con boxplot generamos el grÃ¡fico de cajas agregado color para distiguir 
# cada grafico y borramos el eje "y" para agregar uno mas exacto, al igual
# que lo hicimos con la distribucion de poisson
boxplot(#values ~ group,
  #data,
  a,b,c,d,
  col = c("pink",
          "beige", 
          "lightblue", 
          "lightgreen"),
  main = "Distribución normal",
  xlab = "Muestras",
  ylab = "Valor variable aleatoria", 
  yaxt='n')

# Agregamos eje "y" mas exacto
axis(2, at=seq(-20, 20, 1),las=2)

# Obtenemos los stats de cada boxplot para analizar
boxplot.stats(a)
boxplot.stats(b)
boxplot.stats(c)
boxplot.stats(d)

# Como estamos trabajando con la dsitribucion exponencial, sabemos que la esperanza
# teorica se calcula como 1/lambda o 1/rate
espT = (1/3.5)

# Se utiliza "mean" para calcular la esperanza empirica
espA = mean(a)
espB = mean(b)
espC = mean(c)
espD = mean(d)

# La varianza teorica se calcula como 1/lambda^2 en este caso (3.5^2)
varT = (1/(3.5^2))

# Varianza Empirica
# Se utiliza "var" para calcular la varianza empirica
varA = var(a)
varB = var(b)
varC = var(c)
varD = var(d)

# Histogramas
# Generamos un histograma con los valores obtenidos de ralizar los experimentos aleatorios
# Rejilla del eje X
x <- seq(0, 8, 0.1)
# Histograma para  muestra 10^2
hist(a, 
     main="Distribución Exponencial - muestra 10^2",
     breaks = 10, 
     xaxt="n",
     ylab="Densidad",
     xlab="x",
     freq=FALSE)
# Dibujamos una linea con la funcion de densidad
lines(x, dexp(x, rate = 3.5), col = "blue", lty = 1, lwd = 2)

# Agregamos una leyenda
legend("topright", 
       legend = c("Valores aleatorios",
                  "Funcion de densidad"),
       lty = 1, col = c("grey","blue"), lwd =1, box.lty =1)


# Realizamos el histograma para el tamaño de muestra 10^5 analogamente al 10^2
# Rejilla del eje X
x <- seq(0, 8, 0.1)

# Histograma para  muestra 10^5
hist(d, 
     main="Distribución Exponencial - muestra 10^5",
     ylab="Densidad",
     xlab="x",
     breaks = 50,
     xaxt="n", 
     freq=FALSE)
# Dibujamos una linea con la funcion de densidad
lines(x, dexp(x, rate = 3.5), col = "blue", lty = 1, lwd = 2)

# Agregamos una leyenda
legend("topright", 
       legend = c("Valores aleatorios",
                  "Funcion de densidad"),
       lty = 1, col = c("grey","blue"), lwd =1, box.lty =1)

# Pintamos debajo de la grafica
#polygon(x, 
#        dexp(x, rate = 3.5), 
#        col = rgb(0, 0, 1, alpha = 0.5))
#axis(1, at=seq(-20, 10, 1),las=1)

#------------------------------------------------------------------------------------

# EJ 3
# Distribución Exponencial(3.5)

# PT 1) n = 10 ^ 3
# Generamos una constante con valores aleatorios para la variable con distribución normal
a = rexp(10^3,-4, sqrt(16)) 
# Calculamos la media empírica de los valoreas aleatorias de a
muEmp = mean(a) 
# Guradamos la media teórica 
muTeo = -4
# Guardamos desviación estandar teórica
desvTeo = sqrt(16)
# Valor estandarizado de mediana empírica
medEsta = sqrt(1000) * ((muEmp - muTeo)/desvTeo)

# PT 2)

# Para la parte 2 realizamos un loop for para realizar los calculos de la parte "A" y guardarlas en un array 
promEst <- 0
for(i in 1:500){
  ab = rnorm(10^3,-4, sqrt(16))
  muEmpb = mean(ab) 
  promEst[i] <- sqrt(10^3) * ((muEmpb - muTeo)/desvTeo)
}

# Presentamos en un histograma los valores obtenidos en el loop
s = seq(-4, 4, 0.1)
hist(promEst,
     main="Promedio estandarizados y distribuciÃ³n normal estandar",
     breaks = 50,
     xaxt="n",
     ylab="Densidad",
     xlab="x",
     freq=FALSE)

# Sobre el histograma graficamos la densidad de la distribución normal estandard
lines(s, 
      dnorm(s, mean = 0, sd = 1),
      lty = 1, 
      lwd = 2)
polygon(s, 
        dnorm(s, mean = 0, sd = 1), 
        col = rgb(1, 0, 0, alpha = 0.5))

# Por último agregamos una leyenda para visualizar claramente los resultados  y cambiamos la escala del eje
axis(1, at=s,las=1)
legend("topleft", 
       legend = c("Promedio estandarizado",
                  "DistribuciÃ³n normal estandar"),
       lty = 1, col = c("grey","red"), lwd =1, box.lty =1)
