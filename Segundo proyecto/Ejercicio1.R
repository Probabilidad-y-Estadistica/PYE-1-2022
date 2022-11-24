# Ejercicio 1

# La función rexp genera muestras con distribución exponencial, tiene dos parámetros: n y rate.
# 	n es el tamano
# 	rate es la tasa de crecimiento (lambda). 

lambda = 3.5

# Generar X1, X2, .. X50 con distribución exponencial 
muestra = rexp(50, lambda)

# µ
muEmp = mean(muestra) 

# σ2
varEmp = var(muestra)

################################
# Parte A
################################
# Esperanza teórica para la distribución exponencial: E(X) = 1/lambda (o 1/rate)
espX50 = 1/lambda

# Varianza teórica de una distribución exponencial: Var(X) = 1/lambda^2 , en este caso (3.5^2)
# Varianza teórica de X50
varTeo = (1/(3.5^2))
varX50 = varTeo / 50

#################################
# Parte B
#################################
# Se debe generar un vector de tamano 10ˆ4 con simulaciones independientes de muEmp

medias = c()
desviaciones = c()
for(i in 1:10000){
  sim = rexp(50, lambda)
  uEmp = mean(sim)
  vEmp = var(sim)
  
  medias[i] = uEmp
  desviaciones[i] = sqrt(vEmp)
}

# Superponer vector con simulaciones y la N(µ, σ) calculados en parte A
hist(medias, freq = FALSE, col = "lightgreen", main = "Promedios y Distribución Normal", xlab = "Promedios empíricos",  ylab = '')
curve(dnorm(x, mean = espX50, sd = sqrt(varX50)), add = TRUE, col = "red", lwd = 2)

###############################
# Parte C 
###############################
#Construir 10^4 intervalos de confianza para la media a partir de las muestras generadas en la parte anterior. 
#Utilice una confianza de 95 % y asuma varianza desconocida.

alpha = 0.05
n = 50
# Identificamos puntuacion critica (z)
z = qnorm(1-(alpha/2))

# Para cada valor de vector, calculo los estadisticos
limitesInf = c()
limitesSup = c()

for(i in 1:length(medias)){
  x = medias[i]
  
  inf = x - ((desviaciones[i] / sqrt(n)) * z)
  limitesInf = c(limitesInf, inf)
  
  sup = x + ((desviaciones[i] / sqrt(n)) * z)
  limitesSup = c(limitesSup, sup)
}

# De los 10^4 intervalos de confianza construidos en la parte anterior, calcular la 
# proporción de ellos que no contiene el valor esperado teórico de X1
# Es decir: espX50 es menor al limite inferior o mayor al limite superior
cantFuera = 0
for(i in 1:10000){
  if(limitesInf[i] > espX50 || limitesSup[i] < espX50){
    cantFuera = cantFuera + 1
  }
}

proporcion = cantFuera/length(medias)





