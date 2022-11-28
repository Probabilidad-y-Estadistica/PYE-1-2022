############################
# Ejercicio 2
############################

############################
# Parte A
############################
lambda = 3.5

# Vector ejercicio 1
medias = c()
for(i in 1:10000){
  sim = rexp(50, lambda)
  uEmp = mean(sim)
  
  medias[i] = uEmp
}

# Esperanza teórica para la distribución exponencial: E(X1) = 1/lambda (o 1/rate)
espX50 = 1/lambda
u0 = espX50
var = (1/(lambda^2))

alpha = 0.01
rc = qnorm(1-(alpha/2))

cant = 0
for(i in 1:length(medias)){
  z = (medias[i] - u0)/(sqrt(var)/sqrt(50))
  
  #Check si esta o no en la región crítica
  if(z < -rc || z > rc){
    cant = cant + 1
  }
}

prop = cant / 10000
print(prop)
############################
# Parte B
############################

# Generar una nueva muestra y estimar el p-valor para esta muestra usando la proporcion de valores de Xn en 
# el vector del Ejercicio 1 que cumplen |Xn − μ0| > |xn − μ0|.

distrNueva = rexp(50, lambda)
desvNueva = sqrt(var(distrNueva))
espNueva = mean(distrNueva)

cantP = 0
for(i in 1:length(medias)){
  if(abs(medias[i] - u0) > abs(espNueva - u0)){
    cantP = cantP + 1
  }
}

pValor = cantP/10000
print(pValor)
############################
# Parte C
############################

# alpha* = 2[1 - Q(a)]
a = (sqrt(50)/desvNueva) * abs(espNueva - u0)
alpha = 2 * (1 - pnorm(a))

print(alpha)
