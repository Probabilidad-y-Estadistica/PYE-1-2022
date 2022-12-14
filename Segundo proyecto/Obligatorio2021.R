#Ejercicio 1
vPromedios = (1:10^4)
vExtSup = (1:10^4)
vExtInf = (1:10^4)

for(i in 1:10^4){
  #generamos la muestra tama?o 50
  z=rnorm(50, mean=-4, sd=4)
  #calculamos el promedio de la muestra anterior
  promedio <- mean(z)
  #guardo en variables los valores que voy a usar en la función qt.
  a = (1-(0.05/2))
  b = (50 -1)
  ErrorE = sd(z)/sqrt(50)
  #calculamos extremos inferiores y superiores de cada muestra
  ExtremoInf <- promedio-(ErrorE*qt(a,b))
  ExtremoSup <- promedio+(ErrorE*qt(a,b))
  #guardamos promedios
  vPromedios[i] <- promedio
  #guardamos extremos de los intervalos de confianza
  vExtSup [i] <- ExtremoSup
  vExtInf [i] <- ExtremoInf
  
}

# Presentamos en un histograma los valores obtenidos
h <- hist(vPromedios, breaks = 100, density = 10,
          ylab = "Densidad", xlab = "x", main = "Promedio  y distribucion normal") 

xfit <- seq(min(vPromedios), max(vPromedios), length = 40)

yfit <- dnorm(xfit, mean = -4, sd =sqrt(0.32))

yfit <- yfit * diff(h$mids[1:2]) * length(vPromedios) 
lines(xfit, yfit, col = "orange", lwd = 2)
#agregamos una leyenda para visualizar claramente los resultados  
# y cambiamos la escala del eje
axis(1, at=s,las=1)
legend("topleft", 
       legend = c("Promedio","Distribucion normal"),
       lty = 1, col = c("grey","orange"), lwd =1, box.lty =1)

#Parte c) intervalos obtenidos
vExtSup
vExtInf

#Parte D)

#se crea contador que cuenta cuando el intervalo no contiene la esperanza teórica
noContieneEsperanza = 0
#recorremos los vectores de extremos de los intervalos de confianza
for(i in 1:10^4){
  if( -4 > vExtSup[i] | -4 < vExtInf[i]){
    #incrementamos el contador cuando el intervalo no contiene a la
    #esperanza teórica
    noContieneEsperanza = noContieneEsperanza +1
  }
}
#mostramos el resultado del contador
noContieneEsperanza #511
#calculamos la proporción que no posee a la esperanza teorica
proporcion = noContieneEsperanza/10^4
#mostramos el resultado de la proporción
proporcion #0.0511


# Ejercicio 2
# Parte A)
#Definimos variables a utilizar en el cálculo de la región crítica
alfa = 0.01
sigma = 4
sqrt_n = sqrt(50)
z = qnorm(1-(alfa/2))
#calculamos el extremos de la región crítica a partir de las variables definidas arriba
extremoRC = (sigma * z/ sqrt_n)
#creamos variable para contar la cantidad de rechazos
rechazo = 0
for(i in 1:10^4){
  #evaluamos si la región crítica y contamos los rechazos de H0
  if(abs(vPromedios[i] - (-4)) > extremoRC) {
    rechazo = rechazo + 1
  }
}
# Proporción de rechazo
propRechazo = (rechazo / 10^4)
#mostramos la proporción obtenida
propRechazo

# Parte B)
#presentamos la muestra fija a usar en parte B Y C
muestra = rnorm(50, mean=-4, sd=sqrt(16))
mu0 = -4

#promedio de la muestra
promedioX = mean(muestra)
#estadístico observado dado en la letra
estadisticoObservado = abs(promedioX - mu0)
#contador que cuenta cuando se cumple la desigualdad
cumpleDesigualdad = 0
for(i in 1:10^4){
  if((abs(vPromedios[i] - mu0)) > estadisticoObservado){
    #incrementamos el contador cuando se sumple la desigualdad
    cumpleDesigualdad = cumpleDesigualdad + 1
  }
}

#estimación del p valor haciendo una proporción de los éxitos dividido muestra total
estimacionP_valor = cumpleDesigualdad/10^4
#mostramos la estimación del p-valor
estimacionP_valor
#Parte C)
#iguala el extremo de la regi?n cr?tica al valor del estad?stico y despejamos alfa
#el desarrollo se encuentra explicado en el documento final

valorAbs = abs(promedioX - mu0)
raiz = sqrt(50)
sigma = 4
#esta ecuación corresponde con el desarrollo planteado en el documento
pValor = (1-(pnorm(((valorAbs*raiz))/sigma)))*2
#mostramos el p-valor obtenido
pValor
t.test(muestra, mu = mu0, alternative = "two.sided")
