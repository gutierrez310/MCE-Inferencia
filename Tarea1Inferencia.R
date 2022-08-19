# --Inferencia Estadistica--

# Problema 4
"a) Escriba un programa en R que reproduzca las gracas de las funciones de distribucion
acumulada y de masa de la distribucion uniforme que aparecen en las notas del curso.
Las gracas deben verse similares a las guras de la Figura 1."
n <-10
{
x <- rep(1/n,n)
plot(x,xlab="x",ylab="f(x)",ylim=c(0,5/n), main="PMF Uniforme Discreta")
cdf <- ecdf(0:n)
plot(cdf,ylab="F(x)",xlab="x", main="CDF Uniforme Discreta")
}
"b) Lea en la documentacion de R, o en cualquier otra fuente de informacion conable,
la explicacion de la funcion sample(x, size, replace=FALSE, prob=NULL). (No es necesario
entregar algo para este ejercicio)."

"c) Usando la funcion sample simule una muestra de tama~no 10 000 de la distribucion
U(1; : : : ; 10). Fijando la semilla en 13 (set.seed(13)), muestre los resultados de
la simulacion en una tabla de frecuencia y calcule la media y la varianza. Sugerencia:
Use la funcion table."
{
set.seed(13)
x = sample(1:n, 10000, replace = T)
y = table(x) #tabla de frecuencias
sumX  <- 0
for(index in 1:length(y)){
  sumX = sumX + y[index]*index
}
print("Media por promedio: ")
print(sumX/10000)
print("Media por funcion mean(): ")
print(mean(x))
print("Media por probabilidad: ")
print((n+1)/2)
}
"d) Graque las frecuencias de la simulacion anterior."
{
plot(y,type="p",main="Tabla de frecuencias")
for(  i in -1:n+1){
  abline(h = 0, v = i, col = "gray60")
}
}

"Problema 5. Para el siguiente ejercicio tambien necesitamos R."
"a) Usando la funcion sample, simule 10 lanzamientos de una moneda equilibrada y cuente 
el numero de aguilas que obtiene. Repita este proceso 104 veces y muestre sus primeros
3 resultados. Graque las frecuencias del numero de aguilas obtenidas en los 104 experiementos.
Tambien graque las proporciones del numero de aguilas obtenidas"
n <- 10000
{
freq <- rep(0,11)
for(index in 1:3){
  x <- sample(c(0,1), 10, replace = T)
  y <- factor(x,c("0","1"),labels=c("Sol","Aguila"))
  a <- sum(x)
  plot(y, main =paste("T. Frecuencias #", index))
  freq[a+1]=freq[a+1]+1
}
for(index in 4:n){
  x <- sample(c(0,1), 10, replace = T)
  a <- sum(x)
  freq[a+1]=freq[a+1]+1
}
plot(y=freq,x=0:10,ylim=c(0,2500),xlab="", ylab="Frecuencia", main="Frecuencia de Numero de Aguilas Obtenidas",type="p",pch=1)
plot(y=freq/10000,x=0:10,ylim=c(0,2500/10000),xlab="", ylab="Frecuencia", main="Proporcion de Numero de Aguilas Obtenidas",pch=3)
}

"b) Usando la funcion dbinom graque la funcion de masa de una distribucion B(10; 0:5)
sobre la graca de las proporciones que hizo en el inciso anterior"
n <- 10
{
k <- seq(0, n, by = 1)
points(k, dbinom(k, n, 0.5),pch=1,col="red")
legend("topright", y.leg[i], c("dbinom","'sample'"), pch = c(1,3), col = c(2, 1),cex=.7)
}
"c) Repita los dos incisos anteriores para una moneda desequilibrada que tiene probabilidad
p = 0:3 de obtener un aguila cuando se lanza. >Que observa?"
{
n <- 10000
freq <- rep(0,11)
for(index in 1:3){
  x <- sample(c(0,1),prob=c(.7,.3), 10, replace = T)
  y <- factor(x,c("0","1"),labels=c("Sol","Aguila"))
  a <- sum(x)
  plot(y, main =paste("T. Frecuencias #", index))
  freq[a+1]=freq[a+1]+1
}
for(index in 4:n){
  x <- sample(c(0,1),prob=c(.7,.3), 10, replace = T)
  a <- sum(x)
  freq[a+1]=freq[a+1]+1
}
plot(y=freq,x=0:10,ylim=c(0,3000),xlab="", ylab="Frecuencia", main="Frecuencia de Numero de Aguilas Obtenidas",type="p",pch=1)
plot(y=freq/10000,x=0:10,xlab="",ylim=c(0,.3), ylab="Frecuencia", main="Numero de aguilas obtenidas",type="p",pch=3)
n <- 10
k <- seq(0, n, by = 1)
points(k, dbinom(k, n, 0.3),pch=1,col="red")
legend("topright", y.leg[i], c("dbinom","'sample'"), pch = c(1,3), col = c(2, 1),cex=.7)
}

# Problema 6
"Una urna contiene 46 bolas grises y 49 bolas blancas. Usando la funcion sample en R, simule
la extraccion sin reemplazamiento de 20 de estas bolas y cuente el numero de bolas grises que
obtuvo. Repita este proceso 104 veces y graque las frecuencias de bolas grises obtenidas en
cada experimento. >Cual es la probabilidad de que al extraer 20 bolas de la urna 5 de ellas
sean grises? Tambien graque la proporcion de bolas grises obtenidas en los experiementos
anteriores y sobre esta gura a~nada la correspondiente funcion de masa de la distristibucion
Hipergeometrica asociada al experimento total."
{
K=46
n=20
N=K+49
freq <- rep(0,n+1)
y = dhyper(0:20,46,49,20)
for(index in 1:10000){
  x <- sample(c(rep(0,N-K),rep(1,K)),n,replace=F)
  a <- sum(x)
  freq[a+1]=freq[a+1]+1
}
max(freq)
plot(freq,x=0:n,ylim=c(0,2000),xlab="Bolas Grises",ylab="Frequencia",main="Frecuencia de Bolas Grises Obtenidas",pch=5)
}

"Cual es la probabilidad de que al extraer 20 bolas de la urna 5 de ellas
sean grises?"
{
y = dhyper(0:n,K,N-K,n)
print(paste("Por probabilidad: ",y[5+1]))
}

"Gráfica de la proporción de bolas grises obtenidas en los experimentos anteriores y la
correspondiente función de masa de la distribución Hipergeométrica asociada"
{
plot(freq/10000,x=0:n,ylim=c(0,.2),xlab="Bolas Grises",ylab="Frequencia",main="Proporcion de bolas grises obtenidas",pch=3)
points(y,x=0:n,pch=1,col="red")
legend("topright", y.leg[i], c("dhyper","'sample'"), pch = c(1,3), col = c(2, 1),cex=.7)
}






# 1
