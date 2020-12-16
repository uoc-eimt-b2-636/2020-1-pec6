###Ejecutar online en:
# https://www.tutorialspoint.com/execute_r_online.php
# o
# http://rextester.com/l/r_online_compiler

###K-means
#Ejemplo del material sobre aprendizaje no supervisado del tercer módulo de la PEC6.

#Guarda la configuración original de los gráficos.
default <- par()

#Define la semilla de pseudoaleatoriedad para reproducibilidad.
set.seed(10)

#Genera una matrix de 2x100 valores, siguiendo una distribución normal con media 0 (valor por defecto) y desviación estándar 1 (valor por defecto).
x=matrix(rnorm(100*2),100,2)

#Genera una matrix de 2x4 valores, siguiendo una distribución normal con media 0 (valor por defecto) y desviación estándar 4.
xmean=matrix(rnorm(8,sd=4),4,2)

#Genera un vector de 100 valores enteros aleatorios entre 1 y 4.
which=sample(1:4,100,replace=TRUE)

#Genera el conjunto final de valores.
x=x+xmean[which,]

#Genera un vector de 100 valores enteros aleatorios entre 1 y 4, para la asignación inicial de grupos.
col_ini=sample(1:4,100,replace=TRUE)

#Permite visualizar 3 gráficos de forma simultanea.
par(mfrow=c(1,3))

#Visualiza los datos y la asignación inicial alteatoria.
plot(x,ylab="",xlab="Asignaci\u{F3}n aleatoria",col=col_ini,pch=19)

#Calcula los centroides de las muestras sin agrupar, y los visualiza.
centroids_init=matrix(NA,4,2)
for(i in 1:4){
  centroids_init[i,] <- colMeans(x[col_ini==i,])
}
points(centroids_init,col=c(1,2,3,4),cex=2,pch=10,lwd=2)

#Realiza una agrupación en 4 conuntos con el algoritmo K-means, usando una iteración.
km.out=kmeans(x,4,iter.max=1)

#Visualiza la agrupación y los nuevos centroides.
plot(x,ylab="",xlab="K-means Iteraci\u{F3}n 1",col=km.out$cluster,pch=19)
points(km.out$centers,col=c(1,2,3,4),cex=2,pch=10,lwd=2)


#Realiza una agrupación en 4 conuntos con el algoritmo K-means, usando dos iteraciones.
km.out=kmeans(x,4,iter.max=2)

#Se renombran los grupos para facilitar la visualización.
km.out$cluster[km.out$cluster==3]<-5
km.out$cluster[km.out$cluster==1]<-3
km.out$cluster[km.out$cluster==5]<-1

#Visualiza la agrupación y los nuevos centroides.
plot(x,ylab="",xlab="K-means Iteraci\u{F3}n 2",col=km.out$cluster,pch=19)
points(km.out$centers,col=c(3,2,1,4),cex=2,pch=10,lwd=2)

#Visualiza los datos de la clasificación.
km.out

#Recupera los parámetros gráficos originales.
par(default)
