###Ejecutar online en:
# https://www.tutorialspoint.com/execute_r_online.php
# o
# http://rextester.com/l/r_online_compiler

###Modelo de Regresión Lineal Simple
#Ejemplo del material sobre aprendizaje supervisado del tercer módulo de la PEC6.

#Carga una librería de datasets includia en R. Información del conjunto de datasets en https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html
library(datasets)

#Usaremos el dataset 'mtcars', se puede obtener más información con el comando '?mtcars'.
attach(mtcars)

#Genera un modelo lineal del consumo del motor de un coche en relación a su potencia.
lm.fit=lm(mpg~hp,data=mtcars)

#Muestra información sobre el modelo generado.
summary(lm.fit)

#Muestra información sobre el intérvalo de confianza del modelo generado.
confint(lm.fit)

#Predice el consumo basado en el modelo lineal generado para valores determinados de potencia del motor, junto con el intérvalo de confianza.
predict(lm.fit,data.frame(hp=(c(100,170,250))), interval="confidence")

#Genera las predicciones para todo el rango de datos.
newx <- seq(min(mtcars$hp), max(mtcars$hp), length.out=nrow(mtcars))
preds <- predict(lm.fit, data.frame(hp=newx), interval = 'confidence')

#Visualiza los datos
plot(mpg~hp,xlab="Potencia en CV",ylab="Consumo en MPG",data=mtcars)

#Visualiza el intérvalo de confianza de la predicción.
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col=rgb(0, 0, 0,0.2), border = NA)

#Visualiza el modelo lineal
abline(lm.fit,lwd=3,col="red")

#Visualiza los márgenes superior e inferior del intérvalo de confianza.
lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')


