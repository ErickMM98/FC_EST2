#Cargamos las librerias

library(stats)
library(MASS)
library(lmtest)
library(car)
library(zoo)
library(psych) 
library(nortest)

#Cargamos los datos

setwd("/Users/jose/Desktop/Tarea 5")
getwd()

Datos <- read.csv("plomo.csv",header=TRUE,sep=",",dec=".")
attach(Datos)

# --------------- Algunas Funciones -----------------------
Prueba_B0 <- function(X,Y){
  n     <- dim(Datos)[1]
  Sxx   <- sum((X-(mean(X)))^2)
  Sxy   <- sum((X-mean(X))*(Y-mean(Y)))
  Syy   <- sum((Y-(mean(Y)))^2)
  
  #Los estimadores son:
  beta1 <- Sxy/Sxx
  beta0 <- mean(Y)-(beta1*(mean(X)))
  SSe   <- Syy-beta1*Sxy
  MSe   <- (SSe)/(n-2)
  sigma.gorro <- MSe
  
  #La estadística de prueba para B0 es:
  Q     <- abs(beta0/(sqrt((sigma.gorro)*((1/n) + (((mean(X))^2)/Sxx)))))
  
  #Cuantil de una T-Student: 
  T     <- qt(1-(0.05/2),n-2)
  
  #Region Critica={|Q|>t(n-2,1-alpha/2)} donde t es el cuantil de una t-student
  aux     <- pt(Q,n-2)
  p.value <- 2*min(1-aux, aux) #p.value<alpha=0.05
  cat("\n\tPRUEBA DE HIPOTESIS H0: B0=0 vs H1:B0=!0\n",
      "\nRegión Crítica: Rechazar H0 si |Q|>t(n-2,1-alpha/2)","\nEstadistica de prueba Q:\n",Q,
      "\nCuantil de la T-Student:\n",T,
      "\nP-value:\n", p.value)
}
Prueba_B1 <- function(X,Y){
  n     <- dim(Datos)[1]
  Sxx   <- sum((X-(mean(X)))^2)
  Sxy   <- sum((X-mean(X))*(Y-mean(Y)))
  Syy   <- sum((Y-(mean(Y)))^2)
  
  
  #Los estimadores son:
  beta1 <- Sxy/Sxx
  beta0 <- mean(Y)-(beta1*(mean(X)))
  SSe   <- Syy-beta1*Sxy
  MSe   <- (SSe)/(n-2)
  sigma.gorro <- MSe
  
  #La estadística de prueba para B0 es:
  Q     <- abs(beta1/(sqrt((sigma.gorro/Sxx))))
  
  #Cuantil de una T-Student: 
  T     <- qt(1-(0.05/2),n-2)
  
  #Region Critica={|Z|>t(n-2,1-alpha/2)} donde t es el cuantil de una t-student
  aux     <- pt(Q,n-2)
  p.value <-2*min(1-aux, aux) #p.value<alpha=0.05
  cat("\n\tPRUEBA DE HIPOTESIS H0: B1=0 vs H1:B1=!0\n",
      "\nRegion Critica: Rechazar H0 si |Q|>t(n-2,1-alpha/2)","\nEstadistica de prueba Q:\n",Q,
      "\nCuantil de la T-Student:\n",T,
      "\nP-value:\n", p.value)
}

# Hacemos un pequeño análisis preliminar para ver que los
# datos no presentan ningún problema

View(Datos)       # Mostramos la tabla de los datos
is.na(Datos)      # Revisamos que no haya datos NA
str(Datos)        # Revisamos la estructura de la tabla
summary(Datos)    # Hacemos un resumen de la tabla


# --------------------------------------------------------------------------------------------------
# ------------------------------------ CUARTO AJUSTE: sqrt(IQ) -------------------------------------
# --------------------------------------------------------------------------------------------------
# ------------------------------------ 1) Análisis Descriptivo ------------------------------------

# Definimos la transformacion
X3 <- Plomo
Y3 <- sqrt(IQ)
Datos3 = data.frame("X3" = X3, "Y3" = Y3)

# Observamos como se comportan los datos del sqrt(IQ)
summary(Y3)
hist(Y3, freq = FALSE,breaks=15, col = "blue", main="Histograma de la variable sqrt(IQ)")
lines(density(Y3), col="red",lwd=3)

#Ahora observamos la asociación entre nuestros variables 
#mediante scaterplots.
plot(X3, Y3,pch=19, col="red", ylab="sqrt(IQ)", xlab = "Plomo en cuerpo", lwd=2,
     main="Relación entre el plomo y el sqrt(IQ)")
pairs(Datos3, col=c("red","blue"), pch=20, lwd=3,
      main="Relación etre el el plomo y el sqrt(IQ)")

#Prueba del coeficienciente de correlacion
cor.test(X3, Y3)

#------------------------------------- 2) Ajuste Del Modelo ----------------------------------------

modelo3 <- lm(Y3 ~ X3)
summary(modelo3)

plot(X3,Y3, main="Relación etre el entre el plomo y sqrt(IQ)")
abline(modelo3, col="red", lwd=2)
points(X3,modelo3$fit, col="blue",cex=1,pch=19)

#------------------------------------- 3) Inferencia Sobre el Modelo -------------------------------

#--------------- Pruebas de Hipotesis

# Checamos primero los valores del p-value en el summary
summary(modelo3)
# Para corroborar usamos nuestras funciones para la peuba estadística 
Prueba_B0(X3,Y3)
Prueba_B1(X3,Y3)

#--------------- Intervalos de Confianza

# Usamos confint para obtener la ordenada al origen y pendiente
# de nuestros intervalos de confianza
confint(modelo3)

# Hacemos las rectas de intervalos de confianza y predicción
X3.2 = data.frame(X3=X3[order(X3)])
conf1 <- predict(modelo3, newdata=X3.2, interval="confidence")
pred1 <- predict(modelo3, newdata=X3.2, interval="prediction")
plot(X3,Y3, xlab="Plomo",ylab="sqrt(IQ)", main="Relación etre el entre el plomo y el sqrt(IQ)", cex=0.5)
lines(X3.2$X3,conf1[,1], lwd=2, col="red")
lines(X3.2$X3,conf1[,2], lwd=2,lty=2, col="blue")
lines(X3.2$X3,conf1[,3], lwd=2,lty=2, col="blue")
lines(X3.2$X3,pred1[,2], lwd=2,lty=3, col="magenta")
lines(X3.2$X3,pred1[,3], lwd=2,lty=3, col="magenta")
#------------------------------------- 4) ANOVA ----------------------------------------------------

anova(modelo3)

#------------------------------------- 5) y 6) Predicción ------------------------------------------

X3.0 <-data.frame(X3 = 25)

conf1 <- predict(modelo3, newdata=X3.0, interval="confidence")
pred1 <- predict(modelo3, newdata=X3.0, interval="prediction")
plot(X3,Y3, xlab="Plomo",ylab="sqrt(IQ)", main="Relación etre el entre el plomo y el sqrt(IQ)", cex=0.5)
segments(X3.0$X3,y0 = pred1[2],y1 = pred1[3],lwd=1.5)
segments(X3.0$X3,y0 = conf1[2],y1 = conf1[3],lwd=2, col = "magenta")
points(X3.0,conf1[1], col = "blue", cex=0.5, lty = 3)

#------------------------------------- 7) Coeficiente de Determinación -----------------------------

summary(modelo3)

#------------------------------------- 8) Pruebas de los Supuestos ---------------------------------

# 1. VARIANZA CONSTANTE:
# graficamos los valores ajustados contra los residuales:

residuos3 <- rstandard(modelo3)
ajustados3 <- fitted(modelo3)
plot(ajustados3, residuos3,
     main="Residuos vs Yi ajustadas", ylim = c(-3,3))
abline(0,0, col="blue1", lw=2, lty=2)
abline(2,0, col="red", lw=2, lty=2)
abline(-2,0, col="red", lw=2, lty=2)
# Estamos graficando los residuales estandarizados y todos deberán estar en -2<=di<=2

# Hacemos las pruebas de hipotesis para homocedasticidad:
# Recordemos que:
# Ho:Los residuales tienen varianza cosntante VS H1:No tienen varanza constante
# Rechazamos al nivel de significancia alpha=0.05 si p-value<alpha

# Test de Breusch-Pagan 
bptest(modelo3)

# Prueba de puntaje de varianza no constante
ncvTest(modelo3)

# 2. SUPUESTO DE CORRELACION:
# Recordemos que:
# Ho: No hay correlación entre los residuos VS H1: Si hay correlación entre los residuos
# Rechazamos al nivel de significancia alpha=0.05 si p-value<alpha

# Prueba de Durbin-Watson
dwtest(modelo3)


# 3. SUPUESTO DE NORMALIDAD: 
# el primer acercamiento es mediante un Q-QPlot de los residuos
qqnorm(residuos1, pch=19,lwd=1)
qqline(residuos1, col="red", lwd=3)
hist(residuos, col="navyblue",freq = F, breaks = 15, main="Histograma de los residuos")

# Hacemos las pruebas de hipotesis para normalidad:
# Recordemos que:
#H0:Los errores se distribuyen N(0,1)
#H1:Los errores no se distribuyen N(0,1)
# Rechazamos al nivel de significancia alpha=0.05 si p-value<alpha

# Prueba Anderson-Darling
ad.test(residuos1)

# Prueba Shapiro-Wilk
shapiro.test(residuos1)



