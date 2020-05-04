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
# ----------------------------------------- AJUSTE INICIAL -----------------------------------------
# --------------------------------------------------------------------------------------------------
#------------------------------------- 1) Análisis Descriptivo ------------------------------------

# Observamos como se comportan los datos del Plomo
summary(Plomo)
hist(Plomo, freq = FALSE,breaks=15, col = "blue", main="Histograma de la variable Plomo")
lines(density(Plomo), col="red",lwd=3)

# Observamos como se comportan los datos del IQ
summary(IQ)
hist(IQ, freq = FALSE,breaks=15, col = "blue", main="Histograma de la variable IQ")
lines(density(IQ), col="red",lwd=3)

#Ahora observamos la asociación entre nuestros variables 
#mediante scaterplots.
plot(Plomo, IQ,pch=19, col="red", ylab="IQ", xlab = "Plomo en cuerpo", lwd=2,
     main="Relación entre el plomo y el IQ")
pairs(Datos, col=c("red","blue"), pch=20, lwd=3,
      main="Relación etre el el plomo y el IQ")

#Prueba del coeficienciente de correlacion
cor.test(Plomo, IQ)

#------------------------------------- 2) Ajuste Del Modelo ----------------------------------------

modelo <- lm(IQ ~ Plomo)
summary(modelo)

plot(Plomo,IQ, main="Relación etre el entre el plomo y el IQ")
abline(modelo, col="red", lwd=2)
points(Plomo,modelo$fit, col="blue", cex=1,pch=19)

#------------------------------------- 3) Inferencia Sobre el Modelo -------------------------------

#--------------- Pruebas de Hipotesis

# Checamos primero los valores del p-value en el summary
summary(modelo)

# Para corroborar usamos nuestras funciones para la peuba estadística 
Prueba_B0(Plomo,IQ)
Prueba_B1(Plomo,IQ)

#--------------- Intervalos de Confianza

# Usamos confint para obtener la ordenada al origen y pendiente
# de nuestros intervalos de confianza
confint(modelo)

# Hacemos las rectas de intervalos de confianza y predicción
Plomo2 = data.frame(Plomo=Plomo[order(Plomo)])
conf <- predict(modelo, newdata=Plomo2, interval="confidence")
pred <- predict(modelo, newdata=Plomo2, interval="prediction")
plot(Plomo,IQ, xlab="Plomo",ylab="IQ", main="Relación etre el entre el plomo y el IQ", cex=0.5)
lines(Plomo2$Plomo,conf[,1], lwd=2, col="red")
lines(Plomo2$Plomo,conf[,2], lwd=2,lty=2, col="blue")
lines(Plomo2$Plomo,conf[,3], lwd=2,lty=2, col="blue")
lines(Plomo2$Plomo,pred[,2], lwd=2,lty=3, col="magenta")
lines(Plomo2$Plomo,pred[,3], lwd=2,lty=3, col="magenta")

#------------------------------------- 4) ANOVA ----------------------------------------------------

anova(modelo)

#------------------------------------- 5) y 6) Predicción ------------------------------------------

X0 <-data.frame(Plomo = 10)

conf <- predict(modelo, newdata=X0, interval="confidence")
pred <- predict(modelo, newdata=X0, interval="prediction")
plot(Plomo,IQ, xlab="Plomo",ylab="IQ", main="Relación etre el entre el plomo y el IQ", cex=0.5)
segments(X0$Plomo,y0 = pred[2],y1 = pred[3],lwd=1.5)
segments(X0$Plomo,y0 = conf[2],y1 = conf[3],lwd=2, col = "magenta")
points(X0,conf[1], col = "blue", cex=0.5, lty = 3)

#------------------------------------- 7) Coeficiente de Determinación -----------------------------

summary(modelo)

#------------------------------------- 8) Pruebas de los Supuestos ---------------------------------

# 1. VARIANZA CONSTANTE:
# graficamos los valores ajustados contra los residuales:

residuos <- rstandard(modelo)
ajustados <- fitted(modelo)
plot(ajustados, residuos,
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
bptest(modelo)

# Prueba de puntaje de varianza no constante
ncvTest(modelo)

# 2. SUPUESTO DE CORRELACION:
# Recordemos que:
# Ho: No hay correlación entre los residuos VS H1: Si hay correlación entre los residuos
# Rechazamos al nivel de significancia alpha=0.05 si p-value<alpha

# Prueba de Durbin-Watson
dwtest(modelo)


# 3. SUPUESTO DE NORMALIDAD: 
# el primer acercamiento es mediante un Q-QPlot de los residuos
qqnorm(residuos, pch=19,lwd=1)
qqline(residuos, col="red", lwd=3)
hist(residuos, col="navyblue",freq = F, breaks = 15, main="Histograma de los residuos")

# Hacemos las pruebas de hipotesis para normalidad:
# Recordemos que:
#H0:Los errores se distribuyen N(0,1)
#H1:Los errores no se distribuyen N(0,1)
# Rechazamos al nivel de significancia alpha=0.05 si p-value<alpha

# Prueba Anderson-Darling
ad.test(residuos)

# Prueba Shapiro-Wilk
shapiro.test(residuos)





