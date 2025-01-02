# NOMBRE: BRIAN SENA SIMONS
# startregion Imports
# We load the libraries we might need here
require(dplyr)
require(tidyverse)
# endregion Imports

# startregion config
# We get our default working directory
getwd()
# We set our new working directory to be able to read files correctly
setwd("Master/CienciaDatos/Regresion/Laboratorios/Lab1")
# endregion config

# startregion input
read_dat_file <- function(x) { 
  # Read all lines from the .dat file
  lines <- readLines(x)
  # Extract column names from the lines starting with '@attribute'
  attr_lines <- grep("@attribute", lines)
  column_names <- gsub("@attribute\\s+(\\w+).*", "\\1", lines[attr_lines])
  # Find where the data starts
  data_start <- grep("@data", lines) + 1
  # Read the data using extracted column names
  read.table(
    text = lines[data_start:length(lines)],
    header = FALSE,
    sep = ",",
    col.names = column_names
  )
}

set_predictors_name <- function(x) {
  #Asignación automática, facilita el acceso a los campos 
  n <- length(names(x)) - 1 
  names(x)[1:n] <- paste ("X", 1:n, sep="") 
  names(x)[n+1] <- "Y"  
  x
}

California <- read_dat_file("california.dat")
# endregion input

# startregion pre_analysis
# View the first few rows of the data
head(California)
attach(California)

temp <- California 
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""), ylab=names(temp)[y])
}
par(mfrow=c(3,4)) #Si margin too large => (2,3)
x <- sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
# Al visualizar los plots, observamos una posible relación lineal del predictor X8
# Vamos a intentar visualizar mejor esas variables
par(mfrow=c(3,3)) #Si margin too large => (2,3)
x <- sapply(c(4, 5, 6, 7, 8), plotY, dim(temp)[2])
par(mfrow=c(1,1))
# endregion pre_analysis

# startregion fit_models
California <- set_predictors_name(California)
fit1=lm(Y~X8, data=California)
fit1

fit2=lm(Y~X4, data=California)
fit2

summary(fit1)
par(mfrow=c(2,1))
plot(Y~X8, California)
abline(fit1,col="red")
confint(fit1)

summary(fit2)
plot(Y~X4, California)
abline(fit2,col="blue")
par(mfrow=c(1,1))
confint(fit2)

sqrt(sum(fit1$residuals^2)/(length(fit1$residuals))) #MSE
sqrt(sum(fit1$residuals^2)/(length(fit1$residuals)-2)) #Error estandard de los residuos

predict(fit1, data.frame(X8=c(5,10,15)))

yprime=predict(fit1,data.frame(X8=California$X8)) #Valores estimados para todos los datos
#o directamente #yprime=predict(fit1,Boston)
sqrt(sum(abs(California$Y-yprime)^2)/length(yprime)) #MSE

temp <- California
plot(temp[,-dim(temp)[2]],pch=16,col=gray(1-(temp[,dim(temp)[2]]/max(temp[,dim(temp)[2]]))))

# startregion simple_linear_models
get_linear_models <- function(x, data) { 
  lm(paste("Y ~", x, sep =" "), data = data)
}

fits <- lapply(paste("X", 1:(dim(California)[2]-1), sep=""), get_linear_models, California)

summarise_linear_models <- function(x) {
  summary(x)
}

fits_summary <- lapply(fits, summarise_linear_models)
fits_summary
# Como esperado, vemos que apenas X8 parece tener un comportamiento lineal con la salida
# endregion simple_linear_models

# Podemos también mirar el comportamiento de los residuos
par(mfrow=c(1,1)) 
plot(yprime, fit1$residuals)
# Vemos un posible patrón en los residuos que podrían indicar no-linealidad
fit1_nll <- lm(Y ~ X8 + log(X8), data = California)
summary(fit1_nll)
fit1_nls <- lm(Y ~ X8 + I(X8^2), data = California)
summary(fit1_nls)
fit1_nlr <- lm(Y ~ X8 + sqrt(X8), data = California)
summary(fit1_nlr)
# Vemos que aplicar el cuadrado del valor parece ayudar
fit1_nlp <- lm(Y ~ X8 + I(X8^2) + I(X8^3), data = California)
summary(fit1_nlp)
fit1_nlp <- lm(Y ~ X8 + I(X8^2) + I(X8^3) + I(X8^4), data = California)
summary(fit1_nlp)
# Según los p-value el polinomio de grado 4 ya deja de aportar información. 

# startregion multiple_regression
temp <- California
par(mfrow=c(3,3))
plot(temp[,-dim(temp)[2]],pch=16,col=gray(1-(temp[,dim(temp)[2]]/max(temp[,dim(temp)[2]]))))
par(mfrow=c(1,1))
# Forward selection
fit3 <- lm(Y~X8+X4, data=California)
summary(fit3)
# Vemos que el modelo ha mejorado, no obstante X4 no aporta información (high p-value)

fit4 <- lm(Y~X8+X5,data=California)
summary(fit4)
# Hemos mejorado el modelo y el p-value indica que X5 aporta información

# Hacemos lo mismo para las combinación aditiva de pares 
xnames <- paste("X", 1:(dim(California)[2]-2), sep="")
pairnames <- paste("X8 +", xnames)
pairnames
fits <- lapply(pairnames, get_linear_models, California)
names(fits) <- xnames
sum_fits <- lapply(fits, summarise_linear_models)
sum_fits
attributes(fits[[1]])
fits[[1]]$coefficients
fits_rss <- lapply(fits, function(x) {sum(x$residuals^2)})
fits_rss[order(unlist(fits_rss))]
# Según vemos en este pequeño análisis, X3 tiene el menor RSS y mejora R^2 a 0.5
# Hacemos lo mismo ahora pero desde el modelo base X8 + X3
pairnames <- paste("X8 + X3 +", xnames)
pairnames
fit_sum_and_sort <- function(pairnames, data) {
  fits <- lapply(pairnames, get_linear_models, data)
  names(fits) <- pairnames
  sum_fits <- lapply(fits, summarise_linear_models)
  fits_rss <- lapply(fits, function(x) {sum(x$residuals^2)})
  list(fits, sum_fits, fits_rss[order(unlist(fits_rss))])
}
comp_fits <- fit_sum_and_sort(pairnames, California)
comp_fits
# Analizando los resultados vemos que X8+X3+X5 ~ min(RSS) + R^2=0.524  + F-statistic  >> 1
predictors <- "X8+X3+X5+"
possible <- xnames[!xnames %in% str_split(predictors, pattern="\\+", simplify = TRUE)[1, ]]
pairnames <- paste(predictors, possible, sep="")
pairnames
comp_fits <- fit_sum_and_sort(pairnames, California)
comp_fits
# Vemos que X8 + X3 + X5 + X6 ~ min(RSS) + R^2 = 0.5485 + F-statistic >> 1
predictors <-"X8+X3+X5+X6+"
possible <- xnames[!xnames %in% str_split(predictors, pattern="\\+", simplify = TRUE)[1, ]]
pairnames <- paste(predictors, possible, sep="")
pairnames
comp_fits <- fit_sum_and_sort(pairnames, California)
comp_fits
# Vemos que X8 + X3 + X5 + X6 + X4 ~ min(RSS) + R^2 = 0.5615 + F-statistic >> 1
predictors <-"X8+X3+X5+X6+X4+"
possible <- xnames[!xnames %in% str_split(predictors, pattern="\\+", simplify = TRUE)[1, ]]
pairnames <- paste(predictors, possible, sep="")
pairnames
comp_fits <- fit_sum_and_sort(pairnames, California)
comp_fits
# Vemos que X8 + X3 + X5 + X6 + X4 + X2 ~ min(RSS) + R^2 = 0.5679 + F-statistic >> 1
predictors <-"X8+X3+X5+X6+X4+X2+"
possible <- xnames[!xnames %in% str_split(predictors, pattern="\\+", simplify = TRUE)[1, ]]
pairnames <- paste(predictors, possible, sep="")
pairnames
comp_fits <- fit_sum_and_sort(pairnames, California)
comp_fits
# Vemos que X8 + X3 + X5 + X6 + X4 + X2 + X1 ~ min(RSS) + R^2 = 0.6364 + F-statistic >> 1
predictors <-"X8+X3+X5+X6+X4+X2+X1+"
possible <- xnames[!xnames %in% str_split(predictors, pattern="\\+", simplify = TRUE)[1, ]]
pairnames <- paste(predictors, possible, sep="")
pairnames
comp_fits <- fit_sum_and_sort(pairnames, California)
comp_fits
# Vemos que añadir X7 solamente nos incremente a 0.6371
# Pero el resultado de forward selection es que todas las variables aportan información en conjunto.

# Backward selection:
fit5 <- lm(Y~., data=California) #TODAS para descendente
summary(fit5)
# Según el backward selection not parece que podamos ir recortando variables del mismo.
fit6 <- lm(Y~.-X7, data = California)
summary(fit6)

# startregion interactions
fit8 <- lm(Y~X8*X3, California)
summary(fit8)
plot(Y~X8)
points(X8,fitted(fit8),col="green",pch=20)

fitbien1 <- lm(Y~.+X8*X3, data = California) 
summary(fitbien1)

fit9 <- lm(Y~X8 + I(X8^2), California)
summary(fit9)
plot(Y~X8)
points(X8,fitted(fit9),col="red",pch=20)

fit10 <- lm(Y~X8 + I(X8^2) + I(X8^3), California)
summary(fit10)
plot(Y~X8)
points(X8,fitted(fit10),col="red",pch=20)

fitbien2 <- lm(Y~.+X8*X3+I(X8^2)+I(X8^3), California) 
# This model achieves r = 0.6563
summary(fitbien2)

fitprueba <- lm(Y~.+X8*X3+I(X8^2)+I(X8^3)+I(X8^2*X3), California)
# This model achieves r = 0.6601
summary(fitprueba)
attach(California)
plot(Y~X8)
points(X8, fitted(fitprueba), col="red", pch=20)
# Probamos también añadir no-linealidad a la segunda variable que más información
# aporta según los estudios preliminares
fitprueba <- lm(Y~.+X8*X3+I(X8^2)+I(X8^3)+I(X8^2*X3)+I(X3^2), California)
summary(fitprueba)
# Obtenemos el mayor r hasta el momento de 0.662
# endregion interactions

yprime=predict(fitbien2, California)
sqrt(sum(abs(California$Y-yprime)^2)/length(yprime))
yprime=predict(fitprueba,California)
sqrt(sum(abs(California$Y-yprime)^2)/length(yprime))
# Obtenemos mejor MSE con el modelo final con iteracciones y no-linealidades. 

# También comparamos los modelos presentados en la diapositiva del laboratorio 2
fitDiap1 <- lm(Y~X8+I(X8^2)+I(X8^3)+I(log(X3))+I(log(X4/X6)) +I(log(X5/X6))+I(log(X6/X7))+I(log(X7)), California) 
summary(fitDiap1)
fitDiap3=lm(Y~.+X4*X7*X8, California) 
summary(fitDiap3)
fitDiap4=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4)+X7*X8*X4*X5*X6, California) 
summary(fitDiap4)
# Vemos que hemos obtenido el mejor modelo hasta el momento por la iteracción X7*X8*X4*X5*X6
# Podemos seguir probando añadir iteracciones y otras operaciones.
# No obstante, también vemos que F-statistic empieza a bajar y acercarse a 1. 
# Significando que pueda ser probable que algún coeficiente llegara a no aportar.
fitprueba2=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4)+X8*X7*X6*X5*X4*X3, California) 
summary(fitprueba2)
# Ahora hemos obtenido un R~0.7076.
# Estas iteracciones vienen de haber estudiado la relación lineal entre las variables.
# Aunque en algunos p-valores de estos modelos más complejos son altos; como forman
# parte de los "main-effects" por el principio de jerarquía no podemos eliminarlos.
par(mfrow=c(1,1))
plot(Y~X8)
points(X8,fitted(fitprueba2),col="red",pch=20)
# Si también incluímos la relación entre X1 y X2 + iteracción con X8 podemos obtener r = 0.7098
fitprueba2=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4)+X8*X7*X6*X5*X4*X3+X2*X1*X8, California) 
summary(fitprueba2)
